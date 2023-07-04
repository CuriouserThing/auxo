const std = @import("std");

/// A simple pseudo ring buffer for tracking frame rate.
/// The type args are the minimum and maximum number of frames the tracker is allowed to use to estimate FPS.
/// Not thread-safe.
pub fn FrameTracker(comptime min_frames: u32, comptime max_frames: u32) type {
    if (min_frames > max_frames) @compileError("min_frames > max_frames");
    return struct {
        timer: std.time.Timer,
        started: bool = false,
        head: usize = 0,
        size: usize = 0,
        buffer: [max_frames]u64 = [_]u64{undefined} ** max_frames,
        buffer_sorted: [max_frames]u64 = [_]u64{undefined} ** max_frames,

        /// Starts the tracker if needed, or laps and records the frame time. Must be called exactly once each frame, at the same point in the frame (e.g. right after swapping buffers).
        pub fn startOrLap(self: *@This()) void {
            if (!self.started) {
                self.timer.reset();
                self.started = true;
                return;
            }

            self.buffer[self.head] = self.timer.lap();
            self.head = (self.head +% 1) % max_frames;
            if (self.size < max_frames) self.size += 1;

            const s = self.size;
            std.mem.copy(u64, self.buffer_sorted[0..s], self.buffer[0..s]);
            std.sort.pdq(u64, self.buffer_sorted[0..s], {}, comptime std.sort.asc(u64));
        }

        /// Clear all recorded frame times from the buffer, starting fresh.
        pub fn reset(self: *@This()) void {
            // No point in clearing buffers themselves.
            self.started = false;
            self.head = 0;
            self.size = 0;
        }

        /// Get an estimate of FPS. Returns null if the tracker hasn't recorded sufficient data to estimate frame rate.
        pub fn estimateFps(self: *@This()) ?f64 {
            const s = self.size;
            if (s < min_frames or s == 0) return null;

            // Median is the middle value if an odd total, or the average of the two middle values if an even total.
            var median = self.buffer_sorted[s / 2];
            if (s % 2 == 0) {
                const medianA = self.buffer_sorted[s / 2 - 1];
                median = (median + medianA) / 2;
            }

            const ticks_per_frame: f64 = @floatFromInt(median);
            const ticks_per_second: comptime_float = @floatFromInt(std.time.ns_per_s);
            return ticks_per_second / ticks_per_frame;
        }
    };
}

/// Single-writer, single-reader buffer. Requires at least one dedicated (write-free) reader thread, but the reader can also run on any writer thread.
/// Lockless when under capacity.
pub fn RingBuffer(comptime size: usize, comptime T: type) type {
    if (size < 3) @compileError("Ring buffer must hold at least 3 items.");
    return struct {
        items: [size]T = [_]T{undefined} ** size,
        head: usize = 0, // location of next write (exclusive end)
        tail: usize = 0, // location of next read (inclusive start)
        sync: usize = 0,
        read_unreleased: bool = false,
        read_completed: bool = false,
        mutex: std.Thread.Mutex = .{},
        condition: std.Thread.Condition = .{},

        pub fn write(self: *@This(), item: T) void {
            // Write item
            const head = @atomicLoad(usize, &self.head, .SeqCst);
            self.items[head] = item;

            // If buffer is now full, wait for a read under lock
            const next_head = (head +% 1) % size;
            const tail = @atomicLoad(usize, &self.tail, .SeqCst);
            if (next_head == tail) {
                self.mutex.lock();
                defer self.mutex.unlock();

                // Attempt to signal wait by advancing sync
                // If we can't, reader completed at least one read since we loaded read tail
                const next_tail = (tail +% 1) % size;
                if (@cmpxchgStrong(usize, &self.sync, tail, next_tail, .SeqCst, .SeqCst) == null) {
                    while (!self.read_completed) {
                        self.condition.wait(&self.mutex);
                    }
                    self.read_completed = false;
                }
            }

            // Advance write head
            @atomicStore(usize, &self.head, next_head, .SeqCst);
        }

        pub fn read(self: *@This()) ?T {
            const item = self.readWithoutRelease() orelse return null;
            self.releasePreviousRead();
            return item;
        }

        pub fn readWithoutRelease(self: *@This()) ?T {
            // Return null if the buffer is empty
            const tail = @atomicLoad(usize, &self.tail, .SeqCst);
            const head = @atomicLoad(usize, &self.head, .SeqCst);
            if (tail == head) return null;

            // Read item if previous item was released
            if (@cmpxchgStrong(bool, &self.read_unreleased, false, true, .SeqCst, .SeqCst) == null) {
                return self.items[tail];
            } else {
                return null;
            }
        }

        pub fn releasePreviousRead(self: *@This()) void {
            // Return if already released
            if (@cmpxchgStrong(bool, &self.read_unreleased, true, false, .SeqCst, .SeqCst) != null) return;

            // Advance read tail
            const tail = @atomicLoad(usize, &self.tail, .SeqCst);
            const next_tail = (tail +% 1) % size;
            @atomicStore(usize, &self.tail, next_tail, .SeqCst);

            // Attempt to signal read by advancing sync
            // If we can't, writer is waiting under lock, so we must complete read under lock and signal it via condition
            if (@cmpxchgStrong(usize, &self.sync, tail, next_tail, .SeqCst, .SeqCst) != null) {
                self.mutex.lock();
                defer self.mutex.unlock();

                self.read_completed = true;
                self.condition.signal();
            }
        }
    };
}
