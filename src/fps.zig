const std = @import("std");
const Timer = std.time.Timer;

/// A simple pseudo ring buffer for tracking frame rate.
/// The type args are the minimum and maximum number of frames the tracker is allowed to use to estimate FPS.
/// Not inherently thread-safe; access must be synchronized externally.
pub fn FrameTracker(comptime min_frames: u32, comptime max_frames: u32) type {
    return struct {
        const Self = @This();

        timer: ?Timer = null,
        head: usize = 0,
        size: usize = 0,
        buffer: [max_frames]u64 = [_]u64{undefined} ** max_frames,
        buffer_sorted: [max_frames]u64 = [_]u64{undefined} ** max_frames,

        /// Starts the tracker if needed, or laps and records the frame time. Must be called exactly once each frame, at the same point in the frame (e.g. right after swapping buffers).
        pub fn startOrLap(self: *Self) void {
            if (self.timer == null) {
                self.timer = Timer.start() catch null;
                return;
            }

            self.buffer[self.head] = self.timer.?.lap();
            self.head = (self.head + 1) % max_frames;
            if (self.size < max_frames) self.size += 1;

            const s = self.size;
            std.mem.copy(u64, self.buffer_sorted[0..s], self.buffer[0..s]);
            std.sort.sort(u64, self.buffer_sorted[0..s], {}, comptime std.sort.asc(u64));
        }

        /// Clear all recorded frame times from the buffer, starting fresh.
        pub fn reset(self: *Self) void {
            // No point in clearing buffers themselves.
            self.timer = null;
            self.head = 0;
            self.size = 0;
        }

        /// Get an estimate of FPS. Returns null if the tracker hasn't recorded sufficient data to estimate frame rate.
        pub fn estimateFps(self: *Self) ?f64 {
            const s = self.size;
            if (s < min_frames or s == 0) return null;

            // Median is the middle value if an odd total, or the average of the two middle values if an even total.
            var median = self.buffer_sorted[s / 2];
            if (s % 2 == 0) {
                const medianA = self.buffer_sorted[s / 2 - 1];
                median = (median + medianA) / 2;
            }

            const ticks_per_frame = @intToFloat(f64, median);
            const ticks_per_second = comptime @intToFloat(f64, std.time.ns_per_s);
            return ticks_per_second / ticks_per_frame;
        }
    };
}
