const std = @import("std");
const Allocator = std.mem.Allocator;
const os_tag: comptime std.Target.Os.Tag = std.Target.current.os.tag;

const io = @import("io.zig");
const Point = io.Point;
const VideoMode = io.VideoMode;

const glfw = @import("glfw.zig");
const MonitorHandle = glfw.MonitorHandle(Monitor);

// =====================================================================================================================
// Internal functions

pub fn getOrCreate(handle: MonitorHandle, allocator: *Allocator) !*Monitor {
    if (handle.getUserPointer()) |ptr| return ptr;

    var monitor = try allocator.create(Monitor);
    monitor.* = Monitor{
        .allocator = allocator,
        .handle = handle,
    };
    handle.setUserPointer(monitor);
    return monitor;
}

pub fn freeAll() void {
    var handles = MonitorHandle.getAll() catch return;
    while (handles.next()) |handle| {
        if (handle.getUserPointer()) |monitor| {
            monitor.destroy();
        }
    }
}

/// Get the current video mode and cache it as the DWM video mode for this monitor.
/// Should be called if running on Windows and *reasonably* certain that this monitor isn't currently fullscreen.
pub fn getCurrentModeAsDesktopMode(mon: *Monitor) !VideoMode {
    const mode = try mon.handle.getCurrentVideoMode();
    mon.detected_dwm_mode = mode;
    return mode;
}

// =====================================================================================================================
// Public struct

pub const Monitor = struct {
    allocator: *Allocator,
    handle: MonitorHandle,

    /// On Windows, this is the video mode we assume the DWM uses for composition on this monitor.
    /// We cache this to remember what the desktop mode is before setting some other mode.
    /// Our detection mechanisms are not perfect, but they work for all but a few corner cases.
    detected_dwm_mode: ?VideoMode = null,

    pub fn getPrimary(allocator: *Allocator) !*Monitor {
        const handle = try MonitorHandle.getPrimary();
        return try getOrCreate(handle, allocator);
    }

    fn destroy(self: *Monitor) void {
        self.handle.setUserPointer(null);
        self.allocator.destroy(self);
    }

    pub fn getDesktopMode(self: Monitor) !VideoMode {
        if (os_tag == .windows) {
            return self.detected_dwm_mode orelse try self.handle.getCurrentVideoMode();
        } else {
            return try self.handle.getCurrentVideoMode();
        }
    }

    pub fn getMaxMode(self: Monitor) !VideoMode {
        var iter = &(try self.handle.getAllVideoModes());
        var max_mode: ?VideoMode = null;
        while (iter.next()) |mode| {
            max_mode = mode;
        }
        return max_mode orelse error.Unknown;
    }
};
