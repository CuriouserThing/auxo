const std = @import("std");
const Allocator = std.mem.Allocator;
const Mutex = std.Thread.Mutex;
const os_tag: comptime std.Target.Os.Tag = std.Target.current.os.tag;

const io = @import("io.zig");
const Point = io.Point;
const Size = io.Size;
const Rectangle = io.Rectangle;
const KeyMods = io.KeyMods;
const MouseButtonAction = io.MouseButtonAction;
const MouseButton = io.MouseButton;
const KeyAction = io.KeyAction;
const Key = io.Key;

const glfw = @import("glfw.zig");
const WindowHandle = glfw.WindowHandle(Window);
const MonitorHandle = glfw.MonitorHandle(Monitor);

const monitors = @import("monitors.zig");
const Monitor = monitors.Monitor;

const fps = @import("fps.zig");
const FrameTracker = fps.FrameTracker(60, 240);

// =====================================================================================================================
// Internal functions

pub fn createFromState(allocator: *Allocator, creation_state: Window.CreationState, event_handler: Window.EventHandler) !*Window {
    const title = creation_state.title;
    const restored_size = creation_state.restored_size;
    switch (creation_state.mode) {
        .windowed => |mode| {
            const window = try Window.create(allocator, restored_size, restored_size, title, null, event_handler);
            if (os_tag != .linux) window.restored_pos = window.handle.getPos();
            if (mode == .maximized) window.handle.maximize();
            return window;
        },
        .fullscreen => |mode| {
            const monitor = mode.monitor orelse try Monitor.getPrimary(allocator);
            const cvm = try monitor.handle.getCurrentVideoMode();
            if (os_tag != .windows) {
                return try Window.create(allocator, restored_size, cvm.size, title, monitor, event_handler);
            } else if (mode.kind == .composited) {
                glfw.hintWindowVisible(false);
                glfw.hintWindowDecorated(false);
                const size = adjustWindowSizeForFakeFullscreen(cvm.size);
                const pos = monitor.handle.getPos();
                const window = try Window.create(allocator, restored_size, size, title, null, event_handler);
                window.fake_fullscreen_on = true;
                window.handle.setPos(pos);
                window.handle.show();
                return window;
            } else {
                const vm = if (mode.kind == .exclusive_max) try monitor.getMaxMode() else cvm;
                glfw.hintWindowRefreshRate(vm.refresh_rate);
                return try Window.create(allocator, restored_size, vm.size, title, monitor, event_handler);
            }
        },
    }
}

pub fn destroy(win: *Window) void {
    win.handle.setUserPointer(null);
    win.handle.destroy();
    win.allocator.destroy(win);
}

pub fn render(
    win: *Window,
    comptime ContextT: type,
    comptime renderFn: fn (ContextT, visible_area: Rectangle, estimated_fps: ?f64) void,
    context: ContextT,
) void {
    const held = win.render_lock.acquire();
    defer held.release();

    var size = win.framebuffer_size;
    if (os_tag == .windows and win.fake_fullscreen_on) {
        size = adjustViewportSizeForFakeFullscreen(size);
    }
    const suggested_viewport = Rectangle{
        .pos = Point{ .x = 0, .y = 0 },
        .size = size,
    };

    const estimated_fps = win.frame_tracker.estimateFps();
    renderFn(context, suggested_viewport, estimated_fps);

    if (win.restore_swap_interval) {
        if (win.swap_interval) |interval| {
            glfw.swapInterval(interval);
            win.restore_swap_interval = false;
        }
    }

    win.handle.swapBuffers();
    win.frame_tracker.startOrLap();
}

/// Hack to keep the window truly composited on Windows.
fn adjustWindowSizeForFakeFullscreen(size: Size) Size {
    return .{ .w = size.w, .h = size.h + 1 };
}

/// Adjustment to reverse the hack for rendering.
fn adjustViewportSizeForFakeFullscreen(size: Size) Size {
    return .{ .w = size.w, .h = size.h - 1 };
}

// =====================================================================================================================
// Public struct

pub const Window = struct {
    pub const CreationState = struct {
        title: [:0]const u8,
        mode: union(enum) {
            windowed: WindowedMode,
            fullscreen: FullscreenMode,
        },
        restored_size: Size,
    };

    pub const WindowedMode = enum {
        previous_mode,
        restored,
        maximized,
    };

    pub const FullscreenMode = struct {
        kind: enum {
            composited,
            exclusive_desktop,
            exclusive_max,
        },
        monitor: ?*Monitor,
    };

    pub const EventHandler = struct {
        closeRequestCallback: comptime ?fn (*Window) void = null,
        focusCallback: comptime ?fn (*Window, focused: bool) void = null,
        iconifyCallback: comptime ?fn (*Window, iconified: bool) void = null,
        keyCallback: comptime ?fn (*Window, Key, scancode: i32, KeyAction, KeyMods) void = null,
        charCallback: comptime ?fn (*Window, codepoint: u21) void = null,
        mouseButtonCallback: comptime ?fn (*Window, MouseButton, MouseButtonAction, KeyMods) void = null,
        cursorPosCallback: comptime ?fn (*Window, x: f64, y: f64) void = null,
        cursorEnterCallback: comptime ?fn (*Window, entered: bool) void = null,
        scrollCallback: comptime ?fn (*Window, x_offset: f64, y_offset: f64) void = null,
    };

    allocator: *Allocator,
    handle: WindowHandle,
    event_handler: EventHandler,
    render_lock: Mutex = .{},
    frame_tracker: FrameTracker = .{},

    // Cached state
    restored_pos: Point = .{ .x = 0, .y = 0 },
    restored_size: Size,
    was_maximized: bool = false,
    fake_fullscreen_on: bool = false,
    framebuffer_size: Size,
    swap_interval: ?i32 = null,

    restore_swap_interval: bool = false,
    should_destroy: bool = false,

    fn create(
        allocator: *Allocator,
        restored_size: Size,
        size: Size,
        title: [:0]const u8,
        monitor: ?*Monitor,
        event_handler: EventHandler,
    ) !*Window {
        const window = try allocator.create(Window);
        errdefer allocator.destroy(window);

        const monitor_handle = if (monitor) |m| m.handle else null;
        const handle = try WindowHandle.create(size, title, Monitor, monitor_handle, null);
        window.* = Window{
            .allocator = allocator,
            .handle = handle,
            .event_handler = event_handler,
            .restored_size = restored_size,
            .framebuffer_size = handle.getFramebufferSize(),
        };
        handle.setUserPointer(window);

        handle.setPosCallback(onPos);
        handle.setSizeCallback(onSize);
        handle.setFramebufferSizeCallback(onFramebufferSize);
        handle.setCloseCallback(onClose);
        handle.setFocusCallback(onFocus);
        handle.setIconifyCallback(onIconify);
        handle.setMaximizeCallback(onMaximize);
        handle.setCursorEnterCallback(onCursorEnter);
        handle.setKeyCallback(onKey);
        handle.setCharCallback(onChar);
        handle.setMouseButtonCallback(onMouseButton);
        handle.setCursorPosCallback(onCursorPos);
        handle.setScrollCallback(onScroll);

        return window;
    }

    fn onPos(self: *Window, pos: Point) void {
        if (self.shouldCacheRestoredBounds()) {
            self.restored_pos = pos;
        }
    }

    fn onSize(self: *Window, size: Size) void {
        if (size.w > 0 and size.h > 0 and self.shouldCacheRestoredBounds()) {
            self.restored_size = size;
        }
    }

    fn onFramebufferSize(self: *Window, size: Size) void {
        // Locking this callback has two benefits:
        // - We atomically load/store size.
        // - On Windows, we allow smooth resizing by never returning control to the event thread in the middle of a render. (The Win32 SIZEMOVE loop is...bizarre.)
        const held = self.render_lock.acquire();
        defer held.release();

        if (size.w > 0 and size.h > 0) {
            self.framebuffer_size = size;
        }
    }

    fn onClose(self: *Window) void {
        if (self.event_handler.closeRequestCallback) |callback| callback(self);
    }

    fn onFocus(self: *Window, focused: bool) void {
        if (self.event_handler.focusCallback) |callback| callback(self, focused);
    }

    fn onIconify(self: *Window, iconified: bool) void {
        if (self.event_handler.iconifyCallback) |callback| callback(self, iconified);
    }

    fn onMaximize(self: *Window, value: bool) void {
        if (os_tag != .windows or !self.fake_fullscreen_on) {
            self.was_maximized = value;
        }
    }

    fn onKey(self: *Window, key: Key, scancode: i32, action: KeyAction, mods: KeyMods) void {
        if (self.event_handler.keyCallback) |callback| callback(self, key, scancode, action, mods);
    }

    fn onChar(self: *Window, codepoint: u21) void {
        if (self.event_handler.charCallback) |callback| callback(self, codepoint);
    }

    fn onMouseButton(self: *Window, button: MouseButton, action: MouseButtonAction, mods: KeyMods) void {
        if (self.event_handler.mouseButtonCallback) |callback| callback(self, button, action, mods);
    }

    fn onCursorPos(self: *Window, x: f64, y: f64) void {
        if (self.event_handler.cursorPosCallback) |callback| callback(self, x, y);
    }

    fn onCursorEnter(self: *Window, entered: bool) void {
        if (self.event_handler.cursorEnterCallback) |callback| callback(self, entered);
    }

    fn onScroll(self: *Window, x: f64, y: f64) void {
        if (self.event_handler.scrollCallback) |callback| callback(self, x, y);
    }

    fn shouldCacheRestoredBounds(self: *Window) bool {
        // Don't cache if fake fullscreen
        if (os_tag == .windows and self.fake_fullscreen_on) return false;

        // Don't cache if real fullscreen
        if (self.handle.getFullscreenMonitor(Monitor)) |_| return false;

        // Don't cache if iconified
        if (self.handle.isIconified()) return false;

        // Don't cache if maximized
        if (self.handle.isMaximized()) return false;

        return true;
    }

    pub fn close(self: *Window) void {
        self.should_destroy = true;
    }

    pub fn toggleFullscreen(self: *Window, windowed_mode: WindowedMode, fullscreen_mode: FullscreenMode) !void {
        const should_maximize = self.shouldMaximizeFromMode(windowed_mode);
        if (self.handle.getFullscreenMonitor(Monitor)) |_| {
            self.turnRealFullscreenToWindowed(should_maximize);
        } else if (os_tag == .windows and self.fake_fullscreen_on) {
            self.turnFakeFullscreenToWindowed(should_maximize);
        } else {
            try self.turnCompositedToFullscreen(fullscreen_mode);
        }
    }

    pub fn turnFullscreenOn(self: *Window, mode: FullscreenMode) !void {
        if (self.handle.getFullscreenMonitor(Monitor)) |old_monitor_handle| {
            const monitor = mode.monitor orelse try monitors.getOrCreate(old_monitor_handle, self.allocator);
            const vm = if (os_tag == .windows and mode.kind == .exclusive_max) try monitor.getMaxMode() else try monitor.getDesktopMode();
            if (os_tag == .windows and mode.kind == .composited) {
                self.fake_fullscreen_on = true;
                self.handle.setDecorated(false);
                const pos = monitor.handle.getPos();
                const size = adjustWindowSizeForFakeFullscreen(vm.size);
                self.setWindowed(pos, size);
            } else {
                const rr = if (os_tag == .windows) vm.refresh_rate else null;
                self.setFullscreen(monitor, vm.size, rr);
            }
        } else {
            if (os_tag == .windows and mode.kind != .composited and self.fake_fullscreen_on) {
                self.turnFakeFullscreenOff();
            }
            try self.turnCompositedToFullscreen(mode);
        }
    }

    pub fn turnFullscreenOff(self: *Window, mode: WindowedMode) !void {
        const should_maximize = self.shouldMaximizeFromMode(mode);
        if (self.handle.getFullscreenMonitor(Monitor)) |_| {
            self.turnRealFullscreenToWindowed(should_maximize);
        } else if (os_tag == .windows and self.fake_fullscreen_on) {
            self.turnFakeFullscreenToWindowed(should_maximize);
        } else if (should_maximize) {
            self.handle.maximize();
        } else {
            self.handle.restore();
        }
    }

    fn turnRealFullscreenToWindowed(self: *Window, should_maximize: bool) void {
        self.setWindowed(self.restored_pos, self.restored_size);
        self.handle.restore();
        if (should_maximize) self.handle.maximize();
    }

    fn turnFakeFullscreenToWindowed(self: *Window, should_maximize: bool) void {
        self.turnFakeFullscreenOff();
        if (should_maximize) {
            self.handle.maximize();
        } else {
            // Since we're fake-restoring the window (fake fullscreen is technically restored), we need to manually flag that we've restored it.
            self.was_maximized = false;
        }
    }

    fn turnCompositedToFullscreen(self: *Window, mode: FullscreenMode) !void {
        const monitor = mode.monitor orelse try self.getWindowedMonitor() orelse try Monitor.getPrimary(self.allocator);
        if (os_tag != .windows) {
            const vm = try monitor.getCurrentMode();
            self.setFullscreen(monitor, vm.size, null);
        } else if (mode.kind == .composited) {
            try self.turnCompositedToFakeFullscreen(monitor);
        } else {
            const dvm = try monitors.getCurrentModeAsDesktopMode(monitor); // to cache current mode as desktop mode regardless
            const vm = if (mode.kind == .exclusive_max) try monitor.getMaxMode() else dvm;
            self.setFullscreen(monitor, vm.size, vm.refresh_rate);
        }
    }

    fn turnCompositedToFakeFullscreen(self: *Window, monitor: *Monitor) !void {
        const vm = try monitors.getCurrentModeAsDesktopMode(monitor);
        const pos = monitor.handle.getPos();
        const size = adjustWindowSizeForFakeFullscreen(vm.size);

        // Return if we're not changing anything, so the window doesn't flicker.
        if (self.isFakeFullscreenWithBounds(pos, size)) return;

        // We must first restore the window to deal with two cases:
        // 1. Win32 can't properly mutate a maximized window (this could later lead to a maximized window that doesn't cover the screen).
        // 2. We can't re-dock a "pane" (an unrestored, unmaximized window docked on the side of a monitor), so we don't want to cache its docked pos or size.
        // To properly deal with the first case, we have to manually cache the window's maximized state.
        const wm = self.was_maximized;
        self.handle.restore();
        self.was_maximized = wm;

        self.fake_fullscreen_on = true;
        self.handle.hide();
        self.handle.setDecorated(false);
        self.handle.setPos(pos);
        self.handle.setSize(size);
        self.handle.show();
    }

    fn turnFakeFullscreenOff(self: *Window) void {
        self.handle.hide();
        self.handle.setSize(self.restored_size);
        self.handle.setPos(self.restored_pos);
        self.handle.setDecorated(true);
        self.handle.show();
        self.fake_fullscreen_on = false;

        // For reasons similar to the above.
        self.handle.restore();
    }

    fn isFakeFullscreenWithBounds(self: Window, pos: Point, size: Size) bool {
        if (!self.fake_fullscreen_on) return false;

        const wpos = self.handle.getPos();
        if (pos.x != wpos.x or pos.y != wpos.y) return false;

        const wsize = self.handle.getSize();
        if (size.w != wsize.w or size.h != wsize.h) return false;

        return true;
    }

    fn shouldMaximizeFromMode(self: *Window, mode: WindowedMode) bool {
        return mode == .maximized or (mode == .previous_mode and self.was_maximized);
    }

    /// If possible, returns the monitor containing the center pixel of this window's content area.
    fn getWindowedMonitor(self: Window) !?*Monitor {
        if (os_tag == .linux) return null;

        const wpos = self.handle.getPos();
        const wsize = self.handle.getSize();
        const wx = wpos.x +% wsize.w / 2;
        const wy = wpos.y +% wsize.h / 2;

        var monitor_handles = &(try MonitorHandle.getAll());
        while (monitor_handles.next()) |monitor_handle| {
            const mpos = monitor_handle.getPos();
            const msize = (try monitor_handle.getCurrentVideoMode()).size;
            const ml = mpos.x;
            const mt = mpos.y;
            const mr = ml +% msize.w;
            const mb = mt +% msize.h;

            if (wx >= ml and wx < mr and wy >= mt and wy < mb) {
                return try monitors.getOrCreate(monitor_handle, self.allocator);
            }
        } else return null;
    }

    fn setFullscreen(self: *Window, monitor: *Monitor, size: Size, refresh_rate: ?i32) void {
        self.handle.setFullscreen(Monitor, monitor.handle, size, refresh_rate);
        self.handleModeSwitch();
    }

    fn setWindowed(self: *Window, pos: Point, size: Size) void {
        self.handle.setWindowed(pos, size);
        self.handleModeSwitch();
    }

    fn handleModeSwitch(self: *Window) void {
        const held = self.render_lock.acquire();
        defer held.release();

        // There are buffer swap oddities with some systems/drivers after a mode switch.
        // It *should* be at worst harmless to gently remind the system/driver what swap interval we want.
        // We're on the event thread, so we can't actually set the swap interval right now.
        self.restore_swap_interval = true;

        // There's a chance we're changing the refresh rate and therefore possibly the frame rate.
        self.frame_tracker.reset();
    }
};
