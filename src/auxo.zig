const std = @import("std");
const Allocator = std.mem.Allocator;
const Timer = std.time.Timer;
const Mutex = std.Thread.Mutex;
const os: std.Target.Os.Tag = @import("builtin").os.tag;

const io = @import("io.zig");
pub const Cardinal = io.Cardinal;
pub const Point = io.Point;
pub const Size = io.Size;
pub const Rectangle = io.Rectangle;
pub const DisplayInfo = io.DisplayInfo;
pub const MouseButtonAction = io.MouseButtonAction;
pub const MouseButton = io.MouseButton;
pub const KeyAction = io.KeyAction;
pub const KeyMods = io.KeyMods;
pub const Key = io.Key;
pub const JoyInfo = io.JoyInfo;
pub const JoyButtonState = io.JoyButtonState;
pub const JoyHatDirection = io.JoyHatDirection;
pub const JoyInputSource = io.JoyInputSource;
pub const JoyInputIndex = io.JoyInputIndex;
pub const JoyInput = io.JoyInput;
pub const GamepadButton = io.GamepadButton;
pub const GamepadTrigger = io.GamepadTrigger;
pub const GamepadStick = io.GamepadStick;
pub const GamepadHat = io.GamepadHat;
pub const GamepadMapping = io.GamepadMapping;
pub const GamepadStickState = io.GamepadStickState;
pub const JoyState = io.JoyState;

pub const DISPLAY_MAX = io.DISPLAY_MAX;
pub const JOYSTICK_MAX = io.JOYSTICK_MAX;
pub const JOY_BUTTON_MAX = io.JOY_BUTTON_MAX;
pub const JOY_AXIS_MAX = io.JOY_AXIS_MAX;
pub const JOY_HAT_MAX = io.JOY_HAT_MAX;

const glfw = @import("glfw.zig");
const Monitor = glfw.Monitor;
const Window = glfw.Window;

pub fn Model(comptime WorldState: type, comptime IoState: type, comptime AudioState: type, comptime VideoState: type) type {
    return struct {
        world_config: WorldConfig,
        event_handler: EventHandler(IoState),

        createWindowFn: fn ([]DisplayInfo) WindowCreationState,

        initFn: ?fn (*const App, *WorldState, *IoState, *AudioState, *VideoState) anyerror!void = null,
        initVideoFn: ?fn (*const App, *VideoState) anyerror!void = null,
        deinitVideoFn: ?fn (*VideoState) void = null,
        deinitFn: ?fn (*WorldState, *IoState, *AudioState, *VideoState) void = null,

        beginUpdateFn: fn (*const IoState, *WorldState) void,
        stepFn: fn (*WorldState) void,
        endUpdateFn: fn (*const WorldState, *IoState, *App, UpdateReport) void,
        writeAudioFn: fn (*const WorldState, *AudioState) void,
        writeVideoFn: fn (*const WorldState, *VideoState) void,
        beginFrameFn: fn (*VideoState, FrameInfo) void,
        composeGuiFn: ?fn (*IoState, FrameInfo) void = null,
        endFrameFn: fn (*VideoState, FrameInfo) void,
    };
}

pub const WorldConfig = struct {
    steps_per_second: f64,
    max_steps_per_update: ?u64 = null,
    max_seconds_per_update: ?f64 = null,
};

pub const UpdateReport = struct {
    /// Total time spent on steps during the update, in nanoseconds.
    time_spent: u64,

    /// Number of steps taken during the update.
    steps_taken: u64,

    /// Number of steps the game could not take during the update, due to either a cap on steps or a cap on stepping time.
    steps_skipped: u64,
};

pub const FrameInfo = struct {
    suggested_viewport: Rectangle,
    step_remainder: f64 = 0.0,
    estimated_fps: ?f64 = null,
};

pub const WindowCreationState = struct {
    title: [:0]const u8,
    restored_size: Size,
    maximized: bool = false,
    fullscreen_mode: ?FullscreenMode = null,
};

pub const FullscreenMode = struct {
    monitor_location: ?Point = null,
    is_exclusive: bool = false,
};

// ====================================================================================================================
// EVENTS

pub fn EventHandler(comptime State: type) type {
    return struct {
        closeRequestCallback: ?fn (*State, *App, CloseRequestArgs) void = null,
        focusCallback: ?fn (*State, *App, FocusArgs) void = null,
        iconifyCallback: ?fn (*State, *App, IconifyArgs) void = null,
        keyCallback: ?fn (*State, *App, KeyArgs) void = null,
        charCallback: ?fn (*State, *App, CharArgs) void = null,
        mouseButtonCallback: ?fn (*State, *App, MouseButtonArgs) void = null,
        cursorPosCallback: ?fn (*State, *App, CursorPosArgs) void = null,
        cursorEnterCallback: ?fn (*State, *App, CursorEnterArgs) void = null,
        scrollCallback: ?fn (*State, *App, ScrollArgs) void = null,
        displayStateCallback: ?fn (*State, *App, DisplayStateArgs) void = null,
        joyStateCallback: ?fn (*State, *App, JoyStateArgs) void = null,
    };
}

pub const CloseRequestArgs = void;

pub const FocusArgs = struct {
    focused: bool,
};

pub const IconifyArgs = struct {
    iconified: bool,
};

pub const KeyArgs = struct {
    key: Key,
    scancode: i32,
    action: KeyAction,
    mods: KeyMods,
};

pub const CharArgs = struct {
    codepoint: u21,
};

pub const MouseButtonArgs = struct {
    button: MouseButton,
    action: MouseButtonAction,
    mods: KeyMods,
};

pub const CursorPosArgs = struct {
    x: f64,
    y: f64,
};

pub const CursorEnterArgs = struct {
    entered: bool,
};

pub const ScrollArgs = struct {
    x_offset: f64,
    y_offset: f64,
};

pub const DisplayStateArgs = struct {
    displays: []DisplayInfo,
};

pub const JoyStateArgs = struct {
    id: usize,
    disconnected: bool,
    info: JoyInfo,
    state: JoyState,
};

// ====================================================================================================================

pub fn run(
    comptime WorldState: type,
    comptime IoState: type,
    comptime AudioState: type,
    comptime VideoState: type,
    comptime model: Model(WorldState, IoState, AudioState, VideoState),
    world_state: *WorldState,
    io_state: *IoState,
    audio_state: *AudioState,
    video_state: *VideoState,
) !void {
    logGlfwVersion();
    glfw.setErrorCallback(onGlfwError);

    try glfw.init();
    defer glfw.terminate();

    // Populate array of displays
    const monitors = try Monitor.getAll();
    var display_buffer: [DISPLAY_MAX]DisplayInfo = undefined;
    const display_count = @min(monitors.len, DISPLAY_MAX);
    for (monitors[0..display_count]) |monitor, i| {
        display_buffer[i] = try getDisplayInfo(monitor.?);
    }

    const creation_state = model.createWindowFn(display_buffer[0..display_count]);
    glfw.hintWindowMaximized(creation_state.maximized);
    var maybe_mon: ?*Monitor = null;

    // Determine the size and optional monitor to pass to glfwCreateWindow
    var creation_mon: ?*Monitor = null;
    var creation_size = creation_state.restored_size;
    if (creation_state.fullscreen_mode) |mode| {
        const loc = mode.monitor_location orelse Point.zero;
        maybe_mon = getMonitorAtPoint(monitors, loc.x, loc.y);
        if (maybe_mon) |mon| {
            // Handle real fullscreen via glfwCreateWindow
            if (os != .windows or mode.is_exclusive) {
                const vm = try mon.getVideoMode();
                creation_mon = mon;
                creation_size = .{ .w = vm.width, .h = vm.height };
            }
        }
    }

    const window = try Window.create(creation_size, creation_state.title, creation_mon, null);
    defer window.destroy();

    var app: App = .{
        .window = window,
        .restored_pos = window.getPos() catch Point.zero,
        .restored_size = creation_state.restored_size,
        .was_maximized = creation_state.maximized,
        .frame_tracker = .{ .timer = try Timer.start() },
    };

    if (creation_state.fullscreen_mode) |mode| {
        if (maybe_mon) |mon| {
            // Handle fake fullscreen manually
            if (os == .windows and !mode.is_exclusive) {
                try app.turnCompositedToFakeFullscreen(mon);
            }
        }
    }

    const fb = try window.getFramebufferSize();
    const G = Game(WorldState, IoState, AudioState, VideoState, model);
    var game = G{
        .app = &app,
        .world_state = world_state,
        .io_state = io_state,
        .audio_state = audio_state,
        .video_state = video_state,
        .timer = try Timer.start(),
        .display_buffer = display_buffer,
        .display_count = display_count,
        .framebuffer_size = (@intCast(u64, fb.w) << 32) | (@intCast(u64, fb.h)),
    };
    window.setUserPointer(G, &game);

    window.setPosCallback(G, G.onPos);
    window.setSizeCallback(G, G.onSize);
    window.setFramebufferSizeCallback(G, G.onFramebufferSize);
    window.setCloseCallback(G, G.onClose);
    window.setFocusCallback(G, G.onFocus);
    window.setIconifyCallback(G, G.onIconify);
    window.setMaximizeCallback(G, G.onMaximize);
    window.setCursorEnterCallback(G, G.onCursorEnter);
    window.setKeyCallback(G, G.onKey);
    window.setCharCallback(G, G.onChar);
    window.setMouseButtonCallback(G, G.onMouseButton);
    window.setCursorPosCallback(G, G.onCursorPos);
    window.setScrollCallback(G, G.onScroll);

    try game.eventLoop();
}

pub const App = struct {
    const Self = @This();

    window: *Window,
    frame_tracker: FrameTracker(60, 240),

    // Cached state
    restored_pos: Point,
    restored_size: Size,
    was_maximized: bool = false,
    fake_fullscreen_on: if (os == .windows) bool else void = if (os == .windows) false else {},

    // Queued state
    should_close: bool = false,
    queued_mode_change: ModeChange = .{ .none = {} },

    const ModeChangeKind = enum {
        none,
        toggle_fullscreen,
        turn_fullscreen_on,
        turn_fullscreen_off,
    };
    const ModeChange = union(ModeChangeKind) {
        none: void,
        toggle_fullscreen: FullscreenMode,
        turn_fullscreen_on: FullscreenMode,
        turn_fullscreen_off: void,
    };

    fn onPos(self: *Self, pos: Point) void {
        if (self.shouldCacheRestoredBounds()) {
            self.restored_pos = pos;
        }
    }

    fn onSize(self: *Self, size: Size) void {
        if (size.w > 0 and size.h > 0 and self.shouldCacheRestoredBounds()) {
            self.restored_size = size;
        }
    }

    fn onMaximize(self: *Self, value: bool) void {
        if (os != .windows or !self.fake_fullscreen_on) {
            self.was_maximized = value;
        }
    }

    fn shouldCacheRestoredBounds(self: *Self) bool {
        // Don't cache if fake fullscreen
        if (os == .windows and self.fake_fullscreen_on) return false;

        // Don't cache if real fullscreen
        if (self.window.getFullscreenMonitor()) |_| return false;

        // Don't cache if iconified
        if (self.window.isIconified()) return false;

        // Don't cache if maximized
        if (self.window.isMaximized()) return false;

        return true;
    }

    pub fn close(self: *Self) void {
        self.should_close = true;
    }

    pub fn toggleFullscreen(self: *Self, fullscreen_mode: FullscreenMode) void {
        self.queued_mode_change = .{ .toggle_fullscreen = fullscreen_mode };
    }

    pub fn turnFullscreenOn(self: *Self, mode: FullscreenMode) void {
        self.queued_mode_change = .{ .turn_fullscreen_on = mode };
    }

    pub fn turnFullscreenOff(self: *Self) void {
        self.queued_mode_change = .{ .turn_fullscreen_off = {} };
    }

    fn changeMode(self: *Self, mode_change: ModeChange) !void {
        switch (mode_change) {
            .none => return,
            .toggle_fullscreen => |mode| {
                if (self.window.getFullscreenMonitor()) |_| {
                    self.turnRealFullscreenToWindowed();
                } else if (os == .windows and self.fake_fullscreen_on) {
                    self.turnFakeFullscreenToWindowed();
                } else {
                    const monitor = try findMonitorMatch(mode.monitor_location, self.window);
                    if (os == .windows and !mode.is_exclusive) {
                        try self.turnCompositedToFakeFullscreen(monitor);
                    } else {
                        const vm = try monitor.getVideoMode();
                        self.window.setFullscreen(monitor, .{ .w = vm.width, .h = vm.height }, vm.refreshRate);
                    }
                }
            },
            .turn_fullscreen_on => |mode| {
                const monitor = try findMonitorMatch(mode.monitor_location, self.window);
                if (os == .windows and !mode.is_exclusive) {
                    if (self.window.getFullscreenMonitor()) |_| {
                        self.turnRealFullscreenToWindowed();
                    }
                    try self.turnCompositedToFakeFullscreen(monitor);
                } else {
                    if (os == .windows and self.fake_fullscreen_on) {
                        self.turnFakeFullscreenToWindowed();
                    }
                    const vm = try monitor.getVideoMode();
                    self.window.setFullscreen(monitor, .{ .w = vm.width, .h = vm.height }, vm.refreshRate);
                }
            },
            .turn_fullscreen_off => {
                if (self.window.getFullscreenMonitor()) |_| {
                    self.turnRealFullscreenToWindowed();
                } else if (os == .windows and self.fake_fullscreen_on) {
                    self.turnFakeFullscreenToWindowed();
                } // else nothing needed
            },
        }
    }

    fn turnCompositedToFakeFullscreen(self: *Self, monitor: *Monitor) !void {
        const pos = try monitor.getPos();
        const vm = try monitor.getVideoMode();
        const size = Size{ .w = vm.width, .h = vm.height };

        // Return if we're already fake fullscreen on this monitor, so the window doesn't flicker.
        if (self.isFakeFullscreenWithBounds(pos, size)) return;

        // We must first restore the window to deal with two cases:
        // 1. Win32 can't properly mutate a maximized window (this could later lead to a maximized window that doesn't cover the screen).
        // 2. We can't re-dock a "pane" (an unrestored, unmaximized window docked on the side of a monitor), so we don't want to cache its docked pos or size.
        // To properly deal with the first case, we have to manually cache the window's maximized state.
        const wm = self.was_maximized;
        self.window.restore();
        self.was_maximized = wm;

        self.fake_fullscreen_on = true;
        self.window.hide();
        self.window.setDecorated(false);
        self.window.setPos(pos);
        self.window.setSize(size);
        self.window.show();
    }

    fn turnFakeFullscreenToWindowed(self: *Self) void {
        self.window.hide();
        self.window.setSize(self.restored_size);
        self.window.setPos(self.restored_pos);
        self.window.setDecorated(true);
        self.window.show();
        self.fake_fullscreen_on = false;

        self.window.restore();
        if (self.was_maximized) self.window.maximize();
    }

    fn turnRealFullscreenToWindowed(self: *Self) void {
        self.window.setWindowed(self.restored_pos, self.restored_size);

        self.window.restore();
        if (self.was_maximized) self.window.maximize();
    }

    fn isFakeFullscreenWithBounds(self: *Self, pos: Point, size: Size) bool {
        if (!self.fake_fullscreen_on) return false;

        const wpos = self.window.getPos() catch return false;
        if (pos.x != wpos.x or pos.y != wpos.y) return false;

        const wsize = self.window.getSize() catch return false;
        if (size.w != wsize.w or size.h != wsize.h) return false;

        return true;
    }
};

const Joystick = struct {
    const ButtonField = std.meta.Int(.unsigned, JOY_BUTTON_MAX);
    const AxisField = [JOY_AXIS_MAX]i16;
    const HatField = [JOY_HAT_MAX]JoyHatDirection;
    const button_default: ButtonField = 0;
    const axis_default = [_]i16{0} ** JOY_AXIS_MAX;
    const hat_default = [_]JoyHatDirection{.centered} ** JOY_HAT_MAX;

    buttons: ButtonField = button_default,
    axes: AxisField = axis_default,
    hats: HatField = hat_default,
    info: JoyInfo = .{},
    connected: bool = false,

    fn save_state(self: *Joystick, buttons: []JoyButtonState, axes: []f32, hats: []JoyHatDirection, state_changed: *bool) void {
        var ba = button_default;
        for (buttons) |button, i| {
            const S = comptime std.meta.Int(.unsigned, std.math.log2_int_ceil(usize, JOY_BUTTON_MAX));
            if (button == .pressed) ba |= comptime @intCast(ButtonField, 1) << @intCast(S, i);
        }

        var ha = hat_default;
        for (hats) |hat, i| {
            ha[i] = hat;
        }

        var aa = axis_default;
        for (axes) |axis, i| {
            // This should be lossless but it's not the end of the world if it isn't
            aa[i] = @floatToInt(i16, @max(-32768, @min(32767, axis * 32767.5 - 0.5)));
        }

        state_changed.* = self.buttons != ba or !std.mem.eql(i16, &self.axes, &aa) or !std.mem.eql(JoyHatDirection, &self.hats, &ha);
        self.buttons = ba;
        self.axes = aa;
        self.hats = ha;
    }
};

fn Game(comptime WorldState: type, comptime IoState: type, comptime AudioState: type, comptime VideoState: type, comptime model: Model(WorldState, IoState, AudioState, VideoState)) type {
    return struct {
        const Self = @This();

        app: *App,
        world_state: *WorldState,
        io_state: *IoState,
        audio_state: *AudioState,
        video_state: *VideoState,
        timer: Timer,
        world_timestamp: u64 = 0,
        video_timestamp: u64 = 0,
        input_lock: Mutex = .{},
        world_lock: RwLock = .{},
        audio_lock: Mutex = .{},
        framebuffer_size: u64,
        display_buffer: [DISPLAY_MAX]DisplayInfo,
        display_count: usize = 0,
        joysticks: [glfw.JOYSTICK_COUNT]Joystick = [_]Joystick{.{}} ** glfw.JOYSTICK_COUNT,

        const ticks_per_step = result: {
            const max_int = std.math.maxInt(u64);
            const f_ticks_per_step = @round(@intToFloat(f64, std.time.ns_per_s) / model.world_config.steps_per_second);
            break :result if (f_ticks_per_step < max_int) @floatToInt(u64, f_ticks_per_step) else max_int;
        };

        const max_steps_per_update = model.world_config.max_steps_per_update orelse std.math.maxInt(u64);

        const max_ticks_per_update = result: {
            const max_int = std.math.maxInt(u64);
            const mspu = model.world_config.max_seconds_per_update orelse std.math.inf_f64;
            const mtpu = @round(mspu * @intToFloat(f64, std.time.ns_per_s));
            break :result if (mtpu < max_int) @floatToInt(u64, mtpu) else max_int;
        };

        fn eventLoop(self: *Self) !void {
            if (model.initFn) |init| {
                try init(self.app, self.world_state, self.io_state, self.audio_state, self.video_state);
            }
            defer {
                if (model.deinitFn) |deinit| {
                    deinit(self.world_state, self.io_state, self.audio_state, self.video_state);
                }
            }

            self.timer.reset();

            var event_loop_broken = false;
            var render_loop_aborted = false;
            const render_thread = try std.Thread.spawn(.{}, renderLoop, .{ self, &event_loop_broken, &render_loop_aborted });
            defer {
                event_loop_broken = true;
                render_thread.join();
            }

            while (!self.app.should_close and !render_loop_aborted) {
                // Swallow GLFW errors and rely on GLFW error callback for logging
                self.readMonitors() catch {};

                var jid: i32 = 0;
                while (jid < glfw.JOYSTICK_COUNT) : (jid += 1) {
                    self.readJoystick(jid) catch {};
                }

                self.update();

                var mode_change: App.ModeChange = undefined;
                {
                    self.input_lock.lock();
                    defer self.input_lock.unlock();

                    mode_change = self.app.queued_mode_change;
                    self.app.queued_mode_change = .{ .none = {} };
                }
                self.app.changeMode(mode_change) catch {};

                glfw.waitEvents();
            }
        }

        fn renderLoop(self: *Self, event_loop_broken: *const bool, render_loop_aborted: *bool) void {
            if (model.initVideoFn) |init| {
                init(self.app, self.video_state) catch {
                    render_loop_aborted.* = true;
                    return;
                };
            }
            defer {
                if (model.deinitVideoFn) |deinit| {
                    deinit(self.video_state);
                }
            }

            while (!event_loop_broken.*) {
                self.update();
                self.writeVideo();
                self.render();
                glfw.postEmptyEvent();
            }
        }

        fn update(self: *Self) void {
            var timestamp = self.timer.read();
            var world_delta = timestamp - @atomicLoad(u64, &self.world_timestamp, .SeqCst);
            if (world_delta < ticks_per_step) return;

            const report = update: {
                self.world_lock.writeLock();
                defer self.world_lock.writeUnlock();

                // Check again now that we've locked the method
                timestamp = self.timer.read();
                world_delta = timestamp - self.world_timestamp;
                if (world_delta < ticks_per_step) return;

                {
                    self.input_lock.lock();
                    defer self.input_lock.unlock();

                    model.beginUpdateFn(self.io_state, self.world_state);
                }

                // =============================================================
                const beginning_timestamp = self.timer.read();

                var update_steps: u64 = 0;
                var update_ticks: u64 = 0;
                while (update_steps < max_steps_per_update and update_ticks < max_ticks_per_update) {
                    model.stepFn(self.world_state);
                    timestamp = self.timer.read();
                    self.world_timestamp += ticks_per_step;
                    update_steps += 1;
                    update_ticks = timestamp - beginning_timestamp;
                    world_delta = timestamp - self.world_timestamp;
                    if (world_delta < ticks_per_step) break;
                }

                const ending_timestamp = self.timer.read();
                // =============================================================

                const steps_skipped = world_delta / ticks_per_step;
                self.world_timestamp += ticks_per_step * steps_skipped;

                const report = UpdateReport{
                    .time_spent = ending_timestamp - beginning_timestamp,
                    .steps_taken = update_steps,
                    .steps_skipped = steps_skipped,
                };

                // Downgrade exclusive lock to shared lock
                self.world_lock.readLockFromWriteLock();

                break :update report;
            };

            {
                defer self.world_lock.readUnlock();

                {
                    self.input_lock.lock();
                    defer self.input_lock.unlock();

                    model.endUpdateFn(self.world_state, self.io_state, self.app, report);
                }
                {
                    self.audio_lock.lock();
                    defer self.audio_lock.unlock();

                    model.writeAudioFn(self.world_state, self.audio_state);
                }
            }
        }

        fn writeVideo(self: *Self) void {
            if (self.video_timestamp < @atomicLoad(u64, &self.world_timestamp, .SeqCst)) {
                self.world_lock.readLock();
                defer self.world_lock.readUnlock();

                model.writeVideoFn(self.world_state, self.video_state);
                self.video_timestamp = self.world_timestamp;
            }
        }

        fn render(self: *Self) void {
            const s = @atomicLoad(u64, &self.framebuffer_size, .SeqCst);
            const size: Size = .{ .w = @intCast(u31, (s >> 32) & std.math.maxInt(u31)), .h = @intCast(u31, s & std.math.maxInt(u31)) };
            if (size.w == 0 or size.h == 0) return;

            const estimated_fps = self.app.frame_tracker.estimateFps();
            const delta_ticks = self.timer.read() - self.video_timestamp;
            const frame_info = FrameInfo{
                .suggested_viewport = Rectangle{
                    .pos = Point{ .x = 0, .y = 0 },
                    .size = size,
                },
                .step_remainder = @intToFloat(f64, delta_ticks) / @intToFloat(f64, ticks_per_step),
                .estimated_fps = estimated_fps,
            };

            model.beginFrameFn(self.video_state, frame_info);
            if (model.composeGuiFn) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, frame_info);
            }
            model.endFrameFn(self.video_state, frame_info);

            self.app.frame_tracker.startOrLap();
        }

        fn readMonitors(self: *Self) glfw.Error!void {
            var state_changed = false;
            const monitors = try Monitor.getAll();
            const display_count = @min(monitors.len, DISPLAY_MAX);
            if (self.display_count != display_count) {
                state_changed = true;
            }

            // Use temp buffer to avoid writing an incomplete display state on error
            var buffer: [DISPLAY_MAX]DisplayInfo = undefined;

            for (monitors[0..display_count]) |maybe_monitor, i| {
                const monitor = maybe_monitor orelse return glfw.Error.Unknown;
                const old_info = self.display_buffer[i];
                const new_info = try getDisplayInfo(monitor);
                if (std.meta.eql(old_info, new_info)) {
                    buffer[i] = old_info;
                } else {
                    buffer[i] = new_info;
                    state_changed = true;
                }
            }

            if (state_changed) {
                self.display_count = display_count;
                std.mem.copy(DisplayInfo, self.display_buffer[0..display_count], buffer[0..display_count]);

                if (model.event_handler.displayStateCallback) |f| {
                    self.input_lock.lock();
                    defer self.input_lock.unlock();

                    f(self.io_state, self.app, .{
                        .displays = buffer[0..display_count],
                    });
                }
            }
        }

        /// In addition to GLFW-reported errors, returns glfw.Error.Unknown if we can't read a detected joystick's state.
        fn readJoystick(self: *Self, jid: i32) glfw.Error!void {
            const j = @intCast(usize, jid);
            var joystick = &self.joysticks[j];

            if (!glfw.joystickPresent(jid)) {
                const joyStateCallback = model.event_handler.joyStateCallback orelse return;
                if (joystick.connected) {
                    self.input_lock.lock();
                    defer self.input_lock.unlock();

                    joyStateCallback(self.io_state, self.app, .{
                        .id = j,
                        .disconnected = true,
                        .info = joystick.info, // cached info
                        .state = .{
                            .buttons = &[0]JoyButtonState{},
                            .axes = &[0]f32{},
                            .hats = &[0]JoyHatDirection{},
                        },
                    });
                    joystick.* = .{};
                }
                return;
            }

            var vid: ?u16 = null;
            var pid: ?u16 = null;
            var is_xinput = false;
            if (try glfw.getJoystickGuid(jid)) |guid| {
                // On GLFW, only read VID and PID if this isn't an XInput controller
                const XINPUT_ID = "7869";
                is_xinput = guid.len >= 4 and std.mem.eql(u8, guid[0..4], XINPUT_ID);
                if (!is_xinput) {
                    if (guid.len >= 12) vid = stringToId(guid[8..12]);
                    if (guid.len >= 20) pid = stringToId(guid[16..20]);
                }
            } else return glfw.Error.Unknown;
            const info = JoyInfo{
                .vid = vid,
                .pid = pid,
                .is_xinput = is_xinput,
            };
            joystick.info = info;

            var button_buffer: [JOY_BUTTON_MAX]JoyButtonState = undefined;
            var buttons: []JoyButtonState = undefined;
            if (try glfw.getJoystickButtons(jid)) |new_buttons| {
                const len = @min(JOY_BUTTON_MAX, new_buttons.len);
                var i: usize = 0;
                while (i < len) : (i += 1) {
                    button_buffer[i] = switch (new_buttons[i]) {
                        .release => .released,
                        .press => .pressed,
                        _ => .released,
                    };
                }
                buttons = button_buffer[0..len];
            } else return glfw.Error.Unknown;

            var axis_buffer: [JOY_AXIS_MAX]f32 = undefined;
            var axes: []f32 = undefined;
            if (try glfw.getJoystickAxes(jid)) |new_axes| {
                const len = @min(JOY_AXIS_MAX, new_axes.len);
                std.mem.copy(f32, &axis_buffer, new_axes[0..len]);
                axes = axis_buffer[0..len];
            } else return glfw.Error.Unknown;

            var hat_buffer: [JOY_HAT_MAX]JoyHatDirection = undefined;
            var hats: []JoyHatDirection = undefined;
            if (try glfw.getJoystickHats(jid)) |new_hats| {
                const len = @min(JOY_HAT_MAX, new_hats.len);
                var i: usize = 0;
                while (i < len) : (i += 1) {
                    hat_buffer[i] = switch (new_hats[i]) {
                        .up => .north,
                        .right_up => .northeast,
                        .right => .east,
                        .right_down => .southeast,
                        .down => .south,
                        .left_down => .southwest,
                        .left => .west,
                        .left_up => .northwest,
                        .centered => .centered,
                        _ => .centered,
                    };
                }
                hats = hat_buffer[0..len];
            } else return glfw.Error.Unknown;

            var state_changed = false;
            joystick.save_state(buttons, axes, hats, &state_changed);
            const joyStateCallback = model.event_handler.joyStateCallback orelse return;
            if (state_changed) {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                joyStateCallback(self.io_state, self.app, .{
                    .id = j,
                    .disconnected = false,
                    .info = info,
                    .state = .{
                        .buttons = buttons,
                        .axes = axes,
                        .hats = hats,
                    },
                });
            }
        }

        fn stringToId(s: *const [4]u8) ?u16 {
            // IDs are stored LE
            const a = charToHex(s[2]) orelse return null;
            const b = charToHex(s[3]) orelse return null;
            const c = charToHex(s[0]) orelse return null;
            const d = charToHex(s[1]) orelse return null;
            return (a << 12) | (b << 8) | (c << 4) | (d);
        }

        fn charToHex(c: u8) ?u16 {
            return switch (c) {
                '0'...'9' => c - '0' + 0x0,
                'A'...'F' => c - 'A' + 0xA,
                'a'...'f' => c - 'a' + 0xa,
                else => null,
            };
        }

        fn onPos(self: *Self, pos: Point) void {
            self.app.onPos(pos);
        }

        fn onSize(self: *Self, size: Size) void {
            self.app.onSize(size);
        }

        fn onFramebufferSize(self: *Self, size: Size) void {
            if (size.w > 0 and size.h > 0) {
                const s = (@intCast(u64, size.w) << 32) | (@intCast(u64, size.h));
                @atomicStore(u64, &self.framebuffer_size, s, .SeqCst);
            }
        }

        fn onClose(self: *Self) void {
            if (model.event_handler.closeRequestCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, {});
            }
        }

        fn onFocus(self: *Self, focused: bool) void {
            if (model.event_handler.focusCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{ .focused = focused });
            }
        }

        fn onIconify(self: *Self, iconified: bool) void {
            if (model.event_handler.iconifyCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{ .iconified = iconified });
            }
        }

        fn onMaximize(self: *Self, value: bool) void {
            self.app.onMaximize(value);
        }

        fn onKey(self: *Self, key: Key, scancode: i32, action: KeyAction, mods: KeyMods) void {
            if (model.event_handler.keyCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{
                    .key = key,
                    .scancode = scancode,
                    .action = action,
                    .mods = mods,
                });
            }
        }

        fn onChar(self: *Self, codepoint: u21) void {
            if (model.event_handler.charCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{
                    .codepoint = codepoint,
                });
            }
        }

        fn onMouseButton(self: *Self, button: MouseButton, action: MouseButtonAction, mods: KeyMods) void {
            if (model.event_handler.mouseButtonCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{
                    .button = button,
                    .action = action,
                    .mods = mods,
                });
            }
        }

        fn onCursorPos(self: *Self, x: f64, y: f64) void {
            if (model.event_handler.cursorPosCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{
                    .x = x,
                    .y = y,
                });
            }
        }

        fn onCursorEnter(self: *Self, entered: bool) void {
            if (model.event_handler.cursorEnterCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{ .entered = entered });
            }
        }

        fn onScroll(self: *Self, x_offset: f64, y_offset: f64) void {
            if (model.event_handler.scrollCallback) |f| {
                self.input_lock.lock();
                defer self.input_lock.unlock();

                f(self.io_state, self.app, .{
                    .x_offset = x_offset,
                    .y_offset = y_offset,
                });
            }
        }
    };
}

// ====================================================================================================================
// HELPER FUNCTIONS

fn logGlfwVersion() void {
    std.log.info("Compiled against GLFW {}.{}.{}", .{ glfw.versionMajor, glfw.versionMinor, glfw.versionRevision });

    var major: i32 = 0;
    var minor: i32 = 0;
    var revision: i32 = 0;
    glfw.getVersion(&major, &minor, &revision);
    const version_str = glfw.getVersionString();
    std.log.info("Running against GLFW {}.{}.{} ({s})", .{ major, minor, revision, version_str });
}

fn onGlfwError(error_code: ?glfw.Error, description: []const u8) void {
    std.log.err("GLFW error {s}: {s}", .{ @errorName(error_code orelse glfw.Error.Unknown), description });
}

fn getDisplayInfo(monitor: *Monitor) glfw.Error!DisplayInfo {
    const vm = try monitor.getVideoMode();
    const size = Size{ .w = vm.width, .h = vm.height };
    const pos = try monitor.getPos();
    const work_area = try monitor.getWorkarea();
    const scale = try monitor.getContentScale();
    return .{
        .display_area = .{ .size = size, .pos = pos },
        .work_area = work_area,
        .refresh_rate = @intToFloat(f32, vm.refreshRate),
        .scale_factor = scale.xscale,
    };
}

fn findMonitorMatch(point: ?Point, window: ?*Window) !*Monitor {
    var monitors = Monitor.getAll() catch return Monitor.getPrimary();

    // 1. Try to return monitor at point, if given.
    if (point) |p| {
        if (getMonitorAtPoint(monitors, p.x, p.y)) |monitor| return monitor;
    }

    // 2. Try to return monitor at center of window, if given.
    if (window) |w| b: {
        const wpos = w.getPos() catch break :b;
        const wsize = w.getSize() catch break :b;
        const wx = wpos.x +% @divTrunc(wsize.w, 2);
        const wy = wpos.y +% @divTrunc(wsize.h, 2);
        if (getMonitorAtPoint(monitors, wx, wy)) |monitor| return monitor;
    }

    // 3. Try to return any monitor.
    for (monitors) |maybe_monitor| {
        if (maybe_monitor) |monitor| return monitor;
    }

    // 4. Fall back on the primary monitor.
    return Monitor.getPrimary();
}

fn getMonitorAtPoint(monitors: []?*Monitor, x: i32, y: i32) ?*Monitor {
    for (monitors) |maybe_monitor| {
        if (maybe_monitor) |monitor| {
            const pos = monitor.getPos() catch continue;
            const vm = monitor.getVideoMode() catch continue;
            const ml = pos.x;
            const mt = pos.y;
            const mr = ml +% vm.width;
            const mb = mt +% vm.height;
            if (x >= ml and x < mr and y >= mt and y < mb) {
                return monitor;
            }
        }
    }
    return null;
}

// ====================================================================================================================

/// A simple reader-writer lock with the functionality to downgrade a write lock to a read lock.
const RwLock = struct {
    state: usize = 0,
    write_lock: std.Thread.Mutex = .{},
    read_semaphore: std.Thread.Semaphore = .{},

    const IS_WRITING: usize = 1;
    const WRITER: usize = 1 << 1;
    const READER: usize = 1 << (1 + @bitSizeOf(Count));
    const WRITER_MASK: usize = std.math.maxInt(Count) << @ctz(WRITER);
    const READER_MASK: usize = std.math.maxInt(Count) << @ctz(READER);
    const Count = std.meta.Int(.unsigned, @divFloor(@bitSizeOf(usize) - 1, 2));

    pub fn writeLock(self: *RwLock) void {
        _ = @atomicRmw(usize, &self.state, .Add, WRITER, .SeqCst);
        self.write_lock.lock();

        const state = @atomicRmw(usize, &self.state, .Or, IS_WRITING, .SeqCst);
        _ = @atomicRmw(usize, &self.state, .Sub, WRITER, .SeqCst);
        if (state & READER_MASK != 0) {
            self.read_semaphore.wait();
        }
    }

    pub fn writeUnlock(self: *RwLock) void {
        _ = @atomicRmw(usize, &self.state, .And, ~IS_WRITING, .SeqCst);
        self.write_lock.unlock();
    }

    pub fn readLock(self: *RwLock) void {
        var state = @atomicLoad(usize, &self.state, .SeqCst);
        while (state & (IS_WRITING | WRITER_MASK) == 0) {
            state = @cmpxchgWeak(usize, &self.state, state, state + READER, .SeqCst, .SeqCst) orelse return;
        }

        {
            self.write_lock.lock();
            defer self.write_lock.unlock();

            _ = @atomicRmw(usize, &self.state, .Add, READER, .SeqCst);
        }
    }

    pub fn readUnlock(self: *RwLock) void {
        const state = @atomicRmw(usize, &self.state, .Sub, READER, .SeqCst);
        if ((state & IS_WRITING != 0) and (state & READER_MASK == READER)) {
            self.read_semaphore.post();
        }
    }

    pub fn readLockFromWriteLock(self: *RwLock) void {
        _ = @atomicRmw(usize, &self.state, .Add, READER, .SeqCst);
    }
};

/// A simple pseudo ring buffer for tracking frame rate.
/// The type args are the minimum and maximum number of frames the tracker is allowed to use to estimate FPS.
/// Thread-safe.
fn FrameTracker(comptime min_frames: u32, comptime max_frames: u32) type {
    return struct {
        const Self = @This();

        timer: Timer,
        lock: Mutex = .{},
        started: bool = false,
        head: usize = 0,
        size: usize = 0,
        buffer: [max_frames]u64 = [_]u64{undefined} ** max_frames,
        buffer_sorted: [max_frames]u64 = [_]u64{undefined} ** max_frames,

        /// Starts the tracker if needed, or laps and records the frame time. Must be called exactly once each frame, at the same point in the frame (e.g. right after swapping buffers).
        pub fn startOrLap(self: *Self) void {
            self.lock.lock();
            defer self.lock.unlock();

            if (!self.started) {
                self.timer.reset();
                self.started = true;
                return;
            }

            self.buffer[self.head] = self.timer.lap();
            self.head = (self.head + 1) % max_frames;
            if (self.size < max_frames) self.size += 1;

            const s = self.size;
            std.mem.copy(u64, self.buffer_sorted[0..s], self.buffer[0..s]);
            std.sort.sort(u64, self.buffer_sorted[0..s], {}, comptime std.sort.asc(u64));
        }

        /// Clear all recorded frame times from the buffer, starting fresh.
        pub fn reset(self: *Self) void {
            self.lock.lock();
            defer self.lock.unlock();

            // No point in clearing buffers themselves.
            self.started = false;
            self.head = 0;
            self.size = 0;
        }

        /// Get an estimate of FPS. Returns null if the tracker hasn't recorded sufficient data to estimate frame rate.
        pub fn estimateFps(self: *Self) ?f64 {
            self.lock.lock();
            defer self.lock.unlock();

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
