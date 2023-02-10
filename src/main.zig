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

pub const zgpu = @import("zgpu");
pub const zgui = @import("zgui");
pub const zmath = @import("zmath");
pub const zaudio = @import("zaudio");

pub const Seconds = f64;

const netcode = @import("netcode");
const glfw = @import("glfw.zig");
const Monitor = glfw.Monitor;
const Window = glfw.Window;

pub fn GameTable(comptime Game: type) type {
    return struct {
        // Config
        step_time: Seconds,
        max_step_delay: Seconds = 1.0,

        // Methods
        step: fn (*Game, *AppContext) void,
        skipSteps: fn (*Game, *AppContext, usize) void,
        draw: fn (*Game, *AppContext, *zgpu.GraphicsContext, FrameInfo) void,
        onSwapchainResized: fn (*Game, *AppContext, *zgpu.GraphicsContext, Size) void,
        receiveCloseRequest: fn (*Game, *AppContext, CloseRequestArgs) void,
        receiveFocusState: fn (*Game, *AppContext, FocusStateArgs) void,
        receiveIconifyState: fn (*Game, *AppContext, IconifyStateArgs) void,
        receiveKeyAction: fn (*Game, *AppContext, KeyActionArgs) void,
        receiveCharInput: fn (*Game, *AppContext, CharInputArgs) void,
        receiveMouseButtonAction: fn (*Game, *AppContext, MouseButtonActionArgs) void,
        receiveMouseScroll: fn (*Game, *AppContext, MouseScrollArgs) void,
        receiveCursorPosition: fn (*Game, *AppContext, CursorPositionArgs) void,
        receiveCursorEntryState: fn (*Game, *AppContext, CursorEntryStateArgs) void,
        receiveJoyState: fn (*Game, *AppContext, JoyStateArgs) void,
        receiveDisplayState: fn (*Game, *AppContext, DisplayStateArgs) void,
        receiveServerData: fn (*Game, *AppContext, ServerDataArgs) void,
    };
}

pub fn ClientTable(comptime Client: type, comptime Game: type) type {
    return struct {
        // Config
        use_imgui: bool = false,

        // Methods
        createWindow: fn (*Client, []DisplayInfo) anyerror!WindowCreationState,
        init: fn (*Client, *AppContext, *zgpu.GraphicsContext) anyerror!*Game,
        deinit: fn (*Client, *const AppContext, *Game) void,

        // Inner
        game_table: GameTable(Game),
    };
}

pub fn LoopbackServerTable(comptime LoopbackServer: type) type {
    return struct {
        // Methods
        run: fn (*LoopbackServer, *LoopbackContext) void,
    };
}

pub const FrameInfo = struct {
    texture_view: zgpu.wgpu.TextureView,
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

pub const CloseRequestArgs = void;

pub const FocusStateArgs = struct {
    focused: bool,
};

pub const IconifyStateArgs = struct {
    iconified: bool,
};

pub const KeyActionArgs = struct {
    key: Key,
    scancode: i32,
    action: KeyAction,
    mods: KeyMods,
};

pub const CharInputArgs = struct {
    codepoint: u21,
};

pub const MouseButtonActionArgs = struct {
    button: MouseButton,
    action: MouseButtonAction,
    mods: KeyMods,
};

pub const MouseScrollArgs = struct {
    x_offset: f64,
    y_offset: f64,
};

pub const CursorPositionArgs = struct {
    x: f64,
    y: f64,
};

pub const CursorEntryStateArgs = struct {
    entered: bool,
};

pub const JoyStateArgs = struct {
    id: usize,
    connected: bool,
    info: JoyInfo,
    state: JoyState,
};

pub const DisplayStateArgs = struct {
    displays: []DisplayInfo,
};

pub const ServerDataArgs = struct {
    data: []const u8,
    sequence: u64,
};

// ====================================================================================================================

pub const Packet = struct {
    data: []u8,
    sequence: u64,
};

pub const LoopbackContext = struct {
    const MAX_INCOMING_PACKETS = 64;
    const MAX_OUTGOING_PACKETS = 64;

    client_active: bool = true,
    client_connected: bool = false,
    incoming_packets: RingBuffer(MAX_INCOMING_PACKETS, Packet) = .{},
    incoming_packet_buffer: [MAX_INCOMING_PACKETS][netcode.MAX_PACKET_SIZE]u8 = [_][netcode.MAX_PACKET_SIZE]u8{[_]u8{0} ** netcode.MAX_PACKET_SIZE} ** MAX_INCOMING_PACKETS,
    outgoing_packets: RingBuffer(MAX_OUTGOING_PACKETS, Packet) = .{},
    outgoing_packet_buffer: [MAX_OUTGOING_PACKETS][netcode.MAX_PACKET_SIZE]u8 = [_][netcode.MAX_PACKET_SIZE]u8{[_]u8{0} ** netcode.MAX_PACKET_SIZE} ** MAX_OUTGOING_PACKETS,
    outgoing_packet_sequence: u64 = 0,

    pub fn clientActive(self: *LoopbackContext) bool {
        return self.client_active;
    }

    pub fn clientConnected(self: *LoopbackContext) bool {
        return self.client_connected;
    }

    pub fn sendPacket(self: *LoopbackContext, packet_data: []u8) void {
        const len = @max(packet_data.len, netcode.MAX_PACKET_SIZE);
        var new_packet_data = self.outgoing_packet_buffer[self.outgoing_packets.head][0..len];
        std.mem.copy(u8, new_packet_data, packet_data);
        self.outgoing_packets.write(Packet{
            .data = new_packet_data,
            .sequence = self.outgoing_packet_sequence,
        });
        self.outgoing_packet_sequence += 1;
    }

    pub fn receivePacket(self: *LoopbackContext) ?Packet {
        // Release previous read first, if needed
        // We invert the read and release so the library user doesn't need to call a "freePacket" sort of method
        // This approach is organic in a standard `while (ctx.receivePacket()) |packet|` loop
        self.incoming_packets.releasePreviousRead();
        return self.incoming_packets.readWithoutRelease();
    }

    fn connectClient(self: *LoopbackContext) void {
        self.client_connected = true;
    }

    fn disconnectClient(self: *LoopbackContext) void {
        self.client_connected = false;
    }

    fn sendPacketToServer(self: *LoopbackContext, packet_data: []u8, packet_sequence: u64) void {
        const len = @max(packet_data.len, netcode.MAX_PACKET_SIZE);
        var new_packet_data = self.incoming_packet_buffer[self.incoming_packets.head][0..len];
        std.mem.copy(u8, new_packet_data, packet_data);
        self.incoming_packets.write(Packet{
            .data = new_packet_data,
            .sequence = packet_sequence,
        });
    }

    fn processPacketsFromServer(self: *LoopbackContext, client: *netcode.Client) void {
        // netcode_process_loopback_packet will memcpy the packet, after which we free the memory
        while (self.outgoing_packets.readWithoutRelease()) |packet| {
            defer self.outgoing_packets.releasePreviousRead();
            client.processLoopbackPacket(packet.data, packet.sequence);
        }
    }

    fn sendLoopbackPacketCallback(context: ?*anyopaque, _: c_int, packet_data: [*c]u8, packet_bytes: c_int, packet_sequence: u64) callconv(.C) void {
        if (context) |ctx| {
            const loopback = @ptrCast(*LoopbackContext, @alignCast(@alignOf(*LoopbackContext), ctx));
            loopback.sendPacketToServer(packet_data[0..@intCast(usize, packet_bytes)], packet_sequence);
        }
    }
};

pub fn runLoopback(
    comptime Client: type,
    comptime Game: type,
    comptime client_table: ClientTable(Client, Game),
    client: *Client,
    comptime LoopbackServer: type,
    comptime loopback_server_table: LoopbackServerTable(LoopbackServer),
    loopback_server: *LoopbackServer,
    allocator: Allocator,
) !void {
    var loopback = LoopbackContext{};
    const server_thread = try std.Thread.spawn(.{}, loopback_server_table.run, .{ loopback_server, &loopback });
    defer server_thread.join();

    try runInternal(Client, Game, client_table, client, allocator, &loopback);

    loopback.client_active = false;
}

pub fn run(
    comptime Client: type,
    comptime Game: type,
    comptime client_table: ClientTable(Client, Game),
    client: *Client,
    allocator: Allocator,
) !void {
    try runInternal(Client, Game, client_table, client, allocator, null);
}

fn runInternal(
    comptime Client: type,
    comptime Game: type,
    comptime client_table: ClientTable(Client, Game),
    client: *Client,
    allocator: Allocator,
    loopback: ?*LoopbackContext,
) !void {
    // =========================================================================
    // NETCODE

    try netcode.init();
    defer netcode.term();

    var ip = [_:0]u8{ ':', ':' };
    var client_config = netcode.defaultClientConfig();
    client_config.callback_context = loopback orelse null;
    client_config.send_loopback_packet_callback = LoopbackContext.sendLoopbackPacketCallback;
    var netcode_client = netcode.Client.create(&ip, &client_config, 0) orelse return;
    defer netcode_client.destroy();

    // =========================================================================
    // ZAUDIO

    zaudio.init(allocator);
    defer zaudio.deinit();

    var audio_engine_config = zaudio.Engine.Config.init();
    const audio_engine = try zaudio.Engine.create(audio_engine_config);
    defer audio_engine.destroy();

    // =========================================================================

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

    const creation_state = try client_table.createWindow(client, display_buffer[0..display_count]);
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

    var window = try Window.create(creation_size, creation_state.title, creation_mon, null);
    defer window.destroy();

    var app: AppContext = .{
        .window = window,
        .audio_engine = audio_engine,
        .netcode_client = netcode_client,
        .loopback = loopback,
        .restored_pos = window.getPos() catch Point.zero,
        .restored_size = creation_state.restored_size,
        .was_maximized = creation_state.maximized,
    };

    if (creation_state.fullscreen_mode) |mode| {
        if (maybe_mon) |mon| {
            // Handle fake fullscreen manually
            if (os == .windows and !mode.is_exclusive) {
                try app.turnCompositedToFakeFullscreen(mon);
            }
        }
    }

    // This cast should be our only point of interface with zglfw
    var zwindow = @ptrCast(*@import("zglfw").Window, window);
    var graphics = try zgpu.GraphicsContext.create(allocator, zwindow);
    defer graphics.destroy(allocator);

    const h = graphics.swapchain_descriptor.height;
    const w = graphics.swapchain_descriptor.width;
    const swapchain_size = (@intCast(u64, w) << 32) | (@intCast(u64, h));

    const E = Engine(Game, client_table.game_table);
    var engine = E{
        .app = &app,
        .graphics = graphics,
        .swapchain_size = swapchain_size,
        .framebuffer_size = swapchain_size,
        .timer = try Timer.start(),
        .display_buffer = display_buffer,
        .display_count = display_count,
    };
    window.setUserPointer(E, &engine);

    window.setPosCallback(E, E.onPos);
    window.setSizeCallback(E, E.onSize);
    window.setFramebufferSizeCallback(E, E.onFramebufferSize);
    window.setCloseCallback(E, E.onClose);
    window.setFocusCallback(E, E.onFocus);
    window.setIconifyCallback(E, E.onIconify);
    window.setMaximizeCallback(E, E.onMaximize);
    window.setCursorEnterCallback(E, E.onCursorEnter);
    window.setKeyCallback(E, E.onKey);
    window.setCharCallback(E, E.onChar);
    window.setMouseButtonCallback(E, E.onMouseButton);
    window.setCursorPosCallback(E, E.onCursorPos);
    window.setScrollCallback(E, E.onScroll);

    // Manually set size here now that we've hooked the callback
    const fb = try window.getFramebufferSize();
    engine.framebuffer_size = (@intCast(u64, fb.w) << 32) | (@intCast(u64, fb.h));

    // =========================================================================
    // ZGUI

    if (client_table.use_imgui) {
        zgui.init(allocator);
        zgui.backend.init(
            zwindow,
            graphics.device,
            @enumToInt(zgpu.GraphicsContext.swapchain_format),
        );
    }
    defer if (client_table.use_imgui) {
        zgui.backend.deinit();
        zgui.deinit();
    };

    // =========================================================================

    var game = try client_table.init(client, &app, graphics);
    defer client_table.deinit(client, &app, game);

    try engine.eventLoop(game);
}

pub const AppContext = struct {
    const Self = @This();

    window: *Window,
    audio_engine: *zaudio.Engine,
    netcode_client: *netcode.Client,
    loopback: ?*LoopbackContext,

    // Cached state
    restored_pos: Point,
    restored_size: Size,
    was_maximized: bool = false,
    fake_fullscreen_on: if (os == .windows) bool else void = if (os == .windows) false else {},

    // Queued state
    should_close: bool = false,
    queued_mode_change: ModeChange = .{ .none = {} },

    pub fn connectToServer(self: *Self, connect_token: ?*[netcode.CONNECT_TOKEN_BYTES]u8) void {
        if (connect_token) |token| {
            self.netcode_client.connect(token);
        } else {
            self.netcode_client.connectLoopback(0, 1);
            if (self.loopback) |loopback| {
                loopback.connectClient();
            }
        }
    }

    pub fn disconnectFromServer(self: *Self) void {
        if (self.netcode_client.loopback()) {
            if (self.loopback) |loopback| {
                loopback.disconnectClient();
            }
            self.netcode_client.disconnectLoopback();
        } else {
            self.netcode_client.disconnect();
        }
    }

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

    fn dequeueModeChange(self: *Self) ModeChange {
        const mode_change = self.queued_mode_change;
        self.queued_mode_change = .{ .none = {} };
        return mode_change;
    }

    fn changeMode(self: *Self, mode_change: ModeChange) glfw.Error!void {
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

fn Engine(comptime Game: type, comptime table: GameTable(Game)) type {
    return struct {
        const Self = @This();

        app: *AppContext,
        graphics: *zgpu.GraphicsContext,
        swapchain_size: u64,
        framebuffer_size: u64,
        timer: Timer,
        world_timestamp: u64 = 0,
        game_lock: Mutex = .{},
        display_buffer: [DISPLAY_MAX]DisplayInfo,
        display_count: usize = 0,
        events: RingBuffer(64, EventArgs) = .{},

        const ticks_per_step = @floatToInt(usize, table.step_time * std.time.ns_per_s);
        const max_steps_per_update = @floatToInt(usize, table.max_step_delay / table.step_time);

        fn eventLoop(self: *Self, game: *Game) !void {
            self.timer.reset();

            var event_loop_broken = false;
            var render_loop_aborted = false;
            const render_thread = try std.Thread.spawn(.{}, renderLoop, .{ self, game, &event_loop_broken, &render_loop_aborted });
            defer {
                event_loop_broken = true;
                render_thread.join();
            }

            var joysticks: [glfw.JOYSTICK_COUNT]Joystick = [_]Joystick{.{}} ** glfw.JOYSTICK_COUNT;
            for (joysticks) |_, i| joysticks[i].init(i);

            // Swallow GLFW errors inside loop and rely on GLFW error callback for logging
            while (!self.app.should_close and !render_loop_aborted) {
                var displays = self.display_buffer[0..self.display_count];
                const display_state_changed = displaysChanged(&displays, &self.display_buffer) catch true;

                var joy_states_changed = [_]bool{false} ** glfw.JOYSTICK_COUNT;
                for (joysticks) |_, i| joy_states_changed[i] = joysticks[i].stateChanged() catch false;

                var mode_change: AppContext.ModeChange = undefined;

                {
                    self.game_lock.lock();
                    defer self.game_lock.unlock();

                    if (display_state_changed) {
                        table.receiveDisplayState(game, self.app, .{ .displays = displays });
                    }

                    for (joy_states_changed) |changed, i| {
                        if (changed) {
                            table.receiveJoyState(game, self.app, joysticks[i].getStateArgs());
                        }
                    }

                    self.update(game);

                    mode_change = self.app.dequeueModeChange();
                }

                self.app.changeMode(mode_change) catch {};
                glfw.waitEvents();
            }
        }

        fn renderLoop(self: *Self, game: *Game, event_loop_broken: *const bool, render_loop_aborted: *bool) !void {
            var timer = Timer.start() catch |err| {
                render_loop_aborted.* = true;
                return err;
            };
            var frame_tracker: FrameTracker(60, 240) = .{ .timer = timer };

            while (!event_loop_broken.*) {
                const graphics = self.graphics;
                const fb = @atomicLoad(u64, &self.framebuffer_size, .SeqCst);
                const fb_size: Size = .{ .w = @intCast(u31, (fb >> 32) & std.math.maxInt(u31)), .h = @intCast(u31, fb & std.math.maxInt(u31)) };
                const swapchain_resized = self.swapchain_size != fb;
                if (swapchain_resized) {
                    self.swapchain_size = fb;
                    graphics.swapchain_descriptor.width = @intCast(u32, fb_size.w);
                    graphics.swapchain_descriptor.height = @intCast(u32, fb_size.h);
                    graphics.swapchain.release();
                    graphics.swapchain = graphics.device.createSwapChain(graphics.surface, graphics.swapchain_descriptor);

                    frame_tracker.reset();
                }

                const estimated_fps = frame_tracker.estimateFps();

                zgui.backend.newFrame(
                    graphics.swapchain_descriptor.width,
                    graphics.swapchain_descriptor.height,
                );

                var texture_view = graphics.swapchain.getCurrentTextureView();
                defer texture_view.release();

                {
                    self.game_lock.lock();
                    defer self.game_lock.unlock();

                    if (swapchain_resized) {
                        table.onSwapchainResized(game, self.app, graphics, fb_size);
                    }

                    self.update(game);

                    const delta_ticks = self.timer.read() - self.world_timestamp;
                    const frame_info = FrameInfo{
                        .texture_view = texture_view,
                        .step_remainder = @intToFloat(f64, delta_ticks) / @intToFloat(f64, ticks_per_step),
                        .estimated_fps = estimated_fps,
                    };
                    table.draw(game, self.app, graphics, frame_info);
                }

                {
                    const gui_commands = commands: {
                        const encoder = graphics.device.createCommandEncoder(null);
                        defer encoder.release();

                        {
                            const color_attachments = [_]zgpu.wgpu.RenderPassColorAttachment{.{
                                .view = texture_view,
                                .load_op = .load,
                                .store_op = .store,
                            }};
                            const descriptor = zgpu.wgpu.RenderPassDescriptor{
                                .color_attachment_count = color_attachments.len,
                                .color_attachments = &color_attachments,
                            };
                            const pass = encoder.beginRenderPass(descriptor);
                            defer {
                                pass.end();
                                pass.release();
                            }

                            zgui.backend.draw(pass);
                        }

                        break :commands encoder.finish(null);
                    };
                    defer gui_commands.release();

                    graphics.submit(&.{gui_commands});
                }

                glfw.postEmptyEvent();
                graphics.swapchain.present();
                frame_tracker.startOrLap();
            }
        }

        fn update(self: *Self, game: *Game) void {
            while (self.events.read()) |event| {
                switch (event) {
                    .close_request => |args| table.receiveCloseRequest(game, self.app, args),
                    .focus_state => |args| table.receiveFocusState(game, self.app, args),
                    .iconify_state => |args| table.receiveIconifyState(game, self.app, args),
                    .key_action => |args| table.receiveKeyAction(game, self.app, args),
                    .char_input => |args| table.receiveCharInput(game, self.app, args),
                    .mouse_button_action => |args| table.receiveMouseButtonAction(game, self.app, args),
                    .mouse_scroll => |args| table.receiveMouseScroll(game, self.app, args),
                    .cursor_position => |args| table.receiveCursorPosition(game, self.app, args),
                    .cursor_entry_state => |args| table.receiveCursorEntryState(game, self.app, args),
                }
            }

            if (self.app.loopback) |loopback| {
                loopback.processPacketsFromServer(self.app.netcode_client);
            }
            var packet_sequence: u64 = undefined;
            while (self.app.netcode_client.receivePacket(&packet_sequence) catch null) |data| {
                table.receiveServerData(game, self.app, .{
                    .data = data,
                    .sequence = packet_sequence,
                });
                self.app.netcode_client.freePacket(data);
            }

            var timestamp = self.timer.read();
            var world_delta = timestamp - self.world_timestamp;
            if (world_delta < ticks_per_step) return;

            var steps = world_delta / ticks_per_step;
            if (steps > max_steps_per_update) {
                const steps_to_skip = max_steps_per_update - steps;
                table.skipSteps(game, self.app, steps_to_skip);
                steps -= steps_to_skip;
                self.world_timestamp += steps_to_skip * ticks_per_step;
            }

            var i: usize = 0;
            while (i < steps) : (i += 1) {
                table.step(game, self.app);
                self.world_timestamp += ticks_per_step;
            }
        }

        fn displaysChanged(displays: *[]DisplayInfo, buffer: *[DISPLAY_MAX]DisplayInfo) glfw.Error!bool {
            // Pre-emptively set display return to an empty slice in case of error.
            var old_displays = displays;
            displays.* = buffer[0..0];

            var state_changed = false;
            const monitors = try Monitor.getAll();
            const new_len = @min(monitors.len, DISPLAY_MAX);
            if (old_displays.len == new_len) {
                for (monitors[0..new_len]) |maybe_monitor, i| {
                    const monitor = maybe_monitor orelse return glfw.Error.Unknown;
                    const old_info = old_displays.*[i];
                    const new_info = try getDisplayInfo(monitor);
                    if (std.meta.eql(old_info, new_info)) {
                        buffer[i] = old_info;
                    } else {
                        buffer[i] = new_info;
                        state_changed = true;
                    }
                }
            } else {
                state_changed = true;
                for (monitors[0..new_len]) |maybe_monitor, i| {
                    const monitor = maybe_monitor orelse return glfw.Error.Unknown;
                    buffer[i] = try getDisplayInfo(monitor);
                }
            }

            displays.* = buffer[0..new_len];
            return state_changed;
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
            self.events.write(.{ .close_request = {} });
        }

        fn onFocus(self: *Self, focused: bool) void {
            self.events.write(.{ .focus_state = .{ .focused = focused } });
        }

        fn onIconify(self: *Self, iconified: bool) void {
            self.events.write(.{ .iconify_state = .{ .iconified = iconified } });
        }

        fn onMaximize(self: *Self, value: bool) void {
            self.app.onMaximize(value);
        }

        fn onKey(self: *Self, key: Key, scancode: i32, action: KeyAction, mods: KeyMods) void {
            self.events.write(.{ .key_action = .{
                .key = key,
                .scancode = scancode,
                .action = action,
                .mods = mods,
            } });
        }

        fn onChar(self: *Self, codepoint: u21) void {
            self.events.write(.{ .char_input = .{ .codepoint = codepoint } });
        }

        fn onMouseButton(self: *Self, button: MouseButton, action: MouseButtonAction, mods: KeyMods) void {
            self.events.write(.{ .mouse_button_action = .{
                .button = button,
                .action = action,
                .mods = mods,
            } });
        }

        fn onCursorPos(self: *Self, x: f64, y: f64) void {
            self.events.write(.{ .cursor_position = .{
                .x = x,
                .y = y,
            } });
        }

        fn onCursorEnter(self: *Self, entered: bool) void {
            self.events.write(.{ .cursor_entry_state = .{ .entered = entered } });
        }

        fn onScroll(self: *Self, x_offset: f64, y_offset: f64) void {
            self.events.write(.{ .mouse_scroll = .{
                .x_offset = x_offset,
                .y_offset = y_offset,
            } });
        }
    };
}

const Joystick = struct {
    id: usize = std.math.maxInt(usize),
    connected: bool = false,
    info: JoyInfo = .{},
    button_buffer: [JOY_BUTTON_MAX]JoyButtonState = [_]JoyButtonState{.released} ** JOY_BUTTON_MAX,
    axis_buffer: [JOY_AXIS_MAX]f32 = [_]f32{0.0} ** JOY_AXIS_MAX,
    hat_buffer: [JOY_HAT_MAX]JoyHatDirection = [_]JoyHatDirection{.centered} ** JOY_HAT_MAX,
    button_count: usize = 0,
    axis_count: usize = 0,
    hat_count: usize = 0,

    pub fn init(joystick: *Joystick, id: usize) void {
        joystick.id = id;
    }

    pub fn getStateArgs(joystick: Joystick) JoyStateArgs {
        return .{
            .id = joystick.id,
            .connected = joystick.connected,
            .info = joystick.info,
            .state = .{
                .buttons = joystick.button_buffer[0..joystick.button_count],
                .axes = joystick.axis_buffer[0..joystick.axis_count],
                .hats = joystick.hat_buffer[0..joystick.hat_count],
            },
        };
    }

    /// In addition to GLFW-reported errors, returns glfw.Error.Unknown if we can't read a detected joystick's state.
    pub fn stateChanged(joystick: *Joystick) glfw.Error!bool {
        const jid = @intCast(i32, joystick.id);
        if (!glfw.joystickPresent(jid)) {
            if (joystick.connected) {
                joystick.connected = false;
                joystick.button_count = 0;
                joystick.axis_count = 0;
                joystick.hat_count = 0;
                return true;
            } else {
                return false;
            }
        }

        var state_changed = false;

        // Read all state at once to not leave joystick object in an error state
        const guid = try glfw.getJoystickGuid(jid) orelse return glfw.Error.Unknown;
        const new_buttons = try glfw.getJoystickButtons(jid) orelse return glfw.Error.Unknown;
        const new_axes = try glfw.getJoystickAxes(jid) orelse return glfw.Error.Unknown;
        const new_hats = try glfw.getJoystickHats(jid) orelse return glfw.Error.Unknown;

        // On GLFW, only read VID and PID if this isn't an XInput controller
        var vid: ?u16 = null;
        var pid: ?u16 = null;
        const XINPUT_ID = "7869";
        const is_xinput = guid.len >= 4 and std.mem.eql(u8, guid[0..4], XINPUT_ID);
        if (!is_xinput) {
            if (guid.len >= 12) vid = stringToId(guid[8..12]);
            if (guid.len >= 20) pid = stringToId(guid[16..20]);
        }
        const info = JoyInfo{
            .vid = vid,
            .pid = pid,
            .is_xinput = is_xinput,
        };
        joystick.info = info;

        {
            const len = @min(JOY_BUTTON_MAX, new_buttons.len);
            if (joystick.button_count != len) {
                joystick.button_count = len;
                state_changed = true;
            }
            var i: usize = 0;
            while (i < len) : (i += 1) {
                const new_button: JoyButtonState = switch (new_buttons[i]) {
                    .release => .released,
                    .press => .pressed,
                    _ => .released,
                };
                if (joystick.button_buffer[i] != new_button) {
                    joystick.button_buffer[i] = new_button;
                    state_changed = true;
                }
            }
        }

        {
            const len = @min(JOY_AXIS_MAX, new_axes.len);
            if (joystick.axis_count != len) {
                joystick.axis_count = len;
                state_changed = true;
            }
            var i: usize = 0;
            while (i < len) : (i += 1) {
                const new_axis = new_axes[i];
                if (joystick.axis_buffer[i] != new_axis) {
                    joystick.axis_buffer[i] = new_axis;
                    state_changed = true;
                }
            }
        }

        {
            const len = @min(JOY_HAT_MAX, new_hats.len);
            if (joystick.hat_count != len) {
                joystick.hat_count = len;
                state_changed = true;
            }
            var i: usize = 0;
            while (i < len) : (i += 1) {
                const new_hat: JoyHatDirection = switch (new_hats[i]) {
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
                if (joystick.hat_buffer[i] != new_hat) {
                    joystick.hat_buffer[i] = new_hat;
                    state_changed = true;
                }
            }
        }

        return state_changed;
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
};

const EventKind = enum {
    close_request,
    focus_state,
    iconify_state,
    key_action,
    char_input,
    mouse_button_action,
    mouse_scroll,
    cursor_position,
    cursor_entry_state,
};

const EventArgs = union(EventKind) {
    close_request: CloseRequestArgs,
    focus_state: FocusStateArgs,
    iconify_state: IconifyStateArgs,
    key_action: KeyActionArgs,
    char_input: CharInputArgs,
    mouse_button_action: MouseButtonActionArgs,
    mouse_scroll: MouseScrollArgs,
    cursor_position: CursorPositionArgs,
    cursor_entry_state: CursorEntryStateArgs,
};

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
        .scale_factor = @min(scale.xscale, scale.yscale),
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

/// A simple pseudo ring buffer for tracking frame rate.
/// The type args are the minimum and maximum number of frames the tracker is allowed to use to estimate FPS.
/// Not thread-safe.
fn FrameTracker(comptime min_frames: u32, comptime max_frames: u32) type {
    if (min_frames > max_frames) @compileError("min_frames > max_frames");
    return struct {
        const Self = @This();

        timer: Timer,
        started: bool = false,
        head: usize = 0,
        size: usize = 0,
        buffer: [max_frames]u64 = [_]u64{undefined} ** max_frames,
        buffer_sorted: [max_frames]u64 = [_]u64{undefined} ** max_frames,

        /// Starts the tracker if needed, or laps and records the frame time. Must be called exactly once each frame, at the same point in the frame (e.g. right after swapping buffers).
        pub fn startOrLap(self: *Self) void {
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
            std.sort.sort(u64, self.buffer_sorted[0..s], {}, comptime std.sort.asc(u64));
        }

        /// Clear all recorded frame times from the buffer, starting fresh.
        pub fn reset(self: *Self) void {
            // No point in clearing buffers themselves.
            self.started = false;
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

/// Single-writer, single-reader buffer. Requires at least one dedicated (write-free) reader thread, but the reader can also run on any writer thread.
/// Lockless when under capacity.
fn RingBuffer(comptime size: usize, comptime T: type) type {
    if (size < 3) @compileError("Ring buffer must hold at least 3 items.");
    return struct {
        const Self = @This();

        items: [size]T = [_]T{undefined} ** size,
        head: usize = 0, // location of next write (exclusive end)
        tail: usize = 0, // location of next read (inclusive start)
        sync: usize = 0,
        read_unreleased: bool = false,
        read_completed: bool = false,
        mutex: std.Thread.Mutex = .{},
        condition: std.Thread.Condition = .{},

        pub fn write(self: *Self, item: T) void {
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

        pub fn read(self: *Self) ?T {
            const item = self.readWithoutRelease() orelse return null;
            self.releasePreviousRead();
            return item;
        }

        pub fn readWithoutRelease(self: *Self) ?T {
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

        pub fn releasePreviousRead(self: *Self) void {
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
