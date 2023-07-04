const builtin = @import("builtin");
const std = @import("std");
pub const options = @import("options");
pub const gpu = @import("wgpu");
pub const gui = if (options.use_imgui) @import("zgui") else {};
const netcode = @import("netcode");
const io = @import("io.zig");
const glfw = @import("glfw.zig");
const imgui = if (options.use_imgui) @import("imgui.zig") else {};
const utils = @import("utils.zig");

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

const Monitor = glfw.Monitor;
const Window = glfw.Window;
const FrameTracker = utils.FrameTracker;
const RingBuffer = utils.RingBuffer;

const os = builtin.os;

// ====================================================================================================================
// LOGGING & ERRORS

/// Enumeration of strongly-typed log scopes.
pub const LogScope = enum {
    glfw,
    webgpu,
    imgui,

    pub fn eql(comptime self: LogScope, comptime other: @Type(.EnumLiteral)) bool {
        return comptime std.mem.eql(u8, @tagName(self), @tagName(other));
    }
};

fn Logger(comptime scope: @Type(.EnumLiteral)) type {
    const name = @tagName(scope);
    if (!@hasField(LogScope, name)) {
        @compileError(name ++ " is not a valid log scope.");
    }
    return comptime std.log.scoped(scope);
}

pub const Error = error{
    NoWebGpuInstance,
    NoWebGpuAdapter,
    NoWebGpuDevice,
    ImGuiInitFailure,
};

// ====================================================================================================================
// GAME VTABLE

pub fn GameTable(comptime Game: type) type {
    return struct {
        fn Fn(comptime Args: type) type {
            return fn (*Game, *App, Args) void;
        }

        step: Fn(StepContext),
        draw: Fn(DrawContext),

        handleSkippedSteps: Fn(SkippedStepsContext),
        handleWindowOpen: Fn(WindowOpenContext),
        handleCloseRequest: Fn(CloseRequestContext),
        handleGpuDeviceCreation: Fn(GpuDeviceCreationContext),
        handleGpuDeviceLoss: Fn(GpuDeviceLossContext),
        handleSwapChainCreation: Fn(SwapChainCreationContext),
        handleSwapChainDestruction: Fn(SwapChainDestructionContext),

        receiveFocusState: Fn(FocusStateArgs) = ignore(FocusStateArgs),
        receiveIconifyState: Fn(IconifyStateArgs) = ignore(IconifyStateArgs),
        receiveKeyAction: Fn(KeyActionArgs) = ignore(KeyActionArgs),
        receiveCharInput: Fn(CharInputArgs) = ignore(CharInputArgs),
        receiveMouseButtonAction: Fn(MouseButtonActionArgs) = ignore(MouseButtonActionArgs),
        receiveMouseScroll: Fn(MouseScrollArgs) = ignore(MouseScrollArgs),
        receiveCursorPosition: Fn(CursorPositionArgs) = ignore(CursorPositionArgs),
        receiveCursorEntryState: Fn(CursorEntryStateArgs) = ignore(CursorEntryStateArgs),
        receiveJoyState: Fn(JoyStateArgs) = ignore(JoyStateArgs),
        receiveDisplayState: Fn(DisplayStateArgs) = ignore(DisplayStateArgs),
        receiveServerData: Fn(ServerDataArgs) = ignore(ServerDataArgs),

        fn ignore(comptime Args: type) Fn(Args) {
            return (struct {
                fn value(_: *Game, _: *App, _: Args) void {}
            }).value;
        }
    };
}

pub const StepContext = struct {};

pub const DrawContext = struct {
    device: gpu.Device,
    framebuffer_view: gpu.TextureView,
    framebuffer_format: gpu.TextureFormat = default_swap_chain_format,
    framebuffer_size: Size,
    step_remainder: u64,
    estimated_fps: ?f64 = null,

    /// This replaces a call to `gui.newFrame()`, wrapping the appropriate backend functionality.
    pub fn guiNewFrame(ctx: DrawContext) void {
        if (!options.use_imgui) return;

        const size = ctx.framebuffer_size;
        imgui.wgpu.newFrame();
        gui.io.setDisplaySize(@floatFromInt(size.w), @floatFromInt(size.h));
        gui.io.setDisplayFramebufferScale(1.0, 1.0);
        gui.newFrame();
    }

    /// This replaces a call to `gui.render()`, wrapping the appropriate backend functionality.
    pub fn guiRender(ctx: DrawContext, encoder: gpu.CommandEncoder) void {
        if (!options.use_imgui) return;

        const color_attachments = [_]gpu.RenderPassColorAttachment{.{
            .view = ctx.framebuffer_view,
            .load_op = .load,
            .store_op = .store,
        }};
        const descriptor = gpu.RenderPassDescriptor{
            .color_attachment_count = color_attachments.len,
            .color_attachments = &color_attachments,
        };
        const pass = encoder.beginRenderPass(descriptor);
        defer pass.release();

        gui.render();
        imgui.wgpu.renderDrawData(gui.getDrawData(), pass);
        pass.end();
    }
};

pub const SkippedStepsContext = struct {
    count: u64,
};

pub const WindowOpenContext = struct {
    displays: []const DisplayInfo,
};

pub const CloseRequestContext = struct {};

pub const GpuDeviceCreationContext = struct {
    device: gpu.Device,
    framebuffer_format: gpu.TextureFormat = default_swap_chain_format,
};

pub const GpuDeviceLossContext = struct {};

pub const SwapChainCreationContext = struct {
    device: gpu.Device,
    framebuffer_format: gpu.TextureFormat = default_swap_chain_format,
    framebuffer_size: Size,
};

pub const SwapChainDestructionContext = struct {
    device: gpu.Device,
};

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
    displays: []const DisplayInfo,
};

pub const ServerDataArgs = struct {
    data: []const u8,
    sequence: u64,
};

// ====================================================================================================================
// PUBLIC INTERFACE

pub const Config = struct {
    step_time: u64,
    max_step_delay: u64 = std.time.ns_per_s,
};

pub const WindowedMode = struct {
    restored_size: ?Size = null,
    maximized: ?bool = null,
};

pub const FullscreenMode = struct {
    monitor_location: ?Point = null,
    exclusive: ?bool = null,
};

pub const App = struct {
    netcode_client: *netcode.Client,
    loopback: ?*LoopbackContext,
    should_close: bool = false,
    window_changes: WindowChanges = .{},

    pub fn connectToServer(self: *@This(), connect_token: ?*[netcode.CONNECT_TOKEN_BYTES]u8) void {
        if (connect_token) |token| {
            self.netcode_client.connect(token);
        } else {
            self.netcode_client.connectLoopback(0, 1);
            if (self.loopback) |loopback| {
                loopback.connectClient();
            }
        }
    }

    pub fn disconnectFromServer(self: *@This()) void {
        if (self.netcode_client.loopback()) {
            if (self.loopback) |loopback| {
                loopback.disconnectClient();
            }
            self.netcode_client.disconnectLoopback();
        } else {
            self.netcode_client.disconnect();
        }
    }

    pub fn close(self: *@This()) void {
        self.should_close = true;
    }

    pub fn setTitle(self: *@This(), comptime title: [:0]const u8) void {
        self.window_changes.new_title = title;
    }

    pub fn setWindowed(self: *@This(), mode: WindowedMode) void {
        self.window_changes.mode_change = .{
            .kind = .set_windowed,
            .windowed = mode,
        };
    }

    pub fn setFullscreen(self: *@This(), mode: FullscreenMode) void {
        self.window_changes.mode_change = .{
            .kind = .set_fullscreen,
            .fullscreen = mode,
        };
    }

    pub fn toggleFullscreen(self: *@This(), windowed_mode: WindowedMode, fullscreen_mode: FullscreenMode) void {
        self.window_changes.mode_change = .{
            .kind = .toggle_fullscreen,
            .windowed = windowed_mode,
            .fullscreen = fullscreen_mode,
        };
    }

    fn dequeueWindowChanges(self: *@This()) WindowChanges {
        const window_changes = self.window_changes;
        self.window_changes = .{};
        return window_changes;
    }
};

pub const LoopbackPacket = struct {
    data: []u8,
    sequence: u64,
};

pub const LoopbackContext = struct {
    const MAX_INCOMING_PACKETS = 64;
    const MAX_OUTGOING_PACKETS = 64;

    client_active: bool = true,
    client_connected: bool = false,
    incoming_packets: RingBuffer(MAX_INCOMING_PACKETS, LoopbackPacket) = .{},
    incoming_packet_buffer: [MAX_INCOMING_PACKETS][netcode.MAX_PACKET_SIZE]u8 =
        [_][netcode.MAX_PACKET_SIZE]u8{[_]u8{0} ** netcode.MAX_PACKET_SIZE} ** MAX_INCOMING_PACKETS,
    outgoing_packets: RingBuffer(MAX_OUTGOING_PACKETS, LoopbackPacket) = .{},
    outgoing_packet_buffer: [MAX_OUTGOING_PACKETS][netcode.MAX_PACKET_SIZE]u8 =
        [_][netcode.MAX_PACKET_SIZE]u8{[_]u8{0} ** netcode.MAX_PACKET_SIZE} ** MAX_OUTGOING_PACKETS,
    outgoing_packet_sequence: u64 = 0,

    pub fn clientActive(self: *@This()) bool {
        return self.client_active;
    }

    pub fn clientConnected(self: *@This()) bool {
        return self.client_connected;
    }

    pub fn sendPacket(self: *@This(), packet_data: []u8) void {
        const len = @max(packet_data.len, netcode.MAX_PACKET_SIZE);
        var new_packet_data = self.outgoing_packet_buffer[self.outgoing_packets.head][0..len];
        std.mem.copy(u8, new_packet_data, packet_data);
        self.outgoing_packets.write(LoopbackPacket{
            .data = new_packet_data,
            .sequence = self.outgoing_packet_sequence,
        });
        self.outgoing_packet_sequence += 1;
    }

    pub fn receivePacket(self: *@This()) ?LoopbackPacket {
        // Release previous read first, if needed
        // We invert the read and release so the library user doesn't need to call a "freePacket" sort of method
        // This approach is organic in a standard `while (ctx.receivePacket()) |packet|` loop
        self.incoming_packets.releasePreviousRead();
        return self.incoming_packets.readWithoutRelease();
    }

    fn connectClient(self: *@This()) void {
        self.client_connected = true;
    }

    fn disconnectClient(self: *@This()) void {
        self.client_connected = false;
    }

    fn sendPacketToServer(self: *@This(), packet_data: []u8, packet_sequence: u64) void {
        const len = @max(packet_data.len, netcode.MAX_PACKET_SIZE);
        var new_packet_data = self.incoming_packet_buffer[self.incoming_packets.head][0..len];
        std.mem.copy(u8, new_packet_data, packet_data);
        self.incoming_packets.write(LoopbackPacket{
            .data = new_packet_data,
            .sequence = packet_sequence,
        });
    }

    fn processPacketsFromServer(self: *@This(), client: *netcode.Client) void {
        // netcode_process_loopback_packet will memcpy the packet, after which we free the memory
        while (self.outgoing_packets.readWithoutRelease()) |packet| {
            defer self.outgoing_packets.releasePreviousRead();
            client.processLoopbackPacket(packet.data, packet.sequence);
        }
    }

    fn sendLoopbackPacketCallback(context: ?*anyopaque, _: c_int, packet_data: [*c]u8, packet_bytes: c_int, packet_sequence: u64) callconv(.C) void {
        if (context) |ctx| {
            const loopback: *@This() = @ptrCast(@alignCast(ctx));
            loopback.sendPacketToServer(packet_data[0..@intCast(packet_bytes)], packet_sequence);
        }
    }
};

pub fn runLoopback(
    comptime Game: type,
    comptime game_table: GameTable(Game),
    comptime config: Config,
    game: *Game,
    comptime LoopbackServer: type,
    comptime loopback_server_run: fn (*LoopbackServer, *LoopbackContext) void,
    loopback_server: *LoopbackServer,
    allocator: std.mem.Allocator,
) !void {
    var loopback = LoopbackContext{};
    const server_thread = try std.Thread.spawn(.{}, loopback_server_run, .{ loopback_server, &loopback });
    defer server_thread.join();

    try runInternal(Game, game_table, config, game, allocator, &loopback);

    loopback.client_active = false;
}

pub fn run(
    comptime Game: type,
    comptime game_table: GameTable(Game),
    comptime config: Config,
    game: *Game,
    allocator: std.mem.Allocator,
) !void {
    try runInternal(Game, game_table, config, game, allocator, null);
}

// ====================================================================================================================
// INTERNAL CORE

fn runInternal(
    comptime Game: type,
    comptime game_table: GameTable(Game),
    comptime config: Config,
    game: *Game,
    allocator: std.mem.Allocator,
    loopback: ?*LoopbackContext,
) !void {
    try netcode.init();
    defer netcode.term();

    var ip = [_:0]u8{ ':', ':' };
    var client_config = netcode.defaultClientConfig();
    client_config.callback_context = loopback;
    client_config.send_loopback_packet_callback = LoopbackContext.sendLoopbackPacketCallback;
    const netcode_client = netcode.Client.create(&ip, &client_config, 0) orelse return;
    defer netcode_client.destroy();

    // =========================================================================

    var app = App{
        .netcode_client = netcode_client,
        .loopback = loopback,
    };

    if (options.use_imgui) gui.init(allocator);
    defer if (options.use_imgui) gui.deinit();

    logGlfwVersion();
    glfw.setErrorCallback(onGlfwError);

    try glfw.init();
    defer glfw.terminate();

    var displays = try DisplayArray.create();
    game_table.handleWindowOpen(game, &app, .{ .displays = displays.slice() });
    const window_changes = app.dequeueWindowChanges();
    const title = window_changes.new_title orelse "auxo game";
    const maximized = window_changes.mode_change.windowed.maximized orelse false;
    const restored_size = window_changes.mode_change.windowed.restored_size orelse Size{ .w = 640, .h = 360 };

    glfw.hintWindowVisible(false);
    glfw.hintWindowMaximized(maximized);
    var window = try Window.create(restored_size, title, null, null);
    defer window.destroy();

    var framebuffer_size = AtomicSize.create(try window.getFramebufferSize());
    var event_buffer = EventBuffer{};
    var window_wrapper = WindowWrapper{
        .window = window,
        .framebuffer_size = &framebuffer_size,
        .event_writer = .{ .buffer = &event_buffer },
        .restored_pos = window.getPos() catch Point.zero,
        .restored_size = restored_size,
        .was_maximized = maximized,
    };
    try window_wrapper.init(window_changes.mode_change);

    // Hook ImGui callbacks in-between hooking Auxo callbacks and showing window
    if (options.use_imgui) {
        if (imgui.glfw.initForOther(window, true)) {
            Logger(.imgui).info("ImGui GLFW backend initialization successful.", .{});
        } else {
            Logger(.imgui).err("ImGui GLFW backend initialization failed.", .{});
            return Error.ImGuiInitFailure;
        }
    }
    defer if (options.use_imgui) imgui.glfw.shutdown();

    window.show();

    // =========================================================================

    dawnProcSetProcs(dnGetProcs());
    const native_instance = dniCreate();
    defer dniDestroy(native_instance);

    dniDiscoverDefaultAdapters(native_instance);
    const instance = dniGetWgpuInstance(native_instance) orelse return Error.NoWebGpuInstance;
    defer instance.release();

    const surface = try createSurface(instance, window);
    defer surface.release();

    // =========================================================================

    var engine = Engine(Game, game_table){
        .config = config,
        .game = game,
        .app = &app,
        .world_timer = try std.time.Timer.start(),
        .event_reader = .{ .buffer = &event_buffer },
    };
    var event_loop_state = .{
        .window = &window_wrapper,
        .displays = &displays,
    };
    var render_loop_state = .{
        .instance = instance,
        .surface = surface,
        .framebuffer_size = &framebuffer_size,
    };
    try engine.eventLoop(&event_loop_state, &render_loop_state);
}

// TODO: optionize event buffer capacity
const EventBuffer = RingBuffer(64, EventArgs);

const EventWriter = struct {
    buffer: *EventBuffer,

    pub fn write(self: EventWriter, args: EventArgs) void {
        self.buffer.write(args);
    }
};

const EventReader = struct {
    buffer: *EventBuffer,

    pub fn read(self: EventReader) ?EventArgs {
        return self.buffer.read();
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
    close_request: CloseRequestContext,
    focus_state: FocusStateArgs,
    iconify_state: IconifyStateArgs,
    key_action: KeyActionArgs,
    char_input: CharInputArgs,
    mouse_button_action: MouseButtonActionArgs,
    mouse_scroll: MouseScrollArgs,
    cursor_position: CursorPositionArgs,
    cursor_entry_state: CursorEntryStateArgs,
};

const AtomicSize = struct {
    wh: u64 = 0,

    pub fn create(size: Size) AtomicSize {
        return .{ .wh = pack(size) };
    }

    pub fn store(self: *AtomicSize, size: Size) void {
        @atomicStore(u64, &self.wh, pack(size), .SeqCst);
    }

    pub fn load(self: *AtomicSize) Size {
        return unpack(@atomicLoad(u64, &self.wh, .SeqCst));
    }

    fn pack(size: Size) u64 {
        const w = @as(u64, @intCast(size.w)) << 32;
        const h = size.h;
        return w | h;
    }

    fn unpack(wh: u64) Size {
        return .{
            .w = @intCast(wh >> 32),
            .h = @intCast(wh & std.math.maxInt(u31)),
        };
    }
};

const ModeChange = struct {
    kind: enum {
        none,
        set_windowed,
        set_fullscreen,
        toggle_fullscreen,
    },
    windowed: WindowedMode = .{},
    fullscreen: FullscreenMode = .{},
};

const WindowChanges = struct {
    new_title: ?[:0]const u8 = null,
    mode_change: ModeChange = .{ .kind = .none },
};

const WindowWrapper = struct {
    window: *Window,
    event_writer: EventWriter,
    framebuffer_size: *AtomicSize,
    restored_pos: Point,
    restored_size: Size,
    was_maximized: bool,
    fake_fullscreen_on: if (os.tag == .windows) bool else void = if (os.tag == .windows) false else {},

    pub fn init(self: *@This(), initial_mode: ModeChange) !void {
        try self.changeMode(initial_mode);

        var win = self.window;
        const W = @This();
        win.setUserPointer(W, self);
        win.setPosCallback(W, onPos);
        win.setSizeCallback(W, onSize);
        win.setFramebufferSizeCallback(W, onFramebufferSize);
        win.setCloseCallback(W, onClose);
        win.setFocusCallback(W, onFocus);
        win.setIconifyCallback(W, onIconify);
        win.setMaximizeCallback(W, onMaximize);
        win.setCursorEnterCallback(W, onCursorEnter);
        win.setKeyCallback(W, onKey);
        win.setCharCallback(W, onChar);
        win.setMouseButtonCallback(W, onMouseButton);
        win.setCursorPosCallback(W, onCursorPos);
        win.setScrollCallback(W, onScroll);

        // Now that we've hooked the framebuffer callback, we can init this cleanly
        const size = try self.window.getFramebufferSize();
        self.framebuffer_size.store(size);
    }

    pub fn processChanges(self: *@This(), changes: WindowChanges) !void {
        if (changes.new_title) |title| {
            self.window.setTitle(title);
        }

        try self.changeMode(changes.mode_change);
    }

    fn onPos(self: *@This(), pos: Point) void {
        if (self.shouldCacheRestoredBounds()) {
            self.restored_pos = pos;
        }
    }

    fn onSize(self: *@This(), size: Size) void {
        if (size.w > 0 and size.h > 0 and self.shouldCacheRestoredBounds()) {
            self.restored_size = size;
        }
    }

    fn onFramebufferSize(self: *@This(), size: Size) void {
        self.framebuffer_size.store(size);
    }

    fn onClose(self: *@This()) void {
        self.event_writer.write(.{ .close_request = .{} });
    }

    fn onFocus(self: *@This(), focused: bool) void {
        self.event_writer.write(.{ .focus_state = .{ .focused = focused } });
    }

    fn onIconify(self: *@This(), iconified: bool) void {
        self.event_writer.write(.{ .iconify_state = .{ .iconified = iconified } });
    }

    fn onMaximize(self: *@This(), value: bool) void {
        if (os.tag != .windows or !self.fake_fullscreen_on) {
            self.was_maximized = value;
        }
    }

    fn onKey(self: *@This(), key: Key, scancode: i32, action: KeyAction, mods: KeyMods) void {
        self.event_writer.write(.{ .key_action = .{
            .key = key,
            .scancode = scancode,
            .action = action,
            .mods = mods,
        } });
    }

    fn onChar(self: *@This(), codepoint: u21) void {
        self.event_writer.write(.{ .char_input = .{ .codepoint = codepoint } });
    }

    fn onMouseButton(self: *@This(), button: MouseButton, action: MouseButtonAction, mods: KeyMods) void {
        self.event_writer.write(.{ .mouse_button_action = .{
            .button = button,
            .action = action,
            .mods = mods,
        } });
    }

    fn onCursorPos(self: *@This(), x: f64, y: f64) void {
        self.event_writer.write(.{ .cursor_position = .{
            .x = x,
            .y = y,
        } });
    }

    fn onCursorEnter(self: *@This(), entered: bool) void {
        self.event_writer.write(.{ .cursor_entry_state = .{ .entered = entered } });
    }

    fn onScroll(self: *@This(), x_offset: f64, y_offset: f64) void {
        self.event_writer.write(.{ .mouse_scroll = .{
            .x_offset = x_offset,
            .y_offset = y_offset,
        } });
    }

    fn shouldCacheRestoredBounds(self: @This()) bool {
        // Don't cache if fake fullscreen
        if (os.tag == .windows and self.fake_fullscreen_on) return false;

        // Don't cache if real fullscreen
        if (self.window.getFullscreenMonitor()) |_| return false;

        // Don't cache if iconified
        if (self.window.isIconified()) return false;

        // Don't cache if maximized
        if (self.window.isMaximized()) return false;

        return true;
    }

    fn changeMode(self: *@This(), mode_change: ModeChange) glfw.Error!void {
        if (self.window.getFullscreenMonitor()) |_| {
            try self.changeFromRealFullscreen(mode_change);
        } else if (os.tag == .windows and self.fake_fullscreen_on) {
            try self.changeFromFakeFullscreen(mode_change);
        } else {
            try self.changeFromWindowed(mode_change);
        }
    }

    fn changeFromWindowed(self: *@This(), mode_change: ModeChange) !void {
        switch (mode_change.kind) {
            .none => return,
            .set_windowed => {
                const mode = mode_change.windowed;
                // 1) Are we toggling maximized state? Then resolve here.
                if (mode.maximized) |maximize| {
                    if (self.was_maximized and !maximize) {
                        self.window.restore();
                        if (mode.restored_size) |size| self.window.setSize(size);
                        return;
                    } else if (!self.was_maximized and maximize) {
                        if (mode.restored_size) |size| self.window.setSize(size);
                        self.window.maximize();
                        return;
                    } // else fallthrough
                }
                // 2) Otherwise, are we changing restored size? Then resolve here.
                if (mode.restored_size) |size| {
                    if (self.was_maximized) {
                        // Temporarily restore window just to set new size (silly, but a corner case)
                        self.window.restore();
                        self.window.setSize(size);
                        self.window.maximize();
                    } else {
                        self.window.setSize(size);
                    }
                }
            },
            .set_fullscreen, .toggle_fullscreen => {
                const mode = mode_change.fullscreen;
                const monitor = try findMonitorMatch(mode.monitor_location, self.window);
                const exclusive = mode.exclusive orelse false;
                if (os.tag == .windows and !exclusive) {
                    try self.turnCompositedToFakeFullscreen(monitor);
                } else {
                    const vm = try monitor.getVideoMode();
                    self.window.setFullscreen(monitor, Size.abs(vm.width, vm.height), vm.refreshRate);
                }
            },
        }
    }

    fn changeFromRealFullscreen(self: *@This(), mode_change: ModeChange) !void {
        switch (mode_change.kind) {
            .none => return,
            .set_windowed, .toggle_fullscreen => {
                const mode = mode_change.windowed;
                if (mode.restored_size) |size| self.restored_size = size;
                if (mode.maximized) |maximize| self.was_maximized = maximize;
                self.turnRealFullscreenToWindowed();
            },
            .set_fullscreen => {
                const mode = mode_change.fullscreen;
                const monitor = try findMonitorMatch(mode.monitor_location, self.window);
                const exclusive = mode.exclusive orelse false;
                if (os.tag == .windows and !exclusive) {
                    self.turnRealFullscreenToWindowed();
                    try self.turnCompositedToFakeFullscreen(monitor);
                } else {
                    const vm = try monitor.getVideoMode();
                    self.window.setFullscreen(monitor, Size.abs(vm.width, vm.height), vm.refreshRate);
                }
            },
        }
    }

    fn changeFromFakeFullscreen(self: *@This(), mode_change: ModeChange) !void {
        switch (mode_change.kind) {
            .none => return,
            .set_windowed, .toggle_fullscreen => {
                const mode = mode_change.windowed;
                if (mode.restored_size) |size| self.restored_size = size;
                if (mode.maximized) |maximize| self.was_maximized = maximize;
                self.turnFakeFullscreenToWindowed();
            },
            .set_fullscreen => {
                const mode = mode_change.fullscreen;
                const monitor = try findMonitorMatch(mode.monitor_location, self.window);
                const exclusive = mode.exclusive orelse false;
                if (os.tag == .windows and !exclusive) {
                    try self.turnCompositedToFakeFullscreen(monitor);
                } else {
                    self.turnFakeFullscreenToWindowed();
                    const vm = try monitor.getVideoMode();
                    self.window.setFullscreen(monitor, Size.abs(vm.width, vm.height), vm.refreshRate);
                }
            },
        }
    }

    fn turnCompositedToFakeFullscreen(self: *@This(), monitor: *Monitor) !void {
        const pos = try monitor.getPos();
        const vm = try monitor.getVideoMode();
        const size = Size.abs(vm.width, vm.height);

        // Return if we're already fake fullscreen on this monitor, so the window doesn't flicker.
        if (self.isFakeFullscreenWithBounds(pos, size)) return;

        // We must first restore the window to deal with two cases:
        // 1) Win32 can't properly mutate a maximized window (this could later lead to a maximized window that doesn't cover the screen).
        // 2) We can't re-dock a "pane" (an unrestored, unmaximized window docked on the side of a monitor), so we don't want to cache its docked pos or size.
        // To properly deal with the first case, we have to preserve the window's maximized state.
        self.restorePreservingMaximizedState();

        self.fake_fullscreen_on = true;
        self.window.hide();
        self.window.setDecorated(false);
        self.window.setPos(pos);
        self.window.setSize(size);
        self.window.show();
    }

    fn turnFakeFullscreenToWindowed(self: *@This()) void {
        self.window.hide();
        self.window.setSize(self.restored_size);
        self.window.setPos(self.restored_pos);
        self.window.setDecorated(true);
        self.window.show();
        self.fake_fullscreen_on = false;

        self.restorePreservingMaximizedState();
        if (self.was_maximized) self.window.maximize();
    }

    fn turnRealFullscreenToWindowed(self: *@This()) void {
        self.window.setWindowed(self.restored_pos, self.restored_size);

        self.restorePreservingMaximizedState();
        if (self.was_maximized) self.window.maximize();
    }

    fn isFakeFullscreenWithBounds(self: *@This(), pos: Point, size: Size) bool {
        if (!self.fake_fullscreen_on) return false;

        const wpos = self.window.getPos() catch return false;
        if (pos.x != wpos.x or pos.y != wpos.y) return false;

        const wsize = self.window.getSize() catch return false;
        if (size.w != wsize.w or size.h != wsize.h) return false;

        return true;
    }

    fn restorePreservingMaximizedState(self: *@This()) void {
        const wm = self.was_maximized;
        self.window.restore();
        self.was_maximized = wm;
    }
};

const EventLoopState = struct {
    window: *WindowWrapper,
    displays: *DisplayArray,
};

const RenderLoopState = struct {
    instance: gpu.Instance,
    surface: gpu.Surface,
    framebuffer_size: *AtomicSize,
};

fn Engine(comptime Game: type, comptime table: GameTable(Game)) type {
    return struct {
        config: Config,
        game: *Game,
        app: *App,
        event_reader: EventReader,
        world_timer: std.time.Timer,
        world_timestamp: u64 = 0,
        game_lock: std.Thread.Mutex = .{},

        fn eventLoop(self: *@This(), state: *EventLoopState, render_loop_state: *RenderLoopState) !void {
            const game = self.game;
            const app = self.app;

            var event_loop_broken = false;
            var render_loop_aborted = false;
            const render_thread = try std.Thread.spawn(.{}, renderLoop, .{
                self,
                render_loop_state,
                &event_loop_broken,
                &render_loop_aborted,
            });
            defer {
                event_loop_broken = true;
                render_thread.join();
            }

            var joysticks: [glfw.JOYSTICK_COUNT]Joystick = [_]Joystick{.{}} ** glfw.JOYSTICK_COUNT;
            for (joysticks, 0..) |_, i| joysticks[i].init(i);

            // Swallow GLFW errors inside loop and rely on GLFW error callback for logging
            while (!app.should_close and !render_loop_aborted) {
                // We call this here because it's thread-safer to call it on the event thread than the render thread.
                // There doesn't currently seem to be an issue with this approach (which the code bears out).
                if (options.use_imgui) imgui.glfw.newFrame();

                const display_state_changed = state.displays.stateChanged() catch true;

                var joy_states_changed = [_]bool{false} ** glfw.JOYSTICK_COUNT;
                for (joysticks, 0..) |_, i| joy_states_changed[i] = joysticks[i].stateChanged() catch false;

                var window_changes: WindowChanges = undefined;

                {
                    self.game_lock.lock();
                    defer self.game_lock.unlock();

                    if (display_state_changed) {
                        table.receiveDisplayState(game, app, .{ .displays = state.displays.slice() });
                    }

                    for (joy_states_changed, 0..) |changed, i| {
                        if (changed) {
                            table.receiveJoyState(game, app, joysticks[i].getStateArgs());
                        }
                    }

                    self.update();
                    window_changes = app.dequeueWindowChanges();
                }

                state.window.processChanges(window_changes) catch {};
                glfw.waitEventsTimeout(0.5);
            }
        }

        fn renderLoop(self: *@This(), state: *RenderLoopState, event_loop_broken: *const bool, render_loop_aborted: *bool) !void {
            self.renderLoopInner(state, event_loop_broken) catch |err| {
                render_loop_aborted.* = true;
                return err;
            };
        }

        fn renderLoopInner(self: *@This(), state: *RenderLoopState, event_loop_broken: *const bool) !void {
            const game = self.game;
            const app = self.app;

            var frame_tracker: FrameTracker(60, 240) = .{ .timer = try std.time.Timer.start() };

            var device_lost = false;
            var device = try requestDevice(state.instance, &device_lost);
            var device_created = true;
            defer releaseDevice(device);

            var old_size = state.framebuffer_size.load();
            var swap_chain = createSwapChain(device, state.surface, old_size.w, old_size.h);
            var swap_chain_created = true;
            defer swap_chain.release();

            while (!event_loop_broken.*) {
                // 1) Report device loss to user (aborting if they signal so), then re-create it.
                const device_changed = device_lost;
                if (device_lost) {
                    releaseDevice(device);
                    {
                        self.game_lock.lock();
                        defer self.game_lock.unlock();

                        // Run an update first since this is an exceptional event and may take time to resolve.
                        self.update();
                        table.handleGpuDeviceLoss(game, app, .{});
                        if (app.should_close) {
                            return error.Unknown;
                        }
                    }
                    device_lost = false;
                    device = try requestDevice(state.instance, &device_lost);
                    device_created = true;
                }

                // 2) Report device (re-)creation to user.
                if (device_created) {
                    self.game_lock.lock();
                    defer self.game_lock.unlock();

                    // Run an update first since this is an exceptional event and may take time to resolve.
                    self.update();
                    table.handleGpuDeviceCreation(game, app, .{ .device = device });
                    device_created = false;
                }

                // 3) If any relevant state has changed, report swap-chain destruction to user, then re-create it.
                const size = state.framebuffer_size.load();
                const size_changed = size.w != old_size.w or size.h != old_size.h;
                if (size.w == 0 and size.h == 0) continue;
                old_size = size;
                if (device_changed or size_changed) {
                    frame_tracker.reset();
                    swap_chain.release();
                    {
                        self.game_lock.lock();
                        defer self.game_lock.unlock();

                        table.handleSwapChainDestruction(game, app, .{ .device = device });
                    }
                    swap_chain = createSwapChain(device, state.surface, size.w, size.h);
                    swap_chain_created = true;
                }
                const estimated_fps = frame_tracker.estimateFps();
                var view = swap_chain.getCurrentTextureView();
                defer view.release();

                // 4) Report swap-chain (re-)creation to user.
                if (swap_chain_created) {
                    self.game_lock.lock();
                    defer self.game_lock.unlock();

                    table.handleSwapChainCreation(game, app, .{ .device = device, .framebuffer_size = size });
                    swap_chain_created = false;
                }

                // 5) Update and draw.
                {
                    self.game_lock.lock();
                    defer self.game_lock.unlock();

                    self.update();
                    var world_delta = self.world_timer.read() - self.world_timestamp;
                    table.draw(game, app, .{
                        .device = device,
                        .framebuffer_view = view,
                        .framebuffer_size = size,
                        .step_remainder = world_delta,
                        .estimated_fps = estimated_fps,
                    });
                }

                // 6) Advance the event loop and present.
                glfw.postEmptyEvent();
                swap_chain.present();
                frame_tracker.startOrLap();
            }
        }

        fn update(self: *@This()) void {
            var game = self.game;
            var app = self.app;

            while (self.event_reader.read()) |event| {
                switch (event) {
                    .close_request => |args| table.handleCloseRequest(game, app, args),
                    .focus_state => |args| table.receiveFocusState(game, app, args),
                    .iconify_state => |args| table.receiveIconifyState(game, app, args),
                    .key_action => |args| table.receiveKeyAction(game, app, args),
                    .char_input => |args| table.receiveCharInput(game, app, args),
                    .mouse_button_action => |args| table.receiveMouseButtonAction(game, app, args),
                    .mouse_scroll => |args| table.receiveMouseScroll(game, app, args),
                    .cursor_position => |args| table.receiveCursorPosition(game, app, args),
                    .cursor_entry_state => |args| table.receiveCursorEntryState(game, app, args),
                }
            }

            if (app.loopback) |loopback| {
                loopback.processPacketsFromServer(app.netcode_client);
            }
            var packet_sequence: u64 = undefined;
            while (app.netcode_client.receivePacket(&packet_sequence) catch null) |data| {
                table.receiveServerData(game, app, .{
                    .data = data,
                    .sequence = packet_sequence,
                });
                app.netcode_client.freePacket(data);
            }

            const step_time = self.config.step_time;
            const max_steps = self.config.max_step_delay / step_time;
            const world_delta = self.world_timer.read() - self.world_timestamp;
            var steps = world_delta / step_time;
            if (steps > max_steps) {
                const skipped_steps = steps - max_steps;
                table.handleSkippedSteps(game, app, .{ .count = skipped_steps });
                steps -= skipped_steps;
                self.world_timestamp += skipped_steps * step_time;
            }

            var i: usize = 0;
            while (i < steps) : (i += 1) {
                table.step(game, app, .{});
                self.world_timestamp += step_time;
            }
        }
    };
}

const DisplayArray = struct {
    buffer: [DISPLAY_MAX]DisplayInfo,
    count: usize,

    pub fn create() !@This() {
        const monitors = try Monitor.getAll();
        var buffer: [DISPLAY_MAX]DisplayInfo = undefined;
        var count: usize = @min(monitors.len, DISPLAY_MAX);
        for (monitors, 0..count) |monitor, i| {
            buffer[i] = try getDisplayInfo(monitor.?);
        }
        return .{ .buffer = buffer, .count = count };
    }

    pub fn slice(self: @This()) []const DisplayInfo {
        return self.buffer[0..self.count];
    }

    pub fn stateChanged(self: *@This()) glfw.Error!bool {
        // Pre-emptively set displays to an empty slice in case of error.
        const old_count = self.count;
        self.count = 0;

        var state_changed = false;
        const monitors = try Monitor.getAll();
        const new_count = @min(monitors.len, DISPLAY_MAX);
        if (old_count == new_count) {
            for (monitors, 0..new_count) |maybe_monitor, i| {
                const monitor = maybe_monitor orelse return glfw.Error.Unknown;
                const old_info = self.buffer[i];
                const new_info = try getDisplayInfo(monitor);
                if (!std.meta.eql(old_info, new_info)) {
                    state_changed = true;
                    self.buffer[i] = new_info;
                }
            }
        } else {
            state_changed = true;
            for (monitors, 0..new_count) |maybe_monitor, i| {
                const monitor = maybe_monitor orelse return glfw.Error.Unknown;
                self.buffer[i] = try getDisplayInfo(monitor);
            }
        }

        self.count = new_count;
        return state_changed;
    }
};

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

    pub fn init(self: *@This(), id: usize) void {
        self.id = id;
    }

    pub fn getStateArgs(self: @This()) JoyStateArgs {
        return .{
            .id = self.id,
            .connected = self.connected,
            .info = self.info,
            .state = .{
                .buttons = self.button_buffer[0..self.button_count],
                .axes = self.axis_buffer[0..self.axis_count],
                .hats = self.hat_buffer[0..self.hat_count],
            },
        };
    }

    /// In addition to GLFW-reported errors, returns glfw.Error.Unknown if we can't read a detected joystick's state.
    pub fn stateChanged(self: *@This()) glfw.Error!bool {
        const jid: i32 = @intCast(self.id);
        if (!glfw.joystickPresent(jid)) {
            if (self.connected) {
                self.connected = false;
                self.button_count = 0;
                self.axis_count = 0;
                self.hat_count = 0;
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
        self.info = info;

        {
            const len = @min(JOY_BUTTON_MAX, new_buttons.len);
            if (self.button_count != len) {
                self.button_count = len;
                state_changed = true;
            }
            var i: usize = 0;
            while (i < len) : (i += 1) {
                const new_button: JoyButtonState = switch (new_buttons[i]) {
                    .release => .released,
                    .press => .pressed,
                    _ => .released,
                };
                if (self.button_buffer[i] != new_button) {
                    self.button_buffer[i] = new_button;
                    state_changed = true;
                }
            }
        }

        {
            const len = @min(JOY_AXIS_MAX, new_axes.len);
            if (self.axis_count != len) {
                self.axis_count = len;
                state_changed = true;
            }
            var i: usize = 0;
            while (i < len) : (i += 1) {
                const new_axis = new_axes[i];
                if (self.axis_buffer[i] != new_axis) {
                    self.axis_buffer[i] = new_axis;
                    state_changed = true;
                }
            }
        }

        {
            const len = @min(JOY_HAT_MAX, new_hats.len);
            if (self.hat_count != len) {
                self.hat_count = len;
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
                if (self.hat_buffer[i] != new_hat) {
                    self.hat_buffer[i] = new_hat;
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

// ====================================================================================================================
// GLFW

fn logGlfwVersion() void {
    Logger(.glfw).info("Compiled against GLFW {}.{}.{}", .{ glfw.versionMajor, glfw.versionMinor, glfw.versionRevision });

    var major: i32 = 0;
    var minor: i32 = 0;
    var revision: i32 = 0;
    glfw.getVersion(&major, &minor, &revision);
    const version_str = glfw.getVersionString();
    Logger(.glfw).info("Running against GLFW {}.{}.{} ({s})", .{ major, minor, revision, version_str });
}

fn onGlfwError(error_code: ?glfw.Error, description: []const u8) void {
    Logger(.glfw).err("GLFW error {s}: {s}", .{ @errorName(error_code orelse glfw.Error.Unknown), description });
}

fn getDisplayInfo(monitor: *Monitor) glfw.Error!DisplayInfo {
    const vm = try monitor.getVideoMode();
    const size = Size.abs(vm.width, vm.height);
    const pos = try monitor.getPos();
    const work_area = try monitor.getWorkarea();
    const scale = try monitor.getContentScale();
    return .{
        .display_area = .{ .size = size, .pos = pos },
        .work_area = work_area,
        .refresh_rate = @floatFromInt(vm.refreshRate),
        .scale_factor = @min(scale.xscale, scale.yscale),
    };
}

fn findMonitorMatch(point: ?Point, window: ?*Window) !*Monitor {
    var monitors = Monitor.getAll() catch return Monitor.getPrimary();

    // 1) Try to return monitor at point, if given.
    if (point) |p| {
        if (getMonitorAtPoint(monitors, p.x, p.y)) |monitor| return monitor;
    }

    // 2) Try to return monitor at center of window, if given.
    if (window) |w| b: {
        const wpos = w.getPos() catch break :b;
        const wsize = w.getSize() catch break :b;
        const wx = wpos.x +% @divTrunc(wsize.w, 2);
        const wy = wpos.y +% @divTrunc(wsize.h, 2);
        if (getMonitorAtPoint(monitors, wx, wy)) |monitor| return monitor;
    }

    // 3) Try to return any monitor.
    for (monitors) |maybe_monitor| {
        if (maybe_monitor) |monitor| return monitor;
    }

    // 4) Fall back on the primary monitor.
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
// WEBGPU

// TODO: move extern declarations out of main once Dawn dep is more settled
const DawnNativeInstance = ?*opaque {};
const DawnProcsTable = ?*opaque {};
extern fn dniCreate() DawnNativeInstance;
extern fn dniDestroy(DawnNativeInstance) void;
extern fn dniGetWgpuInstance(DawnNativeInstance) ?gpu.Instance;
extern fn dniDiscoverDefaultAdapters(DawnNativeInstance) void;
extern fn dnGetProcs() DawnProcsTable;
extern fn dawnProcSetProcs(DawnProcsTable) void;

// TODO: examine swap_chain format
const default_swap_chain_format = gpu.TextureFormat.bgra8_unorm;

fn dawnLog(comptime level: std.log.Level, comptime log: []const u8, message: ?[*:0]const u8) void {
    const L = Logger(.webgpu);
    const fun = switch (level) {
        .err => L.err,
        .warn => L.warn,
        .info => L.info,
        .debug => L.debug,
    };
    if (message) |m| {
        fun(log ++ ":\n{s}", .{m});
    } else {
        fun(log ++ ".", .{});
    }
}

fn requestAdapter(instance: gpu.Instance) Error!gpu.Adapter {
    const Response = struct {
        waiting: bool = false,
        status: gpu.RequestAdapterStatus = .unknown,
        adapter: gpu.Adapter = undefined,
        message: ?[*:0]const u8 = null,
    };

    const callback = (struct {
        fn value(
            status: gpu.RequestAdapterStatus,
            adapter: gpu.Adapter,
            message: ?[*:0]const u8,
            userdata: ?*anyopaque,
        ) callconv(.C) void {
            const response: *align(@alignOf(*usize)) Response = @ptrCast(@alignCast(userdata));
            response.* = .{ .status = status, .adapter = adapter, .message = message };
        }
    }).value;

    var response = Response{ .waiting = true };
    instance.requestAdapter(
        .{ .power_preference = .high_performance },
        callback,
        &response,
    );

    // TODO: proper handling of requestAdapter "promise"
    while (response.waiting) {}

    if (response.status == .success) {
        dawnLog(.info, "WebGPU adapter request successful", response.message);
        return response.adapter;
    } else {
        switch (response.status) {
            .unavailable => dawnLog(.err, "WebGPU adapter unavailable", response.message),
            .err => dawnLog(.err, "WebGPU adapter request returned in error", response.message),
            else => dawnLog(.err, "WebGPU adapter request failed for unknown reason", response.message),
        }
        return Error.NoWebGpuAdapter;
    }
}

fn requestDeviceFromAdapter(adapter: gpu.Adapter, device_lost: *bool) Error!gpu.Device {
    const dawn_skip_validation = false; // TODO: optionize skip_validation at either build or run time
    const link: ?*const gpu.ChainedStruct = if (!dawn_skip_validation) null else result: {
        const toggles = [_][*:0]const u8{"skip_validation"};
        const dawn_toggles = gpu.DawnTogglesDeviceDescriptor{
            .chain = .{ .next = null, .struct_type = .dawn_toggles_device_descriptor },
            .force_enabled_toggles_count = toggles.len,
            .force_enabled_toggles = &toggles,
        };
        break :result @ptrCast(&dawn_toggles);
    };

    const Response = struct {
        waiting: bool = false,
        status: gpu.RequestDeviceStatus = .unknown,
        device: gpu.Device = undefined,
        message: ?[*:0]const u8 = null,
    };

    const callback = (struct {
        fn value(
            status: gpu.RequestDeviceStatus,
            device: gpu.Device,
            message: ?[*:0]const u8,
            userdata: ?*anyopaque,
        ) callconv(.C) void {
            const response: *Response = @ptrCast(@alignCast(userdata));
            response.* = .{ .status = status, .device = device, .message = message };
        }
    }).value;

    var response = Response{ .waiting = true };
    adapter.requestDevice(
        gpu.DeviceDescriptor{ .next_in_chain = link },
        callback,
        &response,
    );

    // TODO: proper handling of requestDevice "promise"
    while (response.waiting) {}

    if (response.status != .success) {
        switch (response.status) {
            .err => dawnLog(.err, "WebGPU device request returned in error", response.message),
            else => dawnLog(.err, "WebGPU device request failed for unknown reason", response.message),
        }
        return Error.NoWebGpuDevice;
    }

    const device = response.device;
    errdefer device.release();

    dawnLog(.info, "WebGPU device request successful", response.message);
    device.setLoggingCallback(deviceLoggingCallback, null);
    device.setUncapturedErrorCallback(deviceUncapturedErrorCallback, null);
    device.setDeviceLostCallback(deviceLostCallback, device_lost);

    if (options.use_imgui) {
        if (imgui.wgpu.init(
            device,
            1,
            @intFromEnum(default_swap_chain_format),
            &.{},
        )) {
            Logger(.imgui).info("ImGui WebGPU backend initialization successful.", .{});
        } else {
            Logger(.imgui).err("ImGui WebGPU backend initialization failed.", .{});
            return Error.ImGuiInitFailure;
        }
    }

    return device;
}

fn requestDevice(instance: gpu.Instance, device_lost: *bool) Error!gpu.Device {
    const adapter = try requestAdapter(instance);
    defer adapter.release();

    return requestDeviceFromAdapter(adapter, device_lost);
}

fn releaseDevice(device: gpu.Device) void {
    // TODO: there may be a better way to re-init with ImGui_ImplWGPU_InvalidateDeviceObjects and CreateDeviceObjects
    if (options.use_imgui) imgui.wgpu.shutdown();
    device.release();
}

fn deviceLoggingCallback(log_type: gpu.LoggingType, message: ?[*:0]const u8, userdata: ?*anyopaque) callconv(.C) void {
    _ = userdata;
    const log = "WebGPU device log";
    switch (log_type) {
        .verbose => dawnLog(.debug, log, message),
        .info => dawnLog(.info, log, message),
        .warning => dawnLog(.warn, log, message),
        .err => dawnLog(.err, log, message),
    }
}

fn deviceUncapturedErrorCallback(err_type: gpu.ErrorType, message: ?[*:0]const u8, userdata: ?*anyopaque) callconv(.C) void {
    _ = userdata;
    switch (err_type) {
        .validation => dawnLog(.err, "Dawn validation error", message),
        .out_of_memory => dawnLog(.err, "Dawn out of memory", message),
        .internal => dawnLog(.err, "Dawn internal error", message),
        .device_lost => dawnLog(.err, "WebGPU device lost", message),
        .no_error, .unknown => dawnLog(.err, "Unknown Dawn error", message),
    }

    // TODO: attempt to recover gracefully from some error types (with framework user cooperation)
    std.process.exit(1);
}

fn deviceLostCallback(reason: gpu.DeviceLostReason, message: ?[*:0]const u8, userdata: ?*anyopaque) callconv(.C) void {
    var flag: *bool = @ptrCast(userdata);
    flag.* = true;

    switch (reason) {
        .undef => dawnLog(.warn, "WebGPU device lost (not destroyed)", message),
        .destroyed => dawnLog(.warn, "WebGPU device lost (destroyed)", message),
    }
}

fn createSurface(instance: gpu.Instance, window: *glfw.Window) glfw.Error!gpu.Surface {
    // TODO: more robust windowing system switch (and error on Wayland)
    switch (os.tag) {
        .windows => {
            var desc = gpu.SurfaceDescriptorFromWindowsHWND{
                .chain = .{ .next = null, .struct_type = .surface_descriptor_from_windows_hwnd },
                .hinstance = std.os.windows.kernel32.GetModuleHandleW(null).?,
                .hwnd = try window.getWin32Window(),
            };
            return instance.createSurface(.{ .next_in_chain = @ptrCast(&desc) });
        },

        .linux => {
            var desc = gpu.SurfaceDescriptorFromXlibWindow{
                .chain = .{ .next = null, .struct_type = .surface_descriptor_from_xlib_window },
                .display = try glfw.getX11Display(),
                .window = try window.getX11Window(),
            };
            return instance.createSurface(.{ .next_in_chain = @ptrCast(&desc) });
        },

        .macos => {
            const ns_window = try window.getCocoaWindow();

            // Create Metal layer and add Retina support
            const ns_view = objc.msgSend(ns_window, "contentView", .{}, *anyopaque);
            objc.msgSend(ns_view, "setWantsLayer:", .{true}, void);
            const CAMetalLayer = objc.getClass("CAMetalLayer");
            const layer = objc.msgSend(CAMetalLayer, "layer", .{}, ?*anyopaque) orelse {
                @panic("Couldn't create Metal layer.");
            };
            objc.msgSend(ns_view, "setLayer:", .{layer}, void);
            const scale = objc.msgSend(ns_window, "backingScaleFactor", .{}, f64);
            objc.msgSend(layer, "setContentsScale:", .{scale}, void);

            var desc = gpu.SurfaceDescriptorFromMetalLayer{
                .chain = .{ .next = null, .struct_type = .surface_descriptor_from_metal_layer },
                .layer = layer,
            };
            return instance.createSurface(.{ .next_in_chain = @ptrCast(&desc) });
        },

        else => @compileError("Platform not supported."),
    }
}

fn createSwapChain(device: gpu.Device, surface: gpu.Surface, width: u32, height: u32) gpu.SwapChain {
    const desc = gpu.SwapChainDescriptor{
        .usage = .{ .render_attachment = true },
        .format = default_swap_chain_format,
        .width = width,
        .height = height,
        .present_mode = .fifo,
        .implementation = 0,
    };
    return device.createSwapChain(surface, desc);
}

const objc = struct {
    const SEL = ?*opaque {};
    const Class = ?*opaque {};

    extern fn sel_getUid(str: [*:0]const u8) SEL;
    extern fn objc_getClass(name: [*:0]const u8) Class;
    extern fn objc_msgSend() void;

    fn getClass(name: [*:0]const u8) Class {
        return objc_getClass(name);
    }

    fn msgSend(obj: anytype, sel_name: [:0]const u8, args: anytype, comptime ReturnType: type) ReturnType {
        const args_meta = @typeInfo(@TypeOf(args)).Struct.fields;
        const FnType = switch (args_meta.len) {
            0 => *const fn (@TypeOf(obj), SEL) callconv(.C) ReturnType,
            1 => *const fn (@TypeOf(obj), SEL, args_meta[0].type) callconv(.C) ReturnType,
            else => @compileError("Too many params for objc msgSend (manually add support for more)."),
        };
        const func: FnType = @ptrCast(&objc_msgSend);
        const sel = sel_getUid(sel_name.ptr);
        return @call(.never_inline, func, .{ obj, sel } ++ args);
    }
};
