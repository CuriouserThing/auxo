usingnamespace @cImport({
    @cInclude("GLFW/glfw3.h");
});
const std = @import("std");

const io = @import("io.zig");
const Point = io.Point;
const Size = io.Size;
const VideoMode = io.VideoMode;
const KeyMods = io.KeyMods;
const MouseButtonAction = io.MouseButtonAction;
const MouseButton = io.MouseButton;
const KeyAction = io.KeyAction;
const Key = io.Key;

pub const versionMajor = GLFW_VERSION_MAJOR;
pub const versionMinor = GLFW_VERSION_MINOR;
pub const versionRevision = GLFW_VERSION_REVISION;
pub fn getVersion(major: *i32, minor: *i32, revision: *i32) void {
    glfwGetVersion(major, minor, revision);
}
pub fn getVersionString() []const u8 {
    return std.mem.spanZ(glfwGetVersionString());
}

pub const ErrorCallback = fn (Error, []const u8) void;
pub fn setErrorCallback(callback: ?ErrorCallback) void {
    errorCallback = callback;
    _ = glfwSetErrorCallback(if (callback) |_| cErrorCallback else null);
}
var errorCallback: ?ErrorCallback = null;
fn cErrorCallback(error_code: c_int, description: [*c]const u8) callconv(.C) void {
    errorCallback.?(codeToError(error_code), std.mem.spanZ(description));
}

pub const Error = error{
    Unknown,
    NoError,
    NotInitialized,
    NoCurrentContext,
    InvalidEnum,
    InvalidValue,
    OutOfMemory,
    ApiUnavailable,
    VersionUnavailable,
    PlatformError,
    FormatUnavailable,
    NoWindowContext,
};

pub const ContextCreationError = error{
    Unknown,
    PlatformError,
};

pub const WindowCreationError = error{
    Unknown,
    NotInitialized,
    InvalidEnum,
    InvalidValue,
    ApiUnavailable,
    VersionUnavailable,
    FormatUnavailable,
    PlatformError,
};

fn codeToError(code: c_int) Error {
    return switch (code) {
        GLFW_NO_ERROR => Error.NoError,
        GLFW_NOT_INITIALIZED => Error.NotInitialized,
        GLFW_NO_CURRENT_CONTEXT => Error.NoCurrentContext,
        GLFW_INVALID_ENUM => Error.InvalidEnum,
        GLFW_INVALID_VALUE => Error.InvalidValue,
        GLFW_OUT_OF_MEMORY => Error.OutOfMemory,
        GLFW_API_UNAVAILABLE => Error.ApiUnavailable,
        GLFW_VERSION_UNAVAILABLE => Error.VersionUnavailable,
        GLFW_PLATFORM_ERROR => Error.PlatformError,
        GLFW_FORMAT_UNAVAILABLE => Error.FormatUnavailable,
        GLFW_NO_WINDOW_CONTEXT => Error.NoWindowContext,
        else => Error.Unknown,
    };
}

fn getError(comptime T: type) T {
    const code = glfwGetError(null);
    const err = codeToError(code);
    const name = @errorName(err);
    for (std.meta.fields(T)) |field| {
        if (std.mem.eql(u8, name, field.name)) {
            return @errSetCast(T, err);
        }
    } else {
        return @errSetCast(T, Error.Unknown);
    }
}

pub fn init() !void {
    if (glfwInit() == GLFW_FALSE) {
        return getError(ContextCreationError);
    }
}

pub const terminate = glfwTerminate;

pub const waitEvents = glfwWaitEvents;

pub const postEmptyEvent = glfwPostEmptyEvent;

pub const getProcAddress = glfwGetProcAddress;

pub const swapInterval = glfwSwapInterval;

pub fn detachCurrentContext() void {
    glfwMakeContextCurrent(null);
}

pub fn hintWindowVisible(value: bool) void {
    glfwWindowHint(GLFW_VISIBLE, @boolToInt(value));
}

pub fn hintWindowDecorated(value: bool) void {
    glfwWindowHint(GLFW_DECORATED, @boolToInt(value));
}

pub fn hintWindowRefreshRate(value: i32) void {
    glfwWindowHint(GLFW_REFRESH_RATE, value);
}

pub fn WindowHandle(comptime UserT: type) type {
    return struct {
        const Self = @This();
        ptr: *GLFWwindow,

        pub fn create(size: Size, title: [:0]const u8, comptime MonitorUserT: type, monitor: ?MonitorHandle(MonitorUserT), share: ?Self) !Self {
            const p_title = title.ptr;
            const p_monitor = if (monitor) |m| m.ptr else null;
            const p_share = if (share) |s| s.ptr else null;
            const p = glfwCreateWindow(size.w, size.h, p_title, p_monitor, p_share) orelse return getError(WindowCreationError);
            return Self{ .ptr = p };
        }

        pub fn destroy(self: Self) void {
            glfwDestroyWindow(self.ptr);
        }

        pub fn swapBuffers(self: Self) void {
            glfwSwapBuffers(self.ptr);
        }

        pub fn makeContextCurrent(self: Self) void {
            glfwMakeContextCurrent(self.ptr);
        }

        pub fn setUserPointer(self: Self, pointer: ?*UserT) void {
            glfwSetWindowUserPointer(self.ptr, pointer);
        }

        fn getObj(window: ?*GLFWwindow) ?*UserT {
            const ptr = window orelse return null;
            const user_ptr = glfwGetWindowUserPointer(ptr) orelse return null;
            return @ptrCast(*UserT, @alignCast(@sizeOf(*UserT), user_ptr));
        }

        pub fn setPosCallback(self: Self, comptime callback: ?PosCallback) void {
            _ = glfwSetWindowPosCallback(self.ptr, if (callback) |c| PosCallbackWrapper(c).value else null);
        }

        const PosCallback = fn (*UserT, Point) void;
        fn PosCallbackWrapper(callback: PosCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, xpos: c_int, ypos: c_int) callconv(.C) void {
                    const obj = getObj(window) orelse return;
                    callback(obj, .{ .x = xpos, .y = ypos });
                }
            };
        }

        pub fn setSizeCallback(self: Self, comptime callback: ?SizeCallback) void {
            _ = glfwSetWindowSizeCallback(self.ptr, if (callback) |c| SizeCallbackWrapper(c).value else null);
        }
        pub fn setFramebufferSizeCallback(self: Self, comptime callback: ?SizeCallback) void {
            _ = glfwSetFramebufferSizeCallback(self.ptr, if (callback) |c| SizeCallbackWrapper(c).value else null);
        }

        const SizeCallback = fn (*UserT, Size) void;
        fn SizeCallbackWrapper(callback: SizeCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, width: c_int, height: c_int) callconv(.C) void {
                    const obj = getObj(window) orelse return;
                    if (width < 0 or height < 0) return;
                    callback(obj, .{ .w = @intCast(u31, width), .h = @intCast(u31, height) });
                }
            };
        }

        pub fn setCloseCallback(self: Self, comptime callback: ?CloseCallback) void {
            _ = glfwSetWindowCloseCallback(self.ptr, if (callback) |c| CloseCallbackWrapper(c).value else null);
        }

        const CloseCallback = fn (*UserT) void;
        fn CloseCallbackWrapper(callback: CloseCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow) callconv(.C) void {
                    const obj = getObj(window) orelse return;
                    callback(obj);
                }
            };
        }

        pub fn setMaximizeCallback(self: Self, comptime callback: ?BooleanCallback) void {
            _ = glfwSetWindowMaximizeCallback(self.ptr, if (callback) |c| BooleanCallbackWrapper(c).value else null);
        }
        pub fn setCursorEnterCallback(self: Self, comptime callback: ?BooleanCallback) void {
            _ = glfwSetCursorEnterCallback(self.ptr, if (callback) |c| BooleanCallbackWrapper(c).value else null);
        }
        pub fn setFocusCallback(self: Self, comptime callback: ?BooleanCallback) void {
            _ = glfwSetWindowFocusCallback(self.ptr, if (callback) |c| BooleanCallbackWrapper(c).value else null);
        }
        pub fn setIconifyCallback(self: Self, comptime callback: ?BooleanCallback) void {
            _ = glfwSetWindowIconifyCallback(self.ptr, if (callback) |c| BooleanCallbackWrapper(c).value else null);
        }

        const BooleanCallback = fn (*UserT, bool) void;
        fn BooleanCallbackWrapper(callback: BooleanCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, val: c_int) callconv(.C) void {
                    const obj = getObj(window) orelse return;
                    callback(obj, val != GLFW_FALSE);
                }
            };
        }

        pub fn setKeyCallback(self: Self, comptime callback: ?KeyCallback) void {
            _ = glfwSetKeyCallback(self.ptr, if (callback) |c| KeyCallbackWrapper(c).value else null);
        }

        const KeyCallback = fn (*UserT, Key, i32, KeyAction, KeyMods) void;
        fn KeyCallbackWrapper(callback: KeyCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.C) void {
                    const obj = getObj(window) orelse return;

                    const k = translateKey(key);
                    const a: KeyAction = switch (action) {
                        GLFW_PRESS => .press,
                        GLFW_RELEASE => .release,
                        GLFW_REPEAT => .repeat,
                        else => return,
                    };
                    const m = translateKeyMods(mods);

                    callback(obj, k, scancode, a, m);
                }
            };
        }

        pub fn setCharCallback(self: Self, comptime callback: ?CharCallback) void {
            _ = glfwSetCharCallback(self.ptr, if (callback) |c| CharCallbackWrapper(c).value else null);
        }

        /// The param is a valid Unicode codepoint, so std.unicode.utf8Encode will encode it without error.
        const CharCallback = fn (*UserT, u21) void;
        fn CharCallbackWrapper(callback: CharCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, codepoint: c_uint) callconv(.C) void {
                    const obj = getObj(window) orelse return;
                    if (codepoint > 0x1FFFFF) return;
                    var c = @intCast(u21, codepoint);
                    if (!std.unicode.utf8ValidCodepoint(c)) return;
                    callback(obj, c);
                }
            };
        }

        pub fn setMouseButtonCallback(self: Self, comptime callback: ?MouseButtonCallback) void {
            _ = glfwSetMouseButtonCallback(self.ptr, if (callback) |c| MouseButtonCallbackWrapper(c).value else null);
        }

        const MouseButtonCallback = fn (*UserT, MouseButton, MouseButtonAction, KeyMods) void;
        fn MouseButtonCallbackWrapper(callback: MouseButtonCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, button: c_int, action: c_int, mods: c_int) callconv(.C) void {
                    const obj = getObj(window) orelse return;

                    const b: MouseButton = switch (button) {
                        GLFW_MOUSE_BUTTON_1 => .left,
                        GLFW_MOUSE_BUTTON_2 => .right,
                        GLFW_MOUSE_BUTTON_3 => .middle,
                        GLFW_MOUSE_BUTTON_4 => .x1,
                        GLFW_MOUSE_BUTTON_5 => .x2,
                        else => .unknown,
                    };
                    const a: MouseButtonAction = switch (action) {
                        GLFW_PRESS => .press,
                        GLFW_RELEASE => .release,
                        else => return,
                    };
                    const m = translateKeyMods(mods);

                    callback(obj, b, a, m);
                }
            };
        }

        pub fn setCursorPosCallback(self: Self, comptime callback: ?PointFCallback) void {
            _ = glfwSetCursorPosCallback(self.ptr, if (callback) |c| PointFCallbackWrapper(c).value else null);
        }
        pub fn setScrollCallback(self: Self, comptime callback: ?PointFCallback) void {
            _ = glfwSetScrollCallback(self.ptr, if (callback) |c| PointFCallbackWrapper(c).value else null);
        }

        const PointFCallback = fn (*UserT, f64, f64) void;
        fn PointFCallbackWrapper(callback: PointFCallback) type {
            return struct {
                fn value(window: ?*GLFWwindow, x: f64, y: f64) callconv(.C) void {
                    const obj = getObj(window) orelse return;
                    callback(obj, x, y);
                }
            };
        }

        pub fn hide(self: Self) void {
            glfwHideWindow(self.ptr);
        }

        pub fn show(self: Self) void {
            glfwShowWindow(self.ptr);
        }

        pub fn restore(self: Self) void {
            glfwRestoreWindow(self.ptr);
        }

        pub fn maximize(self: Self) void {
            glfwMaximizeWindow(self.ptr);
        }

        pub fn getPos(self: Self) Point {
            var x: i32 = 0;
            var y: i32 = 0;
            glfwGetWindowPos(self.ptr, &x, &y);
            return .{ .x = x, .y = y };
        }

        pub fn setPos(self: Self, pos: Point) void {
            glfwSetWindowPos(self.ptr, pos.x, pos.y);
        }

        pub fn getSize(self: Self) Size {
            var w: i32 = 0;
            var h: i32 = 0;
            glfwGetWindowSize(self.ptr, &w, &h);
            if (w < 0) w = 0;
            if (h < 0) h = 0;
            return .{ .w = @intCast(u31, w), .h = @intCast(u31, h) };
        }

        pub fn setSize(self: Self, size: Size) void {
            glfwSetWindowSize(self.ptr, size.w, size.h);
        }

        pub fn setDecorated(self: Self, decorated: bool) void {
            glfwSetWindowAttrib(self.ptr, GLFW_DECORATED, @boolToInt(decorated));
        }

        pub fn isMaximized(self: Self) bool {
            return glfwGetWindowAttrib(self.ptr, GLFW_MAXIMIZED) != GLFW_FALSE;
        }

        pub fn isIconified(self: Self) bool {
            return glfwGetWindowAttrib(self.ptr, GLFW_ICONIFIED) != GLFW_FALSE;
        }

        pub fn setFullscreen(self: Self, comptime MonitorUserT: type, monitor: MonitorHandle(MonitorUserT), size: Size, refresh_rate: ?i32) void {
            glfwSetWindowMonitor(self.ptr, monitor.ptr, GLFW_DONT_CARE, GLFW_DONT_CARE, size.w, size.h, refresh_rate orelse GLFW_DONT_CARE);
        }

        pub fn setWindowed(self: Self, pos: Point, size: Size) void {
            glfwSetWindowMonitor(self.ptr, null, pos.x, pos.y, size.w, size.h, GLFW_DONT_CARE);
        }

        pub fn getFramebufferSize(self: Self) Size {
            var w: i32 = 0;
            var h: i32 = 0;
            glfwGetFramebufferSize(self.ptr, &w, &h);
            if (w < 0) w = 0;
            if (h < 0) h = 0;
            return .{ .w = @intCast(u31, w), .h = @intCast(u31, h) };
        }

        pub fn getFullscreenMonitor(self: Self, comptime MonitorUserT: type) ?MonitorHandle(MonitorUserT) {
            const ptr = glfwGetWindowMonitor(self.ptr) orelse return null;
            return MonitorHandle(MonitorUserT){ .ptr = ptr };
        }
    };
}

pub fn MonitorHandle(comptime UserT: type) type {
    return struct {
        const Self = @This();
        ptr: *GLFWmonitor,

        pub fn getPrimary() !Self {
            const ptr = glfwGetPrimaryMonitor() orelse return error.Unknown;
            return Self{ .ptr = ptr };
        }

        pub fn getAll() !Iterator {
            var count: i32 = 0;
            const handles = glfwGetMonitors(&count) orelse return error.Unknown;
            return Iterator{ .ptr = handles, .len = @intCast(usize, count) };
        }

        const Iterator = struct {
            ptr: [*c]?*GLFWmonitor,
            len: usize,
            idx: usize = 0,

            pub fn next(iter: *Iterator) ?Self {
                if (iter.idx >= iter.len) return null;
                const p = iter.ptr[iter.idx] orelse return null;
                iter.idx += 1;
                return Self{ .ptr = p };
            }
        };

        pub fn setUserPointer(self: Self, pointer: ?*UserT) void {
            glfwSetMonitorUserPointer(self.ptr, pointer);
        }

        pub fn getUserPointer(self: Self) ?*UserT {
            const user_ptr = glfwGetMonitorUserPointer(self.ptr) orelse return null;
            return @ptrCast(*UserT, @alignCast(@sizeOf(*UserT), user_ptr));
        }

        pub fn getName(self: Self) []const u8 {
            return std.mem.spanZ(glfwGetMonitorName(self.ptr));
        }

        pub fn getPos(self: Self) Point {
            var xpos: i32 = 0;
            var ypos: i32 = 0;
            glfwGetMonitorPos(self.ptr, &xpos, &ypos);
            return .{ .x = xpos, .y = ypos };
        }

        pub fn getCurrentVideoMode(self: Self) !VideoMode {
            const p_mode = glfwGetVideoMode(self.ptr) orelse return error.Unknown;
            return modeFromGlfw(p_mode.*);
        }

        pub fn getAllVideoModes(self: Self) !ModeIterator {
            var count: i32 = 0;
            const modes = glfwGetVideoModes(self.ptr, &count) orelse return error.Unknown;
            return ModeIterator{ .ptr = modes, .len = @intCast(usize, count) };
        }

        const ModeIterator = struct {
            ptr: [*c]const GLFWvidmode,
            len: usize,
            idx: usize = 0,

            pub fn next(iter: *ModeIterator) ?VideoMode {
                if (iter.idx >= iter.len) return null;
                const mode = iter.ptr[iter.idx];
                iter.idx += 1;
                return modeFromGlfw(mode);
            }
        };

        fn modeFromGlfw(mode: GLFWvidmode) VideoMode {
            const w = if (mode.width < 0) 0 else mode.width;
            const h = if (mode.height < 0) 0 else mode.height;
            return VideoMode{
                .size = .{ .w = @intCast(u31, w), .h = @intCast(u31, h) },
                .refresh_rate = mode.refreshRate,
            };
        }
    };
}

fn translateKeyMods(mods: c_int) KeyMods {
    return .{
        .shift = mods & 0x01 == 0x01,
        .control = mods & 0x02 == 0x02,
        .alt = mods & 0x04 == 0x04,
        .gui = mods & 0x08 == 0x08,
        .caps_lock = mods & 0x10 == 0x10,
        .num_lock = mods & 0x20 == 0x20,
    };
}

fn translateKey(key: c_int) Key {
    return switch (key) {
        GLFW_KEY_UNKNOWN => .unknown,
        GLFW_KEY_SPACE => .space,
        GLFW_KEY_APOSTROPHE => .apostrophe,
        GLFW_KEY_COMMA => .comma,
        GLFW_KEY_MINUS => .hyphen,
        GLFW_KEY_PERIOD => .period,
        GLFW_KEY_SLASH => .slash,
        GLFW_KEY_0 => .keyboard_0,
        GLFW_KEY_1 => .keyboard_1,
        GLFW_KEY_2 => .keyboard_2,
        GLFW_KEY_3 => .keyboard_3,
        GLFW_KEY_4 => .keyboard_4,
        GLFW_KEY_5 => .keyboard_5,
        GLFW_KEY_6 => .keyboard_6,
        GLFW_KEY_7 => .keyboard_7,
        GLFW_KEY_8 => .keyboard_8,
        GLFW_KEY_9 => .keyboard_9,
        GLFW_KEY_SEMICOLON => .semicolon,
        GLFW_KEY_EQUAL => .equal,
        GLFW_KEY_A => .a,
        GLFW_KEY_B => .b,
        GLFW_KEY_C => .c,
        GLFW_KEY_D => .d,
        GLFW_KEY_E => .e,
        GLFW_KEY_F => .f,
        GLFW_KEY_G => .g,
        GLFW_KEY_H => .h,
        GLFW_KEY_I => .i,
        GLFW_KEY_J => .j,
        GLFW_KEY_K => .k,
        GLFW_KEY_L => .l,
        GLFW_KEY_M => .m,
        GLFW_KEY_N => .n,
        GLFW_KEY_O => .o,
        GLFW_KEY_P => .p,
        GLFW_KEY_Q => .q,
        GLFW_KEY_R => .r,
        GLFW_KEY_S => .s,
        GLFW_KEY_T => .t,
        GLFW_KEY_U => .u,
        GLFW_KEY_V => .v,
        GLFW_KEY_W => .w,
        GLFW_KEY_X => .x,
        GLFW_KEY_Y => .y,
        GLFW_KEY_Z => .z,
        GLFW_KEY_LEFT_BRACKET => .left_bracket,
        GLFW_KEY_BACKSLASH => .backslash,
        GLFW_KEY_RIGHT_BRACKET => .right_bracket,
        GLFW_KEY_GRAVE_ACCENT => .grave_accent,
        GLFW_KEY_WORLD_1 => .non_us_hash,
        GLFW_KEY_WORLD_2 => .non_us_backslash,
        GLFW_KEY_ESCAPE => .escape,
        GLFW_KEY_ENTER => .enter,
        GLFW_KEY_TAB => .tab,
        GLFW_KEY_BACKSPACE => .backspace,
        GLFW_KEY_INSERT => .insert,
        GLFW_KEY_DELETE => .delete,
        GLFW_KEY_RIGHT => .right_arrow,
        GLFW_KEY_LEFT => .left_arrow,
        GLFW_KEY_DOWN => .down_arrow,
        GLFW_KEY_UP => .up_arrow,
        GLFW_KEY_PAGE_UP => .page_up,
        GLFW_KEY_PAGE_DOWN => .page_down,
        GLFW_KEY_HOME => .home,
        GLFW_KEY_END => .end,
        GLFW_KEY_CAPS_LOCK => .caps_lock,
        GLFW_KEY_SCROLL_LOCK => .scroll_lock,
        GLFW_KEY_NUM_LOCK => .num_lock,
        GLFW_KEY_PRINT_SCREEN => .print_screen,
        GLFW_KEY_PAUSE => .pause,
        GLFW_KEY_F1 => .f1,
        GLFW_KEY_F2 => .f2,
        GLFW_KEY_F3 => .f3,
        GLFW_KEY_F4 => .f4,
        GLFW_KEY_F5 => .f5,
        GLFW_KEY_F6 => .f6,
        GLFW_KEY_F7 => .f7,
        GLFW_KEY_F8 => .f8,
        GLFW_KEY_F9 => .f9,
        GLFW_KEY_F10 => .f10,
        GLFW_KEY_F11 => .f11,
        GLFW_KEY_F12 => .f12,
        GLFW_KEY_F13 => .f13,
        GLFW_KEY_F14 => .f14,
        GLFW_KEY_F15 => .f15,
        GLFW_KEY_F16 => .f16,
        GLFW_KEY_F17 => .f17,
        GLFW_KEY_F18 => .f18,
        GLFW_KEY_F19 => .f19,
        GLFW_KEY_F20 => .f20,
        GLFW_KEY_F21 => .f21,
        GLFW_KEY_F22 => .f22,
        GLFW_KEY_F23 => .f23,
        GLFW_KEY_F24 => .f24,
        //GLFW_KEY_F25,
        GLFW_KEY_KP_0 => .keypad_0,
        GLFW_KEY_KP_1 => .keypad_1,
        GLFW_KEY_KP_2 => .keypad_2,
        GLFW_KEY_KP_3 => .keypad_3,
        GLFW_KEY_KP_4 => .keypad_4,
        GLFW_KEY_KP_5 => .keypad_5,
        GLFW_KEY_KP_6 => .keypad_6,
        GLFW_KEY_KP_7 => .keypad_7,
        GLFW_KEY_KP_8 => .keypad_8,
        GLFW_KEY_KP_9 => .keypad_9,
        GLFW_KEY_KP_DECIMAL => .keypad_decimal,
        GLFW_KEY_KP_DIVIDE => .keypad_divide,
        GLFW_KEY_KP_MULTIPLY => .keypad_multiply,
        GLFW_KEY_KP_SUBTRACT => .keypad_subtract,
        GLFW_KEY_KP_ADD => .keypad_add,
        GLFW_KEY_KP_ENTER => .keypad_enter,
        GLFW_KEY_KP_EQUAL => .keypad_equal,
        GLFW_KEY_LEFT_SHIFT => .left_shift,
        GLFW_KEY_LEFT_CONTROL => .left_control,
        GLFW_KEY_LEFT_ALT => .left_alt,
        GLFW_KEY_LEFT_SUPER => .left_gui,
        GLFW_KEY_RIGHT_SHIFT => .right_shift,
        GLFW_KEY_RIGHT_CONTROL => .right_control,
        GLFW_KEY_RIGHT_ALT => .right_alt,
        GLFW_KEY_RIGHT_SUPER => .right_gui,
        // GLFW_KEY_MENU,
        else => .unknown,
    };
}
