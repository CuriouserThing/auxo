const c = @cImport({
    @cInclude("GLFW/glfw3.h");
});
const std = @import("std");

const io = @import("io.zig");
const Point = io.Point;
const Size = io.Size;
const Rectangle = io.Rectangle;
const KeyMods = io.KeyMods;
const MouseButtonAction = io.MouseButtonAction;
const MouseButton = io.MouseButton;
const KeyAction = io.KeyAction;
const Key = io.Key;

pub const versionMajor = c.GLFW_VERSION_MAJOR;
pub const versionMinor = c.GLFW_VERSION_MINOR;
pub const versionRevision = c.GLFW_VERSION_REVISION;
pub fn getVersion(major: *i32, minor: *i32, revision: *i32) void {
    c.glfwGetVersion(major, minor, revision);
}
pub fn getVersionString() []const u8 {
    return std.mem.sliceTo(c.glfwGetVersionString(), 0);
}

pub const Error = error{
    Unknown,
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

pub const ErrorCallback = fn (?Error, []const u8) void;
pub fn setErrorCallback(comptime callback: ?*const ErrorCallback) void {
    errorCallback = callback;
    _ = c.glfwSetErrorCallback(if (callback) |_| cErrorCallback else null);
}
var errorCallback: ?*const ErrorCallback = null;
fn cErrorCallback(error_code: c_int, description: [*c]const u8) callconv(.C) void {
    errorCallback.?(codeToError(error_code), std.mem.sliceTo(description, 0));
}

fn codeToError(code: c_int) ?Error {
    return switch (code) {
        c.GLFW_NO_ERROR => null,
        c.GLFW_NOT_INITIALIZED => Error.NotInitialized,
        c.GLFW_NO_CURRENT_CONTEXT => Error.NoCurrentContext,
        c.GLFW_INVALID_ENUM => Error.InvalidEnum,
        c.GLFW_INVALID_VALUE => Error.InvalidValue,
        c.GLFW_OUT_OF_MEMORY => Error.OutOfMemory,
        c.GLFW_API_UNAVAILABLE => Error.ApiUnavailable,
        c.GLFW_VERSION_UNAVAILABLE => Error.VersionUnavailable,
        c.GLFW_PLATFORM_ERROR => Error.PlatformError,
        c.GLFW_FORMAT_UNAVAILABLE => Error.FormatUnavailable,
        c.GLFW_NO_WINDOW_CONTEXT => Error.NoWindowContext,
        else => Error.Unknown,
    };
}

fn getError() ?Error {
    const code = c.glfwGetError(null);
    return codeToError(code);
}

pub fn init() !void {
    if (c.glfwInit() == c.GLFW_FALSE) {
        return getError() orelse Error.Unknown;
    }
}

pub const terminate = c.glfwTerminate;
pub const waitEvents = c.glfwWaitEvents;
pub const postEmptyEvent = c.glfwPostEmptyEvent;

pub fn hintWindowMaximized(value: bool) void {
    c.glfwWindowHint(c.GLFW_MAXIMIZED, @boolToInt(value));
}

pub const JOYSTICK_COUNT = c.GLFW_JOYSTICK_LAST + 1;

pub fn joystickPresent(jid: i32) bool {
    return c.glfwJoystickPresent(jid) == c.GLFW_TRUE;
}

pub const JoyButtonState = enum(u8) {
    release = 0,
    press = 1,
    _,
};
extern fn glfwGetJoystickButtons(jid: c_int, count: [*c]c_int) [*c]const JoyButtonState;
pub fn getJoystickButtons(jid: i32) !?[]const JoyButtonState {
    var count: i32 = 0;
    const ptr = glfwGetJoystickButtons(jid, &count) orelse return getError() orelse null;
    return ptr[0..@intCast(usize, count)];
}

pub fn getJoystickAxes(jid: i32) !?[]const f32 {
    var count: i32 = 0;
    const ptr = c.glfwGetJoystickAxes(jid, &count) orelse return getError() orelse null;
    return ptr[0..@intCast(usize, count)];
}

pub const JoyHatState = enum(u8) {
    centered = 0,
    up = 1,
    right = 2,
    down = 4,
    left = 8,
    right_up = 2 | 1,
    right_down = 2 | 4,
    left_up = 8 | 1,
    left_down = 8 | 4,
    _,
};
extern fn glfwGetJoystickHats(jid: c_int, count: [*c]c_int) [*c]const JoyHatState;
pub fn getJoystickHats(jid: i32) !?[]const JoyHatState {
    var count: i32 = 0;
    const ptr = glfwGetJoystickHats(jid, &count) orelse return getError() orelse null;
    return ptr[0..@intCast(usize, count)];
}

pub fn getJoystickGuid(jid: i32) !?[]const u8 {
    const ptr = c.glfwGetJoystickGUID(jid) orelse return getError() orelse null;
    return std.mem.sliceTo(ptr, 0);
}

pub const Monitor = opaque {
    const Self = @This();

    extern fn glfwGetPrimaryMonitor() ?*Self;
    extern fn glfwSetMonitorUserPointer(*Self, ?*anyopaque) void;
    extern fn glfwGetMonitorUserPointer(*Self) ?*anyopaque;
    extern fn glfwGetMonitorPos(*Self, *c_int, *c_int) void;
    extern fn glfwGetMonitorWorkarea(*Self, *c_int, *c_int, *c_int, *c_int) void;
    extern fn glfwGetMonitorContentScale(*Self, *f32, *f32) void;
    extern fn glfwGetVideoModes(*Self, *c_int) [*c]const c.GLFWvidmode;
    extern fn glfwGetVideoMode(*Self) ?*c.GLFWvidmode;

    pub fn getPrimary() !*Self {
        return glfwGetPrimaryMonitor() orelse return error.Unknown;
    }

    pub fn getAll() ![]?*Self {
        var count: i32 = 0;
        const handles = c.glfwGetMonitors(&count) orelse return getError() orelse return Error.Unknown;
        return @ptrCast([*]?*Self, handles)[0..@intCast(usize, count)];
    }

    pub fn setUserPointer(self: *Self, comptime T: type, pointer: ?*T) void {
        glfwSetMonitorUserPointer(self, pointer);
    }

    pub fn getUserPointer(self: *Self, comptime T: type) ?*T {
        const user_ptr = glfwGetMonitorUserPointer(self) orelse return null;
        return @ptrCast(*T, @alignCast(@sizeOf(*T), user_ptr));
    }

    pub fn getName(self: *Self) ?[]const u8 {
        return std.mem.sliceTo(c.glfwGetMonitorName(self), 0);
    }

    pub fn getPos(self: *Self) !Point {
        var xpos: i32 = 0;
        var ypos: i32 = 0;
        glfwGetMonitorPos(self, &xpos, &ypos);
        return getError() orelse .{ .x = xpos, .y = ypos };
    }

    pub fn getWorkarea(self: *Self) !Rectangle {
        var xpos: i32 = 0;
        var ypos: i32 = 0;
        var width: i32 = 0;
        var height: i32 = 0;
        glfwGetMonitorWorkarea(self, &xpos, &ypos, &width, &height);
        return getError() orelse .{ .pos = .{ .x = xpos, .y = ypos }, .size = .{ .w = width, .h = height } };
    }

    pub const Scale = struct {
        xscale: f32,
        yscale: f32,
    };
    pub fn getContentScale(self: *Self) !Scale {
        var xscale: f32 = 0;
        var yscale: f32 = 0;
        glfwGetMonitorContentScale(self, &xscale, &yscale);
        return getError() orelse .{ .xscale = xscale, .yscale = yscale };
    }

    pub fn getVideoMode(self: *Self) !*c.GLFWvidmode {
        return glfwGetVideoMode(self) orelse return error.Unknown;
    }
};

pub const Window = opaque {
    const Self = @This();

    extern fn glfwCreateWindow(c_int, c_int, [*:0]const u8, ?*Monitor, ?*Self) ?*Self;
    extern fn glfwDestroyWindow(*Self) void;
    extern fn glfwSetWindowUserPointer(*Self, ?*anyopaque) void;
    extern fn glfwGetWindowUserPointer(*Self) ?*anyopaque;
    extern fn glfwHideWindow(*Self) void;
    extern fn glfwShowWindow(*Self) void;
    extern fn glfwRestoreWindow(*Self) void;
    extern fn glfwMaximizeWindow(*Self) void;
    extern fn glfwGetWindowPos(*Self, *c_int, *c_int) void;
    extern fn glfwSetWindowPos(*Self, c_int, c_int) void;
    extern fn glfwGetWindowSize(*Self, *c_int, *c_int) void;
    extern fn glfwSetWindowSize(*Self, c_int, c_int) void;
    extern fn glfwGetFramebufferSize(*Self, *c_int, *c_int) void;
    extern fn glfwSetWindowAttrib(*Self, c_int, c_int) void;
    extern fn glfwGetWindowAttrib(*Self, c_int) c_int;
    extern fn glfwSetWindowMonitor(*Self, ?*Monitor, c_int, c_int, c_int, c_int, c_int) void;
    extern fn glfwGetWindowMonitor(*Self) ?*Monitor;

    pub fn create(size: Size, title: [:0]const u8, monitor: ?*Monitor, share: ?*Self) !*Self {
        return glfwCreateWindow(size.w, size.h, title.ptr, monitor, share) orelse (getError() orelse Error.Unknown);
    }

    pub fn destroy(self: *Self) void {
        glfwDestroyWindow(self);
    }

    pub fn setUserPointer(self: *Self, comptime T: type, pointer: ?*T) void {
        glfwSetWindowUserPointer(self, pointer);
    }

    fn getUserObj(window: ?*Self, comptime T: type) ?*T {
        const w = window orelse return null;
        const user_ptr = glfwGetWindowUserPointer(w) orelse return null;
        return @ptrCast(*T, @alignCast(@sizeOf(*T), user_ptr));
    }

    pub fn hide(self: *Self) void {
        glfwHideWindow(self);
    }

    pub fn show(self: *Self) void {
        glfwShowWindow(self);
    }

    pub fn restore(self: *Self) void {
        glfwRestoreWindow(self);
    }

    pub fn maximize(self: *Self) void {
        glfwMaximizeWindow(self);
    }

    pub fn getPos(self: *Self) !Point {
        var xpos: i32 = 0;
        var ypos: i32 = 0;
        glfwGetWindowPos(self, &xpos, &ypos);
        return getError() orelse .{ .x = xpos, .y = ypos };
    }

    pub fn setPos(self: *Self, pos: Point) void {
        glfwSetWindowPos(self, pos.x, pos.y);
    }

    pub fn getSize(self: *Self) !Size {
        var width: i32 = 0;
        var height: i32 = 0;
        glfwGetWindowSize(self, &width, &height);
        return getError() orelse .{ .w = width, .h = height };
    }

    pub fn setSize(self: *Self, size: Size) void {
        glfwSetWindowSize(self, size.w, size.h);
    }

    pub fn setDecorated(self: *Self, decorated: bool) void {
        glfwSetWindowAttrib(self, c.GLFW_DECORATED, @boolToInt(decorated));
    }

    pub fn isMaximized(self: *Self) bool {
        return glfwGetWindowAttrib(self, c.GLFW_MAXIMIZED) != c.GLFW_FALSE;
    }

    pub fn isIconified(self: *Self) bool {
        return glfwGetWindowAttrib(self, c.GLFW_ICONIFIED) != c.GLFW_FALSE;
    }

    pub fn setFullscreen(self: *Self, monitor: *Monitor, size: Size, refresh_rate: ?i32) void {
        glfwSetWindowMonitor(self, monitor, c.GLFW_DONT_CARE, c.GLFW_DONT_CARE, size.w, size.h, refresh_rate orelse c.GLFW_DONT_CARE);
    }

    pub fn setWindowed(self: *Self, pos: Point, size: Size) void {
        glfwSetWindowMonitor(self, null, pos.x, pos.y, size.w, size.h, c.GLFW_DONT_CARE);
    }

    pub fn getFramebufferSize(self: *Self) !Size {
        var width: i32 = 0;
        var height: i32 = 0;
        glfwGetFramebufferSize(self, &width, &height);
        return getError() orelse .{ .w = width, .h = height };
    }

    pub fn getFullscreenMonitor(self: *Self) ?*Monitor {
        return glfwGetWindowMonitor(self);
    }

    const GLFWwindowposfun = fn (?*Self, c_int, c_int) callconv(.C) void;
    const GLFWwindowsizefun = fn (?*Self, c_int, c_int) callconv(.C) void;
    const GLFWframebuffersizefun = fn (?*Self, c_int, c_int) callconv(.C) void;
    const GLFWwindowclosefun = fn (?*Self) callconv(.C) void;
    const GLFWwindowmaximizefun = fn (?*Self, c_int) callconv(.C) void;
    const GLFWcursorenterfun = fn (?*Self, c_int) callconv(.C) void;
    const GLFWwindowfocusfun = fn (?*Self, c_int) callconv(.C) void;
    const GLFWwindowiconifyfun = fn (?*Self, c_int) callconv(.C) void;
    const GLFWkeyfun = fn (?*Self, c_int, c_int, c_int, c_int) callconv(.C) void;
    const GLFWcharfun = fn (?*Self, c_uint) callconv(.C) void;
    const GLFWmousebuttonfun = fn (?*Self, c_int, c_int, c_int) callconv(.C) void;
    const GLFWcursorposfun = fn (?*Self, f64, f64) callconv(.C) void;
    const GLFWscrollfun = fn (?*Self, f64, f64) callconv(.C) void;

    extern fn glfwSetWindowPosCallback(*Self, *const GLFWwindowposfun) *const GLFWwindowposfun;
    extern fn glfwSetWindowSizeCallback(*Self, *const GLFWwindowsizefun) *const GLFWwindowsizefun;
    extern fn glfwSetFramebufferSizeCallback(*Self, *const GLFWframebuffersizefun) *const GLFWframebuffersizefun;
    extern fn glfwSetWindowCloseCallback(*Self, *const GLFWwindowclosefun) *const GLFWwindowclosefun;
    extern fn glfwSetWindowMaximizeCallback(*Self, *const GLFWwindowmaximizefun) *const GLFWwindowmaximizefun;
    extern fn glfwSetCursorEnterCallback(*Self, *const GLFWcursorenterfun) *const GLFWcursorenterfun;
    extern fn glfwSetWindowFocusCallback(*Self, *const GLFWwindowfocusfun) *const GLFWwindowfocusfun;
    extern fn glfwSetWindowIconifyCallback(*Self, *const GLFWwindowiconifyfun) *const GLFWwindowiconifyfun;
    extern fn glfwSetKeyCallback(*Self, *const GLFWkeyfun) *const GLFWkeyfun;
    extern fn glfwSetCharCallback(*Self, *const GLFWcharfun) *const GLFWcharfun;
    extern fn glfwSetMouseButtonCallback(*Self, *const GLFWmousebuttonfun) *const GLFWmousebuttonfun;
    extern fn glfwSetCursorPosCallback(*Self, *const GLFWcursorposfun) *const GLFWcursorposfun;
    extern fn glfwSetScrollCallback(*Self, *const GLFWscrollfun) *const GLFWscrollfun;

    pub fn setPosCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, Point) void) void {
        _ = glfwSetWindowPosCallback(self, if (callback) |cb| PosCallbackWrapper(T, cb).value else null);
    }
    pub fn setSizeCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, Size) void) void {
        _ = glfwSetWindowSizeCallback(self, if (callback) |cb| SizeCallbackWrapper(T, cb).value else null);
    }
    pub fn setFramebufferSizeCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, Size) void) void {
        _ = glfwSetFramebufferSizeCallback(self, if (callback) |cb| SizeCallbackWrapper(T, cb).value else null);
    }
    pub fn setCloseCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T) void) void {
        _ = glfwSetWindowCloseCallback(self, if (callback) |cb| CloseCallbackWrapper(T, cb).value else null);
    }
    pub fn setMaximizeCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, bool) void) void {
        _ = glfwSetWindowMaximizeCallback(self, if (callback) |cb| BooleanCallbackWrapper(T, cb).value else null);
    }
    pub fn setCursorEnterCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, bool) void) void {
        _ = glfwSetCursorEnterCallback(self, if (callback) |cb| BooleanCallbackWrapper(T, cb).value else null);
    }
    pub fn setFocusCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, bool) void) void {
        _ = glfwSetWindowFocusCallback(self, if (callback) |cb| BooleanCallbackWrapper(T, cb).value else null);
    }
    pub fn setIconifyCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, bool) void) void {
        _ = glfwSetWindowIconifyCallback(self, if (callback) |cb| BooleanCallbackWrapper(T, cb).value else null);
    }
    pub fn setKeyCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, Key, i32, KeyAction, KeyMods) void) void {
        _ = glfwSetKeyCallback(self, if (callback) |cb| KeyCallbackWrapper(T, cb).value else null);
    }
    pub fn setCharCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, u21) void) void {
        _ = glfwSetCharCallback(self, if (callback) |cb| CharCallbackWrapper(T, cb).value else null);
    }
    pub fn setMouseButtonCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, MouseButton, MouseButtonAction, KeyMods) void) void {
        _ = glfwSetMouseButtonCallback(self, if (callback) |cb| MouseButtonCallbackWrapper(T, cb).value else null);
    }
    pub fn setCursorPosCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, f64, f64) void) void {
        _ = glfwSetCursorPosCallback(self, if (callback) |cb| PointFCallbackWrapper(T, cb).value else null);
    }
    pub fn setScrollCallback(self: *Self, comptime T: type, comptime callback: ?fn (*T, f64, f64) void) void {
        _ = glfwSetScrollCallback(self, if (callback) |cb| PointFCallbackWrapper(T, cb).value else null);
    }

    fn PosCallbackWrapper(comptime T: type, comptime callback: fn (*T, Point) void) type {
        return struct {
            fn value(window: ?*Self, xpos: c_int, ypos: c_int) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;
                callback(obj, .{ .x = xpos, .y = ypos });
            }
        };
    }

    fn SizeCallbackWrapper(comptime T: type, comptime callback: fn (*T, Size) void) type {
        return struct {
            fn value(window: ?*Self, width: c_int, height: c_int) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;
                callback(obj, .{ .w = width, .h = height });
            }
        };
    }

    fn CloseCallbackWrapper(comptime T: type, comptime callback: fn (*T) void) type {
        return struct {
            fn value(window: ?*Self) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;
                callback(obj);
            }
        };
    }

    fn BooleanCallbackWrapper(comptime T: type, comptime callback: fn (*T, bool) void) type {
        return struct {
            fn value(window: ?*Self, val: c_int) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;
                callback(obj, val != c.GLFW_FALSE);
            }
        };
    }

    fn KeyCallbackWrapper(comptime T: type, comptime callback: fn (*T, Key, i32, KeyAction, KeyMods) void) type {
        return struct {
            fn value(window: ?*Self, key: c_int, scancode: c_int, action: c_int, mods: c_int) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;

                const k = translateKey(key);
                const a: KeyAction = switch (action) {
                    c.GLFW_PRESS => .press,
                    c.GLFW_RELEASE => .release,
                    c.GLFW_REPEAT => .repeat,
                    else => return,
                };
                const m = translateKeyMods(mods);

                callback(obj, k, scancode, a, m);
            }
        };
    }

    /// The param is a valid Unicode codepoint, so std.unicode.utf8Encode will encode it without error.
    fn CharCallbackWrapper(comptime T: type, comptime callback: fn (*T, u21) void) type {
        return struct {
            fn value(window: ?*Self, codepoint: c_uint) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;
                if (codepoint > 0x1FFFFF) return;
                var cp = @intCast(u21, codepoint);
                if (!std.unicode.utf8ValidCodepoint(cp)) return;
                callback(obj, cp);
            }
        };
    }

    fn MouseButtonCallbackWrapper(comptime T: type, comptime callback: fn (*T, MouseButton, MouseButtonAction, KeyMods) void) type {
        return struct {
            fn value(window: ?*Self, button: c_int, action: c_int, mods: c_int) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;

                const b: MouseButton = switch (button) {
                    c.GLFW_MOUSE_BUTTON_1 => .left,
                    c.GLFW_MOUSE_BUTTON_2 => .right,
                    c.GLFW_MOUSE_BUTTON_3 => .middle,
                    c.GLFW_MOUSE_BUTTON_4 => .x1,
                    c.GLFW_MOUSE_BUTTON_5 => .x2,
                    else => .unknown,
                };
                const a: MouseButtonAction = switch (action) {
                    c.GLFW_PRESS => .press,
                    c.GLFW_RELEASE => .release,
                    else => return,
                };
                const m = translateKeyMods(mods);

                callback(obj, b, a, m);
            }
        };
    }

    fn PointFCallbackWrapper(comptime T: type, comptime callback: fn (*T, f64, f64) void) type {
        return struct {
            fn value(window: ?*Self, x: f64, y: f64) callconv(.C) void {
                const obj = getUserObj(window, T) orelse return;
                callback(obj, x, y);
            }
        };
    }
};

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
        c.GLFW_KEY_UNKNOWN => .unknown,
        c.GLFW_KEY_SPACE => .space,
        c.GLFW_KEY_APOSTROPHE => .apostrophe,
        c.GLFW_KEY_COMMA => .comma,
        c.GLFW_KEY_MINUS => .hyphen,
        c.GLFW_KEY_PERIOD => .period,
        c.GLFW_KEY_SLASH => .slash,
        c.GLFW_KEY_0 => .keyboard_0,
        c.GLFW_KEY_1 => .keyboard_1,
        c.GLFW_KEY_2 => .keyboard_2,
        c.GLFW_KEY_3 => .keyboard_3,
        c.GLFW_KEY_4 => .keyboard_4,
        c.GLFW_KEY_5 => .keyboard_5,
        c.GLFW_KEY_6 => .keyboard_6,
        c.GLFW_KEY_7 => .keyboard_7,
        c.GLFW_KEY_8 => .keyboard_8,
        c.GLFW_KEY_9 => .keyboard_9,
        c.GLFW_KEY_SEMICOLON => .semicolon,
        c.GLFW_KEY_EQUAL => .equal,
        c.GLFW_KEY_A => .a,
        c.GLFW_KEY_B => .b,
        c.GLFW_KEY_C => .c,
        c.GLFW_KEY_D => .d,
        c.GLFW_KEY_E => .e,
        c.GLFW_KEY_F => .f,
        c.GLFW_KEY_G => .g,
        c.GLFW_KEY_H => .h,
        c.GLFW_KEY_I => .i,
        c.GLFW_KEY_J => .j,
        c.GLFW_KEY_K => .k,
        c.GLFW_KEY_L => .l,
        c.GLFW_KEY_M => .m,
        c.GLFW_KEY_N => .n,
        c.GLFW_KEY_O => .o,
        c.GLFW_KEY_P => .p,
        c.GLFW_KEY_Q => .q,
        c.GLFW_KEY_R => .r,
        c.GLFW_KEY_S => .s,
        c.GLFW_KEY_T => .t,
        c.GLFW_KEY_U => .u,
        c.GLFW_KEY_V => .v,
        c.GLFW_KEY_W => .w,
        c.GLFW_KEY_X => .x,
        c.GLFW_KEY_Y => .y,
        c.GLFW_KEY_Z => .z,
        c.GLFW_KEY_LEFT_BRACKET => .left_bracket,
        c.GLFW_KEY_BACKSLASH => .backslash,
        c.GLFW_KEY_RIGHT_BRACKET => .right_bracket,
        c.GLFW_KEY_GRAVE_ACCENT => .grave_accent,
        c.GLFW_KEY_WORLD_1 => .non_us_hash,
        c.GLFW_KEY_WORLD_2 => .non_us_backslash,
        c.GLFW_KEY_ESCAPE => .escape,
        c.GLFW_KEY_ENTER => .enter,
        c.GLFW_KEY_TAB => .tab,
        c.GLFW_KEY_BACKSPACE => .backspace,
        c.GLFW_KEY_INSERT => .insert,
        c.GLFW_KEY_DELETE => .delete,
        c.GLFW_KEY_RIGHT => .right_arrow,
        c.GLFW_KEY_LEFT => .left_arrow,
        c.GLFW_KEY_DOWN => .down_arrow,
        c.GLFW_KEY_UP => .up_arrow,
        c.GLFW_KEY_PAGE_UP => .page_up,
        c.GLFW_KEY_PAGE_DOWN => .page_down,
        c.GLFW_KEY_HOME => .home,
        c.GLFW_KEY_END => .end,
        c.GLFW_KEY_CAPS_LOCK => .caps_lock,
        c.GLFW_KEY_SCROLL_LOCK => .scroll_lock,
        c.GLFW_KEY_NUM_LOCK => .num_lock,
        c.GLFW_KEY_PRINT_SCREEN => .print_screen,
        c.GLFW_KEY_PAUSE => .pause,
        c.GLFW_KEY_F1 => .f1,
        c.GLFW_KEY_F2 => .f2,
        c.GLFW_KEY_F3 => .f3,
        c.GLFW_KEY_F4 => .f4,
        c.GLFW_KEY_F5 => .f5,
        c.GLFW_KEY_F6 => .f6,
        c.GLFW_KEY_F7 => .f7,
        c.GLFW_KEY_F8 => .f8,
        c.GLFW_KEY_F9 => .f9,
        c.GLFW_KEY_F10 => .f10,
        c.GLFW_KEY_F11 => .f11,
        c.GLFW_KEY_F12 => .f12,
        c.GLFW_KEY_F13 => .f13,
        c.GLFW_KEY_F14 => .f14,
        c.GLFW_KEY_F15 => .f15,
        c.GLFW_KEY_F16 => .f16,
        c.GLFW_KEY_F17 => .f17,
        c.GLFW_KEY_F18 => .f18,
        c.GLFW_KEY_F19 => .f19,
        c.GLFW_KEY_F20 => .f20,
        c.GLFW_KEY_F21 => .f21,
        c.GLFW_KEY_F22 => .f22,
        c.GLFW_KEY_F23 => .f23,
        c.GLFW_KEY_F24 => .f24,
        //GLFW_KEY_F25,
        c.GLFW_KEY_KP_0 => .keypad_0,
        c.GLFW_KEY_KP_1 => .keypad_1,
        c.GLFW_KEY_KP_2 => .keypad_2,
        c.GLFW_KEY_KP_3 => .keypad_3,
        c.GLFW_KEY_KP_4 => .keypad_4,
        c.GLFW_KEY_KP_5 => .keypad_5,
        c.GLFW_KEY_KP_6 => .keypad_6,
        c.GLFW_KEY_KP_7 => .keypad_7,
        c.GLFW_KEY_KP_8 => .keypad_8,
        c.GLFW_KEY_KP_9 => .keypad_9,
        c.GLFW_KEY_KP_DECIMAL => .keypad_decimal,
        c.GLFW_KEY_KP_DIVIDE => .keypad_divide,
        c.GLFW_KEY_KP_MULTIPLY => .keypad_multiply,
        c.GLFW_KEY_KP_SUBTRACT => .keypad_subtract,
        c.GLFW_KEY_KP_ADD => .keypad_add,
        c.GLFW_KEY_KP_ENTER => .keypad_enter,
        c.GLFW_KEY_KP_EQUAL => .keypad_equal,
        c.GLFW_KEY_LEFT_SHIFT => .left_shift,
        c.GLFW_KEY_LEFT_CONTROL => .left_control,
        c.GLFW_KEY_LEFT_ALT => .left_alt,
        c.GLFW_KEY_LEFT_SUPER => .left_gui,
        c.GLFW_KEY_RIGHT_SHIFT => .right_shift,
        c.GLFW_KEY_RIGHT_CONTROL => .right_control,
        c.GLFW_KEY_RIGHT_ALT => .right_alt,
        c.GLFW_KEY_RIGHT_SUPER => .right_gui,
        // GLFW_KEY_MENU,
        else => .unknown,
    };
}
