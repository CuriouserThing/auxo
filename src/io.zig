const std = @import("std");
const math = std.math;

pub const Cardinal = enum { north, east, south, west };

pub const Point = struct {
    x: i32,
    y: i32,

    pub const zero = Point{ .x = 0, .y = 0 };
};

pub const Size = struct {
    w: i32,
    h: i32,

    pub const zero = Size{ .w = 0, .h = 0 };
};

pub const Rectangle = struct {
    pos: Point,
    size: Size,

    pub const zero = Rectangle{ .pos = Point.zero, .size = Size.zero };
};

// Maximum number of system displays recognized. Others will be silently ignored.
pub const DISPLAY_MAX = 16;

pub const DisplayInfo = struct {
    display_area: Rectangle,
    work_area: Rectangle,
    refresh_rate: f32,
    scale_factor: f32,
};

pub const MouseButtonAction = enum {
    press,
    release,
};

pub const MouseButton = enum {
    unknown,
    left,
    right,
    middle,
    x1,
    x2,
};

// =====================================================================================================================
// JOYSTICKS

// Maximum number of system joysticks recognized. Others will be silently ignored.
pub const JOYSTICK_MAX = 16;

pub const JoyInfo = struct {
    vid: ?u16 = null,
    pid: ?u16 = null,
    is_xinput: bool = false,
};

/// Maximum number of buttons recognized on each joystick. Others will be silently ignored.
pub const JOY_BUTTON_MAX = 128;
/// Maximum number of axes recognized on each joystick. Others will be silently ignored.
pub const JOY_AXIS_MAX = 8;
/// Maximum number of hats recognized on each joystick. Others will be silently ignored.
pub const JOY_HAT_MAX = 4;

pub const JoyButtonState = enum {
    released,
    pressed,
};

pub const JoyHatDirection = enum(i4) {
    north,
    northeast,
    east,
    southeast,
    south,
    southwest,
    west,
    northwest,
    centered = -1,

    pub fn isNorth(self: JoyHatDirection) bool {
        return self == .northwest or self == .north or self == .northeast;
    }

    pub fn isEast(self: JoyHatDirection) bool {
        return self == .northeast or self == .east or self == .southeast;
    }

    pub fn isSouth(self: JoyHatDirection) bool {
        return self == .southeast or self == .south or self == .southwest;
    }

    pub fn isWest(self: JoyHatDirection) bool {
        return self == .southwest or self == .west or self == .northwest;
    }
};

pub const JoyInputSource = enum {
    button,
    axis,
    hat_north,
    hat_east,
    hat_south,
    hat_west,
};

pub const JoyInputIndex = std.meta.Int(.unsigned, math.log2_int_ceil(usize, @max(JOY_BUTTON_MAX, @max(JOY_AXIS_MAX, JOY_HAT_MAX))));

pub const JoyInput = struct {
    source: JoyInputSource,
    index: JoyInputIndex,
    zero_magnitude: f32 = 0.0,
    full_magnitude: f32 = 1.0,
};

pub const GamepadButton = enum {
    /// The rightmost interface button in the middle of the gamepad. Typically labeled "START", "OPTIONS", menu shape, or plus shape.
    gui_primary,
    /// The leftmost interface button in the middle of the gamepad. Typically labeled "SELECT", "BACK", view shape, or minus shape.
    gui_secondary,
    /// The right face button in a four-button diamond configuration. Typically labeled "A", "B", or circle shape.
    diamond_right,
    /// The bottom face button in a four-button diamond configuration. Typically labeled "B", "A", or cross shape.
    diamond_bottom,
    /// The top face button in a four-button diamond configuration. Typically labeled "X", "Y", or triangle shape.
    diamond_top,
    /// The left face button in a four-button diamond configuration. Typically labeled "Y", "X", or square shape.
    diamond_left,
    /// The left shoulder digital bumper, or the upper one if there are two. Typically labeled "L", "LB", or "L1".
    left_bumper,
    /// The right shoulder digital bumper, or the upper one if there are two. Typically labeled "R", "RB", or "R1".
    right_bumper,
    /// The lower left shoulder digital bumper. Typically labeled "ZL" or "L2".
    left_bumper_2,
    /// The lower right shoulder digital bumper. Typically labeled "ZR" or "R2".
    right_bumper_2,
    /// The button actuated by pressing the left analog stick. Typically unlabeled or labeled "L3".
    left_stick,
    /// The button actuated by pressing the right analog stick. Typically unlabeled or labeled "R3".
    right_stick,
};

pub const GamepadTrigger = enum {
    /// The left shoulder analog trigger. Typically labeled "L", "LT" or "L2".
    left,
    /// The right shoulder analog trigger. Typically labeled "R", "RT" or "R2".
    right,
};

pub const GamepadStick = enum {
    /// The left analog stick.
    left,
    /// The right analog stick.
    right,
};

pub const GamepadHat = enum {
    /// The directional pad (d-pad).
    dpad,
};

pub const GamepadMapping = struct {
    const Self = @This();
    fn enumLength(comptime T: type) comptime_int {
        var max: usize = 0;
        for (std.meta.fields(T)) |field| {
            max = @max(max, field.value);
        }
        return max + 1;
    }
    const BUTTON_COUNT = enumLength(GamepadButton);
    const TRIGGER_COUNT = enumLength(GamepadTrigger);
    const STICK_COUNT = enumLength(GamepadStick);
    const HAT_COUNT = enumLength(GamepadHat);

    buttons: [BUTTON_COUNT]?JoyInput = [_]?JoyInput{null} ** BUTTON_COUNT,
    triggers: [TRIGGER_COUNT]?JoyInput = [_]?JoyInput{null} ** TRIGGER_COUNT,
    stick_cardinals: [STICK_COUNT][4]?JoyInput = [_][4]?JoyInput{[_]?JoyInput{null} ** 4} ** STICK_COUNT,
    hat_cardinals: [HAT_COUNT][4]?JoyInput = [_][4]?JoyInput{[_]?JoyInput{null} ** 4} ** HAT_COUNT,

    pub fn setButton(self: *Self, button: GamepadButton, input: ?JoyInput) void {
        self.buttons[@enumToInt(button)] = input;
    }

    pub fn setTrigger(self: *Self, trigger: GamepadTrigger, input: ?JoyInput) void {
        self.triggers[@enumToInt(trigger)] = input;
    }

    pub fn setStickCardinal(self: *Self, stick: GamepadStick, cardinal: Cardinal, input: ?JoyInput) void {
        self.stick_cardinals[@enumToInt(stick)][@enumToInt(cardinal)] = input;
    }

    pub fn setHatCardinal(self: *Self, hat: GamepadHat, cardinal: Cardinal, input: ?JoyInput) void {
        self.hat_cardinals[@enumToInt(hat)][@enumToInt(cardinal)] = input;
    }

    pub const xinput: GamepadMapping = result: {
        var m = GamepadMapping{};
        m.setButton(.diamond_bottom, .{ .source = .button, .index = 0 });
        m.setButton(.diamond_right, .{ .source = .button, .index = 1 });
        m.setButton(.diamond_left, .{ .source = .button, .index = 2 });
        m.setButton(.diamond_top, .{ .source = .button, .index = 3 });
        m.setButton(.left_bumper, .{ .source = .button, .index = 4 });
        m.setButton(.right_bumper, .{ .source = .button, .index = 5 });
        m.setButton(.gui_secondary, .{ .source = .button, .index = 6 });
        m.setButton(.gui_primary, .{ .source = .button, .index = 7 });
        m.setButton(.left_stick, .{ .source = .button, .index = 8 });
        m.setButton(.right_stick, .{ .source = .button, .index = 9 });
        m.setStickCardinal(.left, .west, .{ .source = .axis, .index = 0, .full_magnitude = -1.0 });
        m.setStickCardinal(.left, .east, .{ .source = .axis, .index = 0 });
        m.setStickCardinal(.left, .north, .{ .source = .axis, .index = 1, .full_magnitude = -1.0 });
        m.setStickCardinal(.left, .south, .{ .source = .axis, .index = 1 });
        m.setStickCardinal(.right, .west, .{ .source = .axis, .index = 2, .full_magnitude = -1.0 });
        m.setStickCardinal(.right, .east, .{ .source = .axis, .index = 2 });
        m.setStickCardinal(.right, .north, .{ .source = .axis, .index = 3, .full_magnitude = -1.0 });
        m.setStickCardinal(.right, .south, .{ .source = .axis, .index = 3 });
        m.setTrigger(.left, .{ .source = .axis, .index = 4, .zero_magnitude = -1.0 });
        m.setTrigger(.right, .{ .source = .axis, .index = 5, .zero_magnitude = -1.0 });
        m.setHatCardinal(.dpad, .north, .{ .source = .hat_north, .index = 0 });
        m.setHatCardinal(.dpad, .east, .{ .source = .hat_east, .index = 0 });
        m.setHatCardinal(.dpad, .south, .{ .source = .hat_south, .index = 0 });
        m.setHatCardinal(.dpad, .west, .{ .source = .hat_west, .index = 0 });
        break :result m;
    };
};

pub const GamepadStickState = struct {
    direction: f32,
    magnitude: f32,

    pub fn x(self: GamepadStickState) f32 {
        return self.magnitude * math.sin(self.direction);
    }

    pub fn y(self: GamepadStickState) f32 {
        return self.magnitude * -math.cos(self.direction);
    }
};

pub const JoyState = struct {
    const Self = @This();
    const ACTUATION_THRESHOLD = 0.5;

    buttons: []JoyButtonState,
    axes: []f32,
    hats: []JoyHatDirection,

    // =================================================================================================================
    // JOY GETTERS

    pub fn getButton(self: Self, button: JoyInputIndex) ?JoyButtonState {
        if (self.buttons.len <= button) return null;
        return self.buttons[button];
    }

    pub fn getAxis(self: Self, axis: JoyInputIndex) ?f32 {
        if (self.axes.len <= axis) return null;
        return self.axes[axis];
    }

    pub fn getHat(self: Self, hat: JoyInputIndex) ?JoyHatDirection {
        if (self.hats.len <= hat) return null;
        return self.hats[hat];
    }

    // =================================================================================================================
    // GAMEPAD HELPERS

    /// {0, 1}
    fn getButtonInputMagnitude(self: Self, button: JoyInputIndex) ?f32 {
        if (self.buttons.len <= button) return null;
        return switch (self.buttons[button]) {
            .released => 0.0,
            .pressed => 1.0,
        };
    }

    /// [-1, +1]
    fn getAxisInputMagnitude(self: Self, axis: JoyInputIndex) ?f32 {
        if (self.axes.len <= axis) return null;
        return self.axes[axis];
    }

    /// {0, 1}
    fn getHatCardinalInputMagnitude(self: Self, hat: JoyInputIndex, cardinal: Cardinal) ?f32 {
        if (self.hats.len <= hat) return null;
        return if (switch (self.hats[hat]) {
            .north => cardinal == .north,
            .northeast => cardinal == .north or cardinal == .east,
            .east => cardinal == .east,
            .southeast => cardinal == .east or cardinal == .south,
            .south => cardinal == .south,
            .southwest => cardinal == .south or cardinal == .west,
            .west => cardinal == .west,
            .northwest => cardinal == .west or cardinal == .north,
            .centered => false,
        }) 1.0 else 0.0;
    }

    /// [0, 1]
    fn getInputMagnitude(self: Self, input: JoyInput) ?f32 {
        const m = switch (input.source) {
            .button => self.getButtonInputMagnitude(input.index),
            .axis => self.getAxisInputMagnitude(input.index),
            .hat_north => self.getHatCardinalInputMagnitude(input.index, .north),
            .hat_east => self.getHatCardinalInputMagnitude(input.index, .east),
            .hat_south => self.getHatCardinalInputMagnitude(input.index, .south),
            .hat_west => self.getHatCardinalInputMagnitude(input.index, .west),
        } orelse return null;
        const z = input.zero_magnitude;
        const f = input.full_magnitude;

        // Map and clamp the range
        return @min(1.0, @max(0.0, (m - z) / (f - z)));
    }

    /// x pointing north and y pointing east, [-1, +1]
    fn get2dInputMagnitude(self: Self, input_cardinals: [4]?JoyInput) ?[2]f32 {
        const ni = input_cardinals[@enumToInt(Cardinal.north)] orelse return null;
        const ei = input_cardinals[@enumToInt(Cardinal.east)] orelse return null;
        const si = input_cardinals[@enumToInt(Cardinal.south)] orelse return null;
        const wi = input_cardinals[@enumToInt(Cardinal.west)] orelse return null;

        // [0, 1] cardinals
        const nm = self.getInputMagnitude(ni) orelse return null;
        const em = self.getInputMagnitude(ei) orelse return null;
        const sm = self.getInputMagnitude(si) orelse return null;
        const wm = self.getInputMagnitude(wi) orelse return null;

        // [-1, +1] axes
        const x = nm - sm;
        const y = em - wm;
        return [_]f32{ x, y };
    }

    // =================================================================================================================
    // GAMEPAD GETTERS

    pub fn getGamepadButton(self: Self, mapping: GamepadMapping, button: GamepadButton) ?JoyButtonState {
        const input = mapping.buttons[@enumToInt(button)] orelse return null;
        const m = self.getInputMagnitude(input) orelse return null;
        return if (m > ACTUATION_THRESHOLD) .pressed else .released;
    }

    pub fn getGamepadTrigger(self: Self, mapping: GamepadMapping, trigger: GamepadTrigger) ?f32 {
        const input = mapping.triggers[@enumToInt(trigger)] orelse return null;
        return self.getInputMagnitude(input);
    }

    pub fn getGamepadStick(self: Self, mapping: GamepadMapping, stick: GamepadStick) ?GamepadStickState {
        const input_cardinals = mapping.stick_cardinals[@enumToInt(stick)];
        const xy = self.get2dInputMagnitude(input_cardinals) orelse return null;
        const x = xy[0];
        const y = xy[1];
        return .{
            .direction = math.atan2(f32, y, x),
            .magnitude = @max(@max(x, -x), @max(y, -y)),
        };
    }

    pub fn getGamepadHat(self: Self, mapping: GamepadMapping, hat: GamepadHat) ?JoyHatDirection {
        const input_cardinals = mapping.hat_cardinals[@enumToInt(hat)];
        const xy = self.get2dInputMagnitude(input_cardinals) orelse return null;
        const north = xy[0] > ACTUATION_THRESHOLD;
        const south = xy[0] < -ACTUATION_THRESHOLD;
        const east = xy[1] > ACTUATION_THRESHOLD;
        const west = xy[1] < -ACTUATION_THRESHOLD;
        if (north) {
            if (east) return .northeast else if (west) return .northwest else return .north;
        } else if (south) {
            if (east) return .southeast else if (west) return .southwest else return .south;
        } else {
            if (east) return .east else if (west) return .west else return .centered;
        }
    }
};

// =====================================================================================================================
// KEYS

pub const KeyAction = enum {
    release,
    press,
    repeat,
};

pub const KeyMods = struct {
    control: bool,
    shift: bool,
    alt: bool,
    gui: bool,
    caps_lock: bool,
    num_lock: bool,
};

/// A non-exhaustive mapping of keys and USB codes (HID Usage Table 0x07)
pub const Key = enum(u8) {
    unknown = 0x00,
    // 0x01 Keyboard ErrorRollOver
    // 0x02 Keyboard POSTFail
    // 0x03 Keyboard ErrorUndefined
    a = 0x04,
    b = 0x05,
    c = 0x06,
    d = 0x07,
    e = 0x08,
    f = 0x09,
    g = 0x0a,
    h = 0x0b,
    i = 0x0c,
    j = 0x0d,
    k = 0x0e,
    l = 0x0f,
    m = 0x10,
    n = 0x11,
    o = 0x12,
    p = 0x13,
    q = 0x14,
    r = 0x15,
    s = 0x16,
    t = 0x17,
    u = 0x18,
    v = 0x19,
    w = 0x1a,
    x = 0x1b,
    y = 0x1c,
    z = 0x1d,
    keyboard_1 = 0x1e,
    keyboard_2 = 0x1f,
    keyboard_3 = 0x20,
    keyboard_4 = 0x21,
    keyboard_5 = 0x22,
    keyboard_6 = 0x23,
    keyboard_7 = 0x24,
    keyboard_8 = 0x25,
    keyboard_9 = 0x26,
    keyboard_0 = 0x27,
    enter = 0x28,
    escape = 0x29,
    backspace = 0x2a,
    tab = 0x2b,
    space = 0x2c,
    hyphen = 0x2d,
    equal = 0x2e,
    left_bracket = 0x2f,
    right_bracket = 0x30,
    backslash = 0x31,
    non_us_hash = 0x32,
    semicolon = 0x33,
    apostrophe = 0x34,
    grave_accent = 0x35,
    comma = 0x36,
    period = 0x37,
    slash = 0x38,
    caps_lock = 0x39,
    f1 = 0x3a,
    f2 = 0x3b,
    f3 = 0x3c,
    f4 = 0x3d,
    f5 = 0x3e,
    f6 = 0x3f,
    f7 = 0x40,
    f8 = 0x41,
    f9 = 0x42,
    f10 = 0x43,
    f11 = 0x44,
    f12 = 0x45,
    print_screen = 0x46,
    scroll_lock = 0x47,
    pause = 0x48,
    insert = 0x49,
    home = 0x4a,
    page_up = 0x4b,
    delete = 0x4c,
    end = 0x4d,
    page_down = 0x4e,
    right_arrow = 0x4f,
    left_arrow = 0x50,
    down_arrow = 0x51,
    up_arrow = 0x52,
    num_lock = 0x53,
    keypad_divide = 0x54,
    keypad_multiply = 0x55,
    keypad_subtract = 0x56,
    keypad_add = 0x57,
    keypad_enter = 0x58,
    keypad_1 = 0x59,
    keypad_2 = 0x5a,
    keypad_3 = 0x5b,
    keypad_4 = 0x5c,
    keypad_5 = 0x5d,
    keypad_6 = 0x5e,
    keypad_7 = 0x5f,
    keypad_8 = 0x60,
    keypad_9 = 0x61,
    keypad_0 = 0x62,
    keypad_decimal = 0x63,
    non_us_backslash = 0x64,
    application = 0x65,
    // 0x66 Keyboard Power
    keypad_equal = 0x67,
    f13 = 0x68,
    f14 = 0x69,
    f15 = 0x6a,
    f16 = 0x6b,
    f17 = 0x6c,
    f18 = 0x6d,
    f19 = 0x6e,
    f20 = 0x6f,
    f21 = 0x70,
    f22 = 0x71,
    f23 = 0x72,
    f24 = 0x73,

    left_control = 0xe0,
    left_shift = 0xe1,
    left_alt = 0xe2,
    left_gui = 0xe3,
    right_control = 0xe4,
    right_shift = 0xe5,
    right_alt = 0xe6,
    right_gui = 0xe7,
};
