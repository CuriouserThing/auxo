pub const App = @import("App.zig");
pub const Window = @import("windowing.zig").Window;
pub const Monitor = @import("monitors.zig").Monitor;

const io = @import("io.zig");
pub const Point = io.Point;
pub const Size = io.Size;
pub const Rectangle = io.Rectangle;
pub const VideoMode = io.VideoMode;
pub const KeyMods = io.KeyMods;
pub const MouseButtonAction = io.MouseButtonAction;
pub const MouseButton = io.MouseButton;
pub const KeyAction = io.KeyAction;
pub const Key = io.Key;
