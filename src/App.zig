const std = @import("std");
const Allocator = std.mem.Allocator;
const Timer = std.time.Timer;

const io = @import("io.zig");
const Rectangle = io.Rectangle;

const glad = @import("glad.zig");
const glfw = @import("glfw.zig");

const monitors = @import("monitors.zig");
const windowing = @import("windowing.zig");
const Window = windowing.Window;
const App = @This();

pub const Model = struct {
    timestep_model: TimestepModel,
    render_model: RenderModel,
    event_handler: EventHandler,
    window_event_handler: Window.EventHandler,
};

pub const TimestepModel = struct {
    steps_per_second: f64,
    stepFn: comptime fn () void,
    max_steps_per_frame: ?u64 = null,
    max_seconds_per_frame: ?f64 = null,
    stepsMissedCallback: ?comptime fn (u64) void = null,
};

pub const RenderModel = struct {
    initFn: ?comptime fn (*RenderContext) anyerror!void = null,
    renderFn: comptime fn (*RenderContext, FrameInfo) void,
    deinitFn: ?comptime fn () void = null,
};

pub const EventHandler = struct {
    loopCallback: comptime fn (AppContext) void,
};

pub const RenderContext = struct {
    swap_interval: ?i32 = null,
};

pub const FrameInfo = struct {
    suggested_viewport: Rectangle,
    step_remainder: f64 = 0.0,
    estimated_fps: ?f64 = null,
};

pub const AppContext = struct {
    window: *Window,
};

model: Model,
window: *Window,

aborted: bool = false,
inner_started: bool = false,
outer_stopped: bool = false,
step_count: u64 = 0,
step_remainder: f64 = 0.0,

pub fn run(model: Model, allocator: *Allocator, windowStateFactory: fn () Window.CreationState) !void {
    logGlfwVersion();
    glfw.setErrorCallback(onGlfwError);

    try glfw.init();
    defer glfw.terminate();

    defer monitors.freeAll();

    const window_state = windowStateFactory();
    const window = try windowing.createFromState(allocator, window_state, model.window_event_handler);
    defer windowing.destroy(window);

    var app = App{
        .model = model,
        .window = window,
    };
    try app.runOuter();
}

fn runOuter(self: *App) !void {
    glfw.detachCurrentContext(); // may not be necessary but detach from the event thread just in case
    const inner_thread = try std.Thread.spawn(runInner, self);

    while (!self.aborted and !self.inner_started) {
        glfw.waitEvents();
    }

    while (!self.aborted and !self.window.should_destroy) {
        self.model.event_handler.loopCallback(.{
            .window = self.window,
        });
        glfw.waitEvents();
    }

    self.outer_stopped = true;
    inner_thread.wait();
}

fn runInner(self: *App) !void {
    const window = self.window;
    const stepper = self.model.timestep_model;
    const renderer = self.model.render_model;

    errdefer self.aborted = true;

    window.handle.makeContextCurrent();
    try glad.loadGl(glfw.getProcAddress);

    if (renderer.initFn) |init| {
        var ctx = RenderContext{ .swap_interval = window.swap_interval };
        try init(&ctx);
        if (ctx.swap_interval) |interval| {
            glfw.swapInterval(interval);
            window.swap_interval = interval;
        }
    }
    defer if (renderer.deinitFn) |deinit| deinit();

    var accumulator: u64 = 0;
    var last_timestamp: u64 = 0;
    var timer = try Timer.start();
    self.inner_started = true;

    while (!self.aborted and !self.outer_stopped) {
        const timestamp = timer.read();

        const max_int = comptime std.math.maxInt(u64);
        if (@addWithOverflow(u64, accumulator, timestamp - last_timestamp, &accumulator)) accumulator = max_int;
        last_timestamp = timestamp;
        const ticks_per_second = comptime @intToFloat(f64, std.time.ns_per_s);
        const f_ticks_per_step = @round(ticks_per_second / stepper.steps_per_second);
        const ticks_per_step = if (f_ticks_per_step < max_int) @floatToInt(u64, f_ticks_per_step) else max_int;

        // There are [currently] two limits on stepping within a frame:
        // 1. We can't take more steps in any single frame than the user lets us.
        // 2. We can't take more steps in a frame if we've exceeded the total stepping time the user allots us.
        const max_steps = stepper.max_steps_per_frame orelse max_int;
        const max_timestamp = getMaxTimestamp(timestamp, stepper.max_seconds_per_frame, ticks_per_second);

        var steps: u64 = 0;
        while (steps < max_steps and accumulator >= ticks_per_step) {
            accumulator -= ticks_per_step;
            stepper.stepFn();
            self.step_count += 1;
            steps += 1;

            const step_timestamp = timer.read();
            if (step_timestamp >= max_timestamp) break;
        }

        var steps_missed = accumulator / ticks_per_step;
        if (steps_missed > 0) {
            accumulator -= ticks_per_step * steps_missed;
            if (stepper.stepsMissedCallback) |callback| callback(steps_missed);
        }

        self.step_remainder = @intToFloat(f64, accumulator) / @intToFloat(f64, ticks_per_step);
        windowing.render(window, *App, render, self);
        glfw.postEmptyEvent();
    }
}

fn render(self: *App, suggested_viewport: Rectangle, estimated_fps: ?f64) void {
    var ctx = RenderContext{ .swap_interval = self.window.swap_interval };
    self.model.render_model.renderFn(&ctx, .{
        .suggested_viewport = suggested_viewport,
        .step_remainder = self.step_remainder,
        .estimated_fps = estimated_fps,
    });
    if (ctx.swap_interval) |interval| {
        glfw.swapInterval(interval);
        self.window.swap_interval = interval;
    }
}

fn getMaxTimestamp(timestamp: u64, max_seconds_per_frame: ?f64, comptime ticks_per_second: f64) u64 {
    const max_int = comptime std.math.maxInt(u64);
    const mspf = max_seconds_per_frame orelse return max_int;
    const max_ticks_per_frame = @round(mspf * ticks_per_second);
    const mtpf = if (max_ticks_per_frame < max_int) @floatToInt(u64, max_ticks_per_frame) else return max_int;
    var max_timestamp: u64 = undefined;
    return if (@addWithOverflow(u64, timestamp, mtpf, &max_timestamp)) max_int else max_timestamp;
}

fn logGlfwVersion() void {
    std.log.info("Compiled against GLFW {}.{}.{}", .{ glfw.versionMajor, glfw.versionMinor, glfw.versionRevision });

    var major: i32 = 0;
    var minor: i32 = 0;
    var revision: i32 = 0;
    glfw.getVersion(&major, &minor, &revision);
    const version_str = glfw.getVersionString();
    std.log.info("Running against GLFW {}.{}.{} ({s})", .{ major, minor, revision, version_str });
}

fn onGlfwError(error_code: glfw.Error, description: []const u8) void {
    std.log.err("GLFW error {s}: {s}", .{ @errorName(error_code), description });
}
