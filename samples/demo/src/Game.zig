const std = @import("std");
const auxo = @import("auxo");
const Graphics = @import("Graphics.zig");

const Game = @This();
const App = auxo.App;
const gui = auxo.gui;

pub const Table = auxo.GameTable(Game){
    .step = step,
    .draw = draw,
    .handleSkippedSteps = handleSkippedSteps,
    .handleWindowOpen = handleWindowOpen,
    .handleCloseRequest = handleCloseRequest,
    .handleGpuDeviceCreation = handleGpuDeviceCreation,
    .handleGpuDeviceLoss = handleGpuDeviceLoss,
    .handleSwapChainCreation = handleSwapChainCreation,
    .handleSwapChainDestruction = handleSwapChainDestruction,
    .receiveKeyAction = receiveKeyAction,
};

graphics: *Graphics,
steps: u64 = 0,
steps_skipped: u64 = 0,
brightness: f32 = 0.0,

fn step(game: *Game, _: *App, _: auxo.StepContext) void {
    const cycle = 1440;
    var b = @as(f32, @floatFromInt(game.steps % cycle)) / @as(f32, @floatFromInt(cycle / 2));
    if (b > 1.0) b = 2.0 - b;
    game.brightness = b;
    game.steps += 1;
}

fn draw(game: *Game, _: *App, ctx: auxo.DrawContext) void {
    if (auxo.options.use_imgui) {
        ctx.guiNewFrame();
        if (gui.begin("Demo", .{})) {
            if (ctx.estimated_fps) |fps| {
                gui.bulletText(
                    "FPS: {d:.1}",
                    .{fps},
                );
            } else {
                gui.bulletText(
                    "FPS: ?",
                    .{},
                );
            }
            gui.bulletText("Steps: {}", .{game.steps});
            gui.bulletText("Steps skipped: {}", .{game.steps_skipped});

            gui.newLine();

            gui.text("Hoykeys:", .{});
            gui.bulletText("1: Set to 640x480 windowed", .{});
            gui.bulletText("2: Set to 1280x720 windowed", .{});
            gui.bulletText("3: Set to maximized", .{});
            gui.bulletText("4: Set borderless fullscreen", .{});
            gui.bulletText("5: Set exclusive fullscreen", .{});
            gui.bulletText("F: Toggle fullscreen", .{});
        }
        gui.end();
    }

    game.graphics.draw(ctx, game.brightness);
}

fn handleSkippedSteps(game: *Game, _: *App, ctx: auxo.SkippedStepsContext) void {
    game.steps_skipped += ctx.count;
}

fn handleWindowOpen(_: *Game, app: *App, _: auxo.WindowOpenContext) void {
    app.setTitle("auxo demo");
}

fn handleCloseRequest(_: *Game, app: *App, _: auxo.CloseRequestContext) void {
    app.close();
}

fn handleGpuDeviceCreation(game: *Game, _: *App, ctx: auxo.GpuDeviceCreationContext) void {
    game.graphics.* = Graphics.init(ctx);
}

fn handleGpuDeviceLoss(_: *Game, _: *App, _: auxo.GpuDeviceLossContext) void {}

fn handleSwapChainCreation(_: *Game, _: *App, _: auxo.SwapChainCreationContext) void {}

fn handleSwapChainDestruction(_: *Game, _: *App, _: auxo.SwapChainDestructionContext) void {}

fn receiveKeyAction(_: *Game, app: *App, args: auxo.KeyActionArgs) void {
    if (args.action == .press) {
        switch (args.key) {
            .keyboard_1 => {
                app.setWindowed(.{ .maximized = false, .restored_size = .{ .w = 640, .h = 480 } });
            },
            .keyboard_2 => {
                app.setWindowed(.{ .maximized = false, .restored_size = .{ .w = 1280, .h = 720 } });
            },
            .keyboard_3 => {
                app.setWindowed(.{ .maximized = true });
            },
            .keyboard_4 => {
                app.setFullscreen(.{ .exclusive = false });
            },
            .keyboard_5 => {
                app.setFullscreen(.{ .exclusive = true });
            },
            .f => {
                app.toggleFullscreen(.{}, .{});
            },
            else => {},
        }
    }
}
