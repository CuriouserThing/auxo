const std = @import("std");
const auxo = @import("auxo");
const Game = @import("Game.zig");
const Graphics = @import("Graphics.zig");

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var allocator = arena.allocator();

    var game = try allocator.create(Game);
    defer allocator.destroy(game);

    var graphics = try allocator.create(Graphics);
    defer allocator.destroy(graphics);

    const config = .{
        .step_time = std.time.ns_per_s / 720,
    };
    game.* = .{ .graphics = graphics };
    var loopback_server = LoopbackServer{};
    try auxo.runLoopback(
        Game,
        Game.Table,
        config,
        game,
        LoopbackServer,
        LoopbackServer.run,
        &loopback_server,
        allocator,
    );
}

const LoopbackServer = struct {
    const ticks_per_second = 100.0;

    pub fn run(_: *LoopbackServer, ctx: *auxo.LoopbackContext) void {
        while (ctx.clientActive()) {
            if (ctx.clientConnected()) {
                while (ctx.receivePacket()) |packet| {
                    _ = packet;
                }
            }
            std.time.sleep(std.time.ns_per_s / ticks_per_second);
        }
    }
};
