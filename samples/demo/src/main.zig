const std = @import("std");
const auxo = @import("auxo");
const AppContext = auxo.AppContext;
const GraphicsContext = auxo.zgpu.GraphicsContext;

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    var allocator = arena.allocator();

    var game = try allocator.create(Game);
    defer allocator.destroy(game);

    var client = Client{ .game = game };
    try auxo.run(Client, Game, Client.Table, &client, allocator);
}

const Client = struct {
    const Table = auxo.ClientTable(Client, Game){
        // Config
        .use_imgui = true,

        // Methods
        .createWindow = createWindow,
        .init = init,
        .deinit = deinit,

        // Inner
        .game_table = Game.Table,
    };

    game: *Game,

    fn createWindow(_: *Client, _: []auxo.DisplayInfo) !auxo.WindowCreationState {
        return .{
            .title = "Auxo Demo",
            .restored_size = .{ .w = 640, .h = 480 },
        };
    }

    fn init(self: *Client, _: *AppContext, _: *GraphicsContext) !*Game {
        self.game.* = .{};
        return self.game;
    }

    fn deinit(_: *Client, _: *const AppContext, _: *Game) void {}
};

const Game = struct {
    const Table = auxo.GameTable(Game){
        // Config
        .step_time = 1.0 / 720.0,

        // Methods
        .step = step,
        .skipSteps = skipSteps,
        .draw = draw,
        .onSwapchainResized = onSwapchainResized,
        .receiveCloseRequest = receiveCloseRequest,
        .receiveFocusState = receiveFocusState,
        .receiveIconifyState = receiveIconifyState,
        .receiveKeyAction = receiveKeyAction,
        .receiveCharInput = receiveCharInput,
        .receiveMouseButtonAction = receiveMouseButtonAction,
        .receiveMouseScroll = receiveMouseScroll,
        .receiveCursorPosition = receiveCursorPosition,
        .receiveCursorEntryState = receiveCursorEntryState,
        .receiveJoyState = receiveJoyState,
        .receiveDisplayState = receiveDisplayState,
        .receiveServerData = receiveServerData,
    };

    fn step(_: *Game, _: *AppContext) void {}

    fn skipSteps(_: *Game, _: *AppContext, _: usize) void {}

    fn draw(_: *Game, _: *AppContext, _: *GraphicsContext, _: auxo.FrameInfo) void {}

    fn onSwapchainResized(_: *Game, _: *AppContext, _: *GraphicsContext, _: auxo.Size) void {}

    fn receiveCloseRequest(_: *Game, app: *AppContext, _: auxo.CloseRequestArgs) void {
        app.close();
    }

    fn receiveFocusState(_: *Game, _: *AppContext, _: auxo.FocusStateArgs) void {}

    fn receiveIconifyState(_: *Game, _: *AppContext, _: auxo.IconifyStateArgs) void {}

    fn receiveKeyAction(_: *Game, _: *AppContext, _: auxo.KeyActionArgs) void {}

    fn receiveCharInput(_: *Game, _: *AppContext, _: auxo.CharInputArgs) void {}

    fn receiveMouseButtonAction(_: *Game, _: *AppContext, _: auxo.MouseButtonActionArgs) void {}

    fn receiveMouseScroll(_: *Game, _: *AppContext, _: auxo.MouseScrollArgs) void {}

    fn receiveCursorPosition(_: *Game, _: *AppContext, _: auxo.CursorPositionArgs) void {}

    fn receiveCursorEntryState(_: *Game, _: *AppContext, _: auxo.CursorEntryStateArgs) void {}

    fn receiveJoyState(_: *Game, _: *AppContext, _: auxo.JoyStateArgs) void {}

    fn receiveDisplayState(_: *Game, _: *AppContext, _: auxo.DisplayStateArgs) void {}

    fn receiveServerData(_: *Game, _: *AppContext, _: auxo.ServerDataArgs) void {}
};
