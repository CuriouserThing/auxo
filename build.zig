const std = @import("std");
const netcode = @import("lib/zig-netcode/build.zig");
const zpool = @import("lib/zig-gamedev/libs/zpool/build.zig");
const zglfw = @import("lib/zig-gamedev/libs/zglfw/build.zig");
const zgpu = @import("lib/zig-gamedev/libs/zgpu/build.zig");
const zgui = @import("lib/zig-gamedev/libs/zgui/build.zig");
const zmath = @import("lib/zig-gamedev/libs/zmath/build.zig");
const zaudio = @import("lib/zig-gamedev/libs/zaudio/build.zig");
const demo = @import("samples/demo/build.zig");

pub fn build(b: *std.build.Builder) void {
    demo.build(b);
}

pub fn linkTo(step: *std.build.LibExeObjStep) !void {
    try netcode.linkTo(step);

    var b = step.builder;
    zglfw.link(step);
    zgpu.link(step, zgpuOptions(b));
    zgui.link(step, zguiOptions(b));
    zaudio.link(step);
}

// Ideally we return a package here but there's a nasty corner case compiler memory ownership quirk
pub fn addTo(step: *std.build.LibExeObjStep, pkg_name: []const u8) void {
    var b = step.builder;
    const zgpu_pkg = zgpu.getPkg(&.{ zgpuOptions(b).getPkg(), zpool.pkg, zglfw.pkg });
    const zgui_pkg = zgui.getPkg(&.{zguiOptions(b).getPkg()});
    const pkg = .{
        .name = pkg_name,
        .source = .{ .path = src() ++ "/src/main.zig" },
        .dependencies = &[_]std.build.Pkg{
            netcode.package("netcode"),
            zgpu_pkg,
            zgui_pkg,
            zglfw.pkg,
            zaudio.pkg,
        },
    };
    step.addPackage(pkg);
}

fn zgpuOptions(b: *std.build.Builder) zgpu.BuildOptionsStep {
    return zgpu.BuildOptionsStep.init(b, .{});
}

fn zguiOptions(b: *std.build.Builder) zgui.BuildOptionsStep {
    return zgui.BuildOptionsStep.init(b, .{ .backend = .glfw_wgpu });
}

inline fn src() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}
