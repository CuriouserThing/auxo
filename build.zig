const std = @import("std");
const netcode = @import("lib/zig-netcode/build.zig");
const zpool = @import("lib/zig-gamedev/libs/zpool/build.zig");
const zglfw = @import("lib/zig-gamedev/libs/zglfw/build.zig");
const zgpu = @import("lib/zig-gamedev/libs/zgpu/build.zig");
const zgui = @import("lib/zig-gamedev/libs/zgui/build.zig");
const zmesh = @import("lib/zig-gamedev/libs/zmesh/build.zig");
const zmath = @import("lib/zig-gamedev/libs/zmath/build.zig");
const zaudio = @import("lib/zig-gamedev/libs/zaudio/build.zig");
const demo = @import("samples/demo/build.zig");

pub fn build(b: *std.Build) void {
    demo.build(b);
}

pub const Package = struct {
    module: *std.Build.Module,
    netcode_package: netcode.Package,
    zglfw_package: zglfw.Package,
    zgpu_package: zgpu.Package,
    zgui_package: zgui.Package,
    zmesh_package: zmesh.Package,
    zaudio_package: zaudio.Package,

    pub fn build(b: *std.Build, target: std.zig.CrossTarget, optimize: std.builtin.Mode) !Package {
        const netcode_package = try netcode.Package.build(b, target, optimize);
        const zglfw_package = zglfw.package(b, target, optimize, .{});
        const zgpu_package = zgpu.package(b, target, optimize, .{
            .deps = .{ .zglfw = zglfw_package.zglfw, .zpool = zpool.package(b, target, optimize, .{}).zpool },
        });
        const zgui_package = zgui.package(b, target, optimize, .{ .options = .{ .backend = .glfw_wgpu } });
        const zmesh_package = zmesh.package(b, target, optimize, .{});
        const zaudio_package = zaudio.package(b, target, optimize, .{});

        const module = b.createModule(.{
            .source_file = .{ .path = src() ++ "/src/main.zig" },
            .dependencies = &.{
                .{ .name = "netcode", .module = netcode_package.module },
                .{ .name = "zglfw", .module = zglfw_package.zglfw },
                .{ .name = "zgpu", .module = zgpu_package.zgpu },
                .{ .name = "zgui", .module = zgui_package.zgui },
                .{ .name = "zmesh", .module = zmesh_package.zmesh },
                .{ .name = "zaudio", .module = zaudio_package.zaudio },
                .{ .name = "zmath", .module = zmath.package(b, target, optimize, .{}).zmath },
            },
        });

        return Package{
            .module = module,
            .netcode_package = netcode_package,
            .zglfw_package = zglfw_package,
            .zgpu_package = zgpu_package,
            .zgui_package = zgui_package,
            .zmesh_package = zmesh_package,
            .zaudio_package = zaudio_package,
        };
    }

    pub fn linkTo(package: Package, exe: *std.Build.CompileStep) !void {
        exe.addIncludePath(src() ++ "/lib/glfw/include");

        try package.netcode_package.linkTo(exe);
        package.zglfw_package.link(exe);
        package.zgpu_package.link(exe);
        package.zgui_package.link(exe);
        package.zmesh_package.link(exe);
        package.zaudio_package.link(exe);
    }
};

inline fn src() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}
