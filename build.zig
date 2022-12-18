const std = @import("std");
const netcode = @import("lib/zig-netcode/build.zig");

pub fn linkTo(exe: *std.build.LibExeObjStep) !void {
    try netcode.linkTo(exe);
}

pub fn package(name: []const u8) std.build.Pkg {
    return .{
        .name = name,
        .source = .{ .path = src() ++ "/src/main.zig" },
        .dependencies = &[_]std.build.Pkg{netcode.package("netcode")},
    };
}

inline fn src() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}
