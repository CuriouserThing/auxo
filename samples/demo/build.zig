const std = @import("std");
const auxo = @import("../../build.zig");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "auxo-demo",
        .root_source_file = .{ .path = src() ++ "/src/main.zig" },
        .target = target,
        .optimize = optimize,
    });
    const auxo_package = auxo.Package.build(b, target, optimize) catch unreachable;
    auxo_package.linkTo(exe) catch unreachable;
    exe.addModule("auxo", auxo_package.module);
    b.installArtifact(exe);

    const run_artifact = b.addRunArtifact(exe);
    run_artifact.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_artifact.addArgs(args);
    }

    const run_step = b.step("run", "Run the app");
    run_step.dependOn(&run_artifact.step);

    const exe_tests = b.addTest(.{
        .root_source_file = .{ .path = src() ++ "src/main.zig" },
        .target = target,
        .optimize = optimize,
    });

    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&exe_tests.step);
}

inline fn src() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}
