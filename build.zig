const std = @import("std");
const zgui = @import("lib/zig-gamedev/libs/zgui/build.zig");
const netcode = @import("lib/zig-netcode/build.zig");
const demo = @import("samples/demo/build.zig");

inline fn thisDir() []const u8 {
    return comptime std.fs.path.dirname(@src().file) orelse ".";
}

inline fn glfwDir() []const u8 {
    return thisDir() ++ "/lib/glfw/";
}

inline fn systemSdkDir() []const u8 {
    return thisDir() ++ "/lib/zig-gamedev/libs/system-sdk";
}

inline fn zgpuDir() []const u8 {
    return thisDir() ++ "/lib/zig-gamedev/libs/zgpu";
}

pub fn build(b: *std.Build) void {
    demo.build(b);
}

pub const Package = struct {
    module: *std.Build.Module,
    glfw: *std.Build.CompileStep,
    zgui_package: zgui.Package,
    netcode_package: netcode.Package,

    pub fn build(b: *std.Build, target: std.zig.CrossTarget, optimize: std.builtin.Mode) !Package {
        const wgpu_module = b.createModule(.{ .source_file = .{ .path = zgpuDir() ++ "/src/wgpu.zig" }, .dependencies = &.{} });
        const zgui_package = zgui.package(b, target, optimize, .{ .options = .{ .backend = .glfw_wgpu } });
        const netcode_package = try netcode.Package.build(b, target, optimize);
        const module = b.createModule(.{
            .source_file = .{ .path = thisDir() ++ "/src/main.zig" },
            .dependencies = &.{
                .{ .name = "wgpu", .module = wgpu_module },
                .{ .name = "zgui", .module = zgui_package.zgui },
                .{ .name = "netcode", .module = netcode_package.module },
            },
        });
        return Package{
            .module = module,
            .glfw = try buildGlfw(b, target, optimize),
            .zgui_package = zgui_package,
            .netcode_package = netcode_package,
        };
    }

    pub fn linkTo(package: Package, exe: *std.Build.CompileStep) !void {
        // TODO: monitor the Windows LTO bug (#8531, #15958) that forces this tweak
        exe.want_lto = false;

        exe.addIncludePath(glfwDir() ++ "/include");
        exe.linkLibrary(package.glfw);
        try package.netcode_package.linkTo(exe);
        try linkDawnTo(exe);
        package.zgui_package.link(exe);
    }
};

fn buildGlfw(b: *std.Build, target: std.zig.CrossTarget, optimize: std.builtin.Mode) !*std.Build.CompileStep {
    const glfw = b.addStaticLibrary(.{
        .name = "glfw",
        .target = target,
        .optimize = optimize,
    });
    glfw.addIncludePath(glfwDir() ++ "/include");
    glfw.linkLibC();

    const src = glfwDir() ++ "/src";
    const common_source = .{
        src ++ "/init.c",
        src ++ "/monitor.c",
        src ++ "/window.c",
        src ++ "/input.c",
        src ++ "/vulkan.c",
        src ++ "/context.c",
        src ++ "/osmesa_context.c",
        src ++ "/egl_context.c",
    };

    const target_info = try std.zig.system.NativeTargetInfo.detect(target);
    switch (target_info.target.os.tag) {
        .windows => {
            glfw.linkSystemLibraryName("gdi32");
            glfw.linkSystemLibraryName("user32");
            glfw.linkSystemLibraryName("shell32");
            glfw.addCSourceFiles(&(common_source ++ .{
                src ++ "/wgl_context.c",
                src ++ "/win32_thread.c",
                src ++ "/win32_time.c",
                src ++ "/win32_init.c",
                src ++ "/win32_monitor.c",
                src ++ "/win32_window.c",
                src ++ "/win32_joystick.c",
            }), &.{"-D_GLFW_WIN32"});
        },

        .linux => {
            glfw.addSystemIncludePath(systemSdkDir() ++ "/linux/include");
            if (target_info.target.cpu.arch.isX86()) {
                glfw.addLibraryPath(systemSdkDir() ++ "/linux/lib/x86_64-linux-gnu");
            } else {
                glfw.addLibraryPath(systemSdkDir() ++ "/linux/lib/aarch64-linux-gnu");
            }
            glfw.linkSystemLibraryName("X11");
            glfw.addCSourceFiles(&(common_source ++ .{
                src ++ "/glx_context.c",
                src ++ "/posix_thread.c",
                src ++ "/posix_time.c",
                src ++ "/x11_init.c",
                src ++ "/x11_monitor.c",
                src ++ "/x11_window.c",
                src ++ "/linux_joystick.c",
                src ++ "/xkb_unicode.c",
            }), &.{"-D_GLFW_X11"});
        },

        .macos => {
            glfw.addFrameworkPath(systemSdkDir() ++ "/macos12/System/Library/Frameworks");
            glfw.addSystemIncludePath(systemSdkDir() ++ "/macos12/usr/include");
            glfw.addLibraryPath(systemSdkDir() ++ "/macos12/usr/lib");
            glfw.linkSystemLibraryName("objc");
            glfw.linkFramework("AppKit");
            glfw.linkFramework("CoreFoundation");
            glfw.linkFramework("CoreGraphics");
            glfw.linkFramework("CoreServices");
            glfw.linkFramework("Foundation");
            glfw.linkFramework("IOKit");
            glfw.linkFramework("Metal");
            glfw.addCSourceFiles(&(common_source ++ .{
                src ++ "/nsgl_context.m",
                src ++ "/posix_thread.c",
                src ++ "/cocoa_time.c",
                src ++ "/cocoa_init.m",
                src ++ "/cocoa_monitor.m",
                src ++ "/cocoa_window.m",
                src ++ "/cocoa_joystick.m",
            }), &.{"-D_GLFW_COCOA"});
        },

        else => unreachable,
    }

    return glfw;
}

fn linkDawnTo(exe: *std.Build.CompileStep) !void {
    exe.addIncludePath(zgpuDir() ++ "/libs/dawn/include");
    exe.addIncludePath(zgpuDir() ++ "/src");
    exe.linkLibC();
    exe.linkLibCpp();
    exe.linkSystemLibraryName("dawn");
    exe.addCSourceFile(zgpuDir() ++ "/src/dawn.cpp", &.{"-std=c++17"});

    const target_info = try std.zig.system.NativeTargetInfo.detect(exe.target);
    switch (target_info.target.os.tag) {
        .windows => {
            exe.addLibraryPath(systemSdkDir() ++ "/windows/lib/x86_64-windows-gnu");
            exe.addLibraryPath(zgpuDir() ++ "/libs/dawn/x86_64-windows-gnu");
            exe.linkSystemLibraryName("ole32");
            exe.linkSystemLibraryName("dxguid");
        },

        .linux => {
            if (target_info.target.cpu.arch.isX86()) {
                exe.addLibraryPath(zgpuDir() ++ "/libs/dawn/x86_64-linux-gnu");
            } else {
                exe.addLibraryPath(zgpuDir() ++ "/libs/dawn/aarch64-linux-gnu");
            }
        },

        .macos => {
            exe.addFrameworkPath(systemSdkDir() ++ "/macos12/System/Library/Frameworks");
            exe.addSystemIncludePath(systemSdkDir() ++ "/macos12/usr/include");
            exe.addLibraryPath(systemSdkDir() ++ "/macos12/usr/lib");
            if (target_info.target.cpu.arch.isX86()) {
                exe.addLibraryPath(zgpuDir() ++ "/libs/dawn/x86_64-macos-none");
            } else {
                exe.addLibraryPath(zgpuDir() ++ "/libs/dawn/aarch64-macos-none");
            }
            exe.linkSystemLibraryName("objc");
            exe.linkFramework("CoreGraphics");
            exe.linkFramework("Foundation");
            exe.linkFramework("IOKit");
            exe.linkFramework("IOSurface");
            exe.linkFramework("Metal");
            exe.linkFramework("QuartzCore");
        },

        else => unreachable,
    }
}
