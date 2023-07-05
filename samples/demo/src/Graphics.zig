const std = @import("std");
const auxo = @import("auxo");
const gpu = auxo.gpu;
const zm = @import("zmath");

// RGB equilateral triangle of unit side length
const vertices =
    [_]f32{ -0.5, -std.math.sqrt(3.0) / 6.0, 1, 0, 0, 1 } ++
    [_]f32{ 0.0, std.math.sqrt(3.0) / 3.0, 0, 1, 0, 1 } ++
    [_]f32{ 0.5, -std.math.sqrt(3.0) / 6.0, 0, 0, 1, 1 };

const FrameUniforms = struct {
    world_to_clip: zm.Mat,
    brightness: f32,
};

vertex_buffer: gpu.Buffer,
uniform_buffer: gpu.Buffer,
staging_buffer: gpu.Buffer,
uniforms_ready: bool = true,
pipeline: gpu.RenderPipeline,
bind_group: gpu.BindGroup,

pub fn init(ctx: auxo.GpuDeviceCreationContext) @This() {
    var device = ctx.device;

    const vertex_buffer = device.createBuffer(.{
        .size = vertices.len * @sizeOf(f32),
        .usage = .{ .vertex = true, .copy_dst = true },
        .mapped_at_creation = true,
    });
    var vertex_slice = vertex_buffer.getMappedRange(f32, 0, vertices.len) orelse unreachable;
    std.mem.copy(f32, vertex_slice, &vertices);
    vertex_buffer.unmap();

    const staging_buffer = device.createBuffer(.{
        .size = @sizeOf(FrameUniforms),
        .usage = .{ .map_write = true, .copy_src = true },
        .mapped_at_creation = true,
    });
    const uniform_buffer = device.createBuffer(.{
        .size = @sizeOf(FrameUniforms),
        .usage = .{ .copy_dst = true, .uniform = true },
    });

    const bind_group_layout = device.createBindGroupLayout(.{
        .entry_count = 1,
        .entries = &[_]gpu.BindGroupLayoutEntry{.{
            .binding = 0,
            .visibility = .{ .vertex = true, .fragment = true },
            .buffer = .{
                .binding_type = .uniform,
                .has_dynamic_offset = true,
                .min_binding_size = 0,
            },
        }},
    });
    defer bind_group_layout.release();

    const pipeline_layout = device.createPipelineLayout(.{
        .bind_group_layout_count = 1,
        .bind_group_layouts = &[_]gpu.BindGroupLayout{bind_group_layout},
    });
    defer pipeline_layout.release();

    const bind_group_entries = [_]gpu.BindGroupEntry{.{
        .binding = 0,
        .buffer = uniform_buffer,
        .offset = 0,
        .size = @sizeOf(FrameUniforms),
    }};
    const bind_group = device.createBindGroup(.{
        .layout = bind_group_layout,
        .entry_count = bind_group_entries.len,
        .entries = &bind_group_entries,
    });

    const vertex_buffer_layouts = [_]gpu.VertexBufferLayout{.{
        .attribute_count = 2,
        .attributes = &[_]gpu.VertexAttribute{
            .{
                .shader_location = 0,
                .offset = 0,
                .format = .float32x4,
            },
            .{
                .shader_location = 1,
                .offset = 8,
                .format = .float32x4,
            },
        },
        .array_stride = 24,
        .step_mode = .vertex,
    }};

    const wgsl_desc = gpu.ShaderModuleWgslDescriptor{
        .chain = .{ .next = null, .struct_type = .shader_module_wgsl_descriptor },
        .source = shader,
    };
    const shader_module = device.createShaderModule(.{ .next_in_chain = @as(*const gpu.ChainedStruct, @ptrCast(&wgsl_desc)) });
    defer shader_module.release();

    const pipeline = device.createRenderPipeline(.{
        .layout = pipeline_layout,
        .vertex = .{
            .module = shader_module,
            .entry_point = "vertex_main",
            .buffer_count = vertex_buffer_layouts.len,
            .buffers = &vertex_buffer_layouts,
        },
        .fragment = &.{
            .module = shader_module,
            .entry_point = "fragment_main",
            .target_count = 1,
            .targets = &[_]gpu.ColorTargetState{.{ .format = ctx.framebuffer_format }},
        },
        .primitive = .{ .topology = .triangle_list },
    });

    return .{
        .vertex_buffer = vertex_buffer,
        .uniform_buffer = uniform_buffer,
        .staging_buffer = staging_buffer,
        .pipeline = pipeline,
        .bind_group = bind_group,
    };
}

pub fn draw(self: *@This(), ctx: auxo.DrawContext, brightness: f32) void {
    const world_distance_from_left = 2.0;
    const world_distance_from_top = 2.0;
    const pixels_per_unit = 100.0;

    const screen_w_pixels: f32 = @floatFromInt(ctx.framebuffer_size.w);
    const screen_h_pixels: f32 = @floatFromInt(ctx.framebuffer_size.h);
    const screen_w = screen_w_pixels / pixels_per_unit;
    const screen_h = screen_h_pixels / pixels_per_unit;
    const world_to_view = zm.lookToLh(
        zm.f32x4(
            screen_w / 2.0 - world_distance_from_left,
            world_distance_from_top - screen_h / 2.0,
            0,
            0,
        ),
        zm.f32x4(0, 0, 1, 0),
        zm.f32x4(0, 1, 0, 0),
    );
    const view_to_clip = zm.orthographicLh(screen_w, screen_h, -1, 100);
    const world_to_clip = zm.transpose(zm.mul(world_to_view, view_to_clip));

    // =========================================================================

    const device = ctx.device;
    const reuse_stage = !self.uniforms_ready;
    const stage = if (reuse_stage) self.staging_buffer else device.createBuffer(.{
        .size = @sizeOf(FrameUniforms),
        .usage = .{ .map_write = true, .copy_src = true },
        .mapped_at_creation = true,
    });
    var uniform_slice = stage.getMappedRange(FrameUniforms, 0, 1) orelse unreachable;
    var uniform = &uniform_slice[0];
    uniform.world_to_clip = world_to_clip;
    uniform.brightness = brightness;

    const command_encoder = device.createCommandEncoder(null);
    defer command_encoder.release();

    const render_pass_descriptor = .{ .color_attachment_count = 1, .color_attachments = &[_]gpu.RenderPassColorAttachment{.{
        .view = ctx.framebuffer_view,
        .clear_value = .{ .r = 0.0, .g = 0.0, .b = 0.0, .a = 1.0 },
        .load_op = .clear,
        .store_op = .store,
    }} };
    const pass_encoder = command_encoder.beginRenderPass(render_pass_descriptor);
    defer pass_encoder.release();

    pass_encoder.setPipeline(self.pipeline);
    pass_encoder.setBindGroup(0, self.bind_group, &.{0});
    pass_encoder.setVertexBuffer(0, self.vertex_buffer, 0, vertices.len * @sizeOf(f32));
    pass_encoder.draw(3, 1, 0, 0);
    pass_encoder.end();

    ctx.guiRender(command_encoder);

    const commands = command_encoder.finish(null);
    defer commands.release();

    const queue = device.getQueue();
    defer queue.release();

    const stage_commands = stage_commands: {
        const stage_encoder = device.createCommandEncoder(null);
        defer stage_encoder.release();

        stage.unmap();
        stage_encoder.copyBufferToBuffer(
            stage,
            0,
            self.uniform_buffer,
            0,
            @sizeOf(FrameUniforms),
        );

        break :stage_commands stage_encoder.finish(null);
    };
    defer stage_commands.release();

    queue.submit(&.{ stage_commands, commands });

    if (reuse_stage) {
        self.uniforms_ready = false;
        self.staging_buffer.mapAsync(.{ .write = true }, 0, @sizeOf(FrameUniforms), uniformsMappedCallback, self);
    } else {
        stage.destroy();
        stage.release();
    }
}

fn uniformsMappedCallback(status: gpu.BufferMapAsyncStatus, userdata: ?*anyopaque) callconv(.C) void {
    var self = @as(*align(@sizeOf(usize)) @This(), @ptrCast(@alignCast(userdata)));
    if (status == .success) {
        self.uniforms_ready = true;
    } else {
        @panic("Couldn't map uniform buffer.");
    }
}

const shader =
    \\ struct VertexOut {
    \\     @builtin(position) position: vec4<f32>,
    \\     @location(0) color: vec4<f32>,
    \\ }
    \\
    \\ struct FrameUniforms {
    \\     world_to_clip: mat4x4<f32>,
    \\     brightness: f32,
    \\ }
    \\ @group(0) @binding(0) var<uniform> frame_uniforms: FrameUniforms;
    \\
    \\ @vertex
    \\ fn vertex_main(@location(0) position: vec2<f32>,
    \\                @location(1) color: vec4<f32>) -> VertexOut
    \\ {
    \\     var output: VertexOut;
    \\     output.position = vec4(position, 0, 1) * frame_uniforms.world_to_clip;
    \\     output.color = color * frame_uniforms.brightness;
    \\     return output;
    \\ }
    \\
    \\ @fragment
    \\ fn fragment_main(fragment_in: VertexOut) -> @location(0) vec4<f32>
    \\ {
    \\     return fragment_in.color;
    \\ }
;
