pub const glfw = struct {
    pub const initForOther = ImGui_ImplGlfw_InitForOther;
    extern fn ImGui_ImplGlfw_InitForOther(
        window: *const anyopaque,
        install_callbacks: bool,
    ) bool;

    pub const shutdown = ImGui_ImplGlfw_Shutdown;
    extern fn ImGui_ImplGlfw_Shutdown() void;

    pub const newFrame = ImGui_ImplGlfw_NewFrame;
    extern fn ImGui_ImplGlfw_NewFrame() void;
};

pub const wgpu = struct {
    const TextureFilterMode = enum(u32) {
        nearest,
        linear,
    };
    const Config = extern struct {
        pipeline_multisample_count: u32 = 1,
        texture_filter_mode: TextureFilterMode = .linear,
    };

    pub const init = ImGui_ImplWGPU_Init;
    extern fn ImGui_ImplWGPU_Init(
        device: *const anyopaque,
        num_frames_in_flight: u32,
        rt_format: u32,
        config: *const Config,
    ) bool;

    pub const newFrame = ImGui_ImplWGPU_NewFrame;
    extern fn ImGui_ImplWGPU_NewFrame() void;

    pub const renderDrawData = ImGui_ImplWGPU_RenderDrawData;
    extern fn ImGui_ImplWGPU_RenderDrawData(
        draw_data: *const anyopaque,
        pass_encoder: *const anyopaque,
    ) void;

    pub const shutdown = ImGui_ImplWGPU_Shutdown;
    extern fn ImGui_ImplWGPU_Shutdown() void;

    pub const invalidateDeviceObjects = ImGui_ImplWGPU_InvalidateDeviceObjects;
    extern fn ImGui_ImplWGPU_InvalidateDeviceObjects() void;

    pub const createDeviceObjects = ImGui_ImplWGPU_CreateDeviceObjects;
    extern fn ImGui_ImplWGPU_CreateDeviceObjects() void;
};
