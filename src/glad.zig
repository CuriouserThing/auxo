usingnamespace @cImport({
    @cInclude("glad/glad.h");
});

pub fn loadGl(getProcAddress: anytype) !void {
    if (gladLoadGLLoader(@ptrCast(GLADloadproc, getProcAddress)) == 0) {
        return error.GlLoadError;
    }
}
