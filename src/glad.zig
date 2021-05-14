usingnamespace @cImport({
    @cInclude("glad/gl.h");
});

pub fn loadGl(getProcAddress: anytype) !void {
    if (gladLoadGL(@ptrCast(GLADloadfunc, getProcAddress)) == 0) {
        return error.GlLoadError;
    }
}
