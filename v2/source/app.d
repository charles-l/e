import opengl.gl4;
import glfw3d;
import derelict.freetype;
import std.stdio;
import std.string;
import std.file;
import gfm.math;
import std.range;
import std.container : DList;
import std.mmfile;

struct point {
    GLfloat x;
    GLfloat y;
    GLfloat s;
    GLfloat t;
}

struct Program {
    uint id;
}

struct Character {
    GLuint textureID;
    vec2i Size;
    vec2i Bearing;
    GLuint Advance;
}

uint _compileShader(string source, uint type) {
    uint id = glCreateShader(type);
    char* s = cast(char*) toStringz(source);
    glShaderSource(id, 1u, cast(const(char**))&s, cast(const(int*)) null);
    glCompileShader(id);
    int result;
    int loglen;
    glGetShaderiv(id, GL_COMPILE_STATUS, &result);
    glGetShaderiv(id, GL_INFO_LOG_LENGTH, &loglen);
    if (loglen > 0) {
        auto log = new char[loglen + 1];
        glGetShaderInfoLog(id, cast(uint) log.length, cast(uint*) null, log.ptr);
        writeln(fromStringz(log.ptr));
        log.destroy;
    }
    return id;
}

Program makeProgram(uint vid, uint fid) {
    uint linkProgram(uint vshader, uint fshader) {
        uint p = glCreateProgram();
        glAttachShader(p, vshader);
        glAttachShader(p, fshader);
        glLinkProgram(p);

        int result, loglen;
        glGetShaderiv(p, GL_COMPILE_STATUS, &result);
        glGetShaderiv(p, GL_INFO_LOG_LENGTH, &loglen);
        if (loglen > 0) {
            auto log = new char[loglen + 1];
            glGetProgramInfoLog(p, cast(uint) log.length, cast(uint*) null, log.ptr);
            writeln(fromStringz(log.ptr));
            log.destroy;
        }

        glDeleteShader(vshader);
        glDeleteShader(fshader);

        return p;
    }

    Program p = {linkProgram(vid, fid)};
    return p;
}

Program makeProgram(string vsource, string fsource) {
    return makeProgram(_compileShader(vsource, GL_VERTEX_SHADER),
            _compileShader(fsource, GL_FRAGMENT_SHADER));
}

Character[dchar] characters;

struct Cursor {
    char *cur;
    alias cur this;
}

struct Piece {
    char[] range;
    alias range this;
}

struct Buffer {
    MmFile file;
    string addbuf;
    Piece[] pieces;
}

Buffer curBuffer;
Cursor cursor;

Cursor at(Buffer b, uint offset) {
    uint o = 0;
    foreach(p; b.pieces) {
        if(o + p.length > offset)
            return Cursor(&p[offset - o]);
        o += p.length;
    }
    throw new Exception("offset not in buffer: %s".format(offset));
}

unittest {
    template assertEquals(T) {
        void assertEquals(T a, T b) {
            assert(a == b, "expected `%s` but got `%s`".format(a, b));
        }
    }

    string x = "a b c d e";
    Buffer b = Buffer(null, null, [Piece(cast(char[]) x[0..2]), Piece(cast(char[]) x[2..4]), Piece(cast(char[]) x[4..x.length])]);
    assertEquals('a', *at(b, 0));
    assertEquals('b', *at(b, 2));
    assertEquals('c', *at(b, 4));
    assertEquals('d', *at(b, 6));
    assertEquals('e', *at(b, 8));
    b = Buffer(null, null, [Piece(cast(char[]) x[0..2]), Piece(cast(char[]) x[4..x.length]), Piece(cast(char[]) x[2..4])]);
    assertEquals('a', *at(b, 0));
    assertEquals('c', *at(b, 2));
    assertEquals('d', *at(b, 4));
    assertEquals('e', *at(b, 6));
}

void remove(ref Buffer b, Cursor start, Cursor end) {
    //start = start.sanitize(b), end = end.sanitize(b);
}

extern(C) nothrow void charCallback(GLFWwindow *win, uint codepoint) {
    //curBuffer.lines.front ~= codepoint;
}

extern(C) nothrow void keyCallback(GLFWwindow *win, int key, int scancode, int action, int mods) {
    if(key == GLFW_KEY_BACKSPACE) {
    }
}

// TODO: make font rendering NOT look like a pile of steaming garbage
void main() {
    //curBuffer.lines.insertBack("");
    curBuffer.file = new MmFile("dub.sdl");
    glfw3dInit();

    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 4);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 5);

    Window w = new Window(640, 480, "TEST");
    w.makeContextCurrent();

    w.setCharCallback(&charCallback);

    DerelictFT.load();
    FT_Library lib;
    FT_Init_FreeType(&lib);

    FT_Face font;

    FT_New_Face(lib, toStringz("/usr/share/fonts/TTF/FiraMono-Regular.ttf"), 0, &font);
    FT_Set_Pixel_Sizes(font, 0, 22);
    FT_Select_Charmap(font, FT_ENCODING_UNICODE);

    Program p = makeProgram(cast(string) read("./text.vert", 512),
            cast(string) read("./text.frag", 512));

    glEnable(GL_BLEND);
    glEnable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glPixelStorei(GL_UNPACK_ALIGNMENT, 1); // Disable byte-alignment restriction

    mat4f proj = mat4f.orthographic(0.0f, 800.0f, 600.0f, 0.0f, -0.1, 1000.0);

    GLuint VAO, VBO;
    glGenVertexArrays(1, &VAO);
    glGenBuffers(1, &VBO);
    glBindVertexArray(VAO);
    glBindBuffer(GL_ARRAY_BUFFER, VBO);
    glBufferData(GL_ARRAY_BUFFER, GLfloat.sizeof * 6 * 4, null, GL_DYNAMIC_DRAW);
    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 4, GL_FLOAT, GL_FALSE, 4 * GLfloat.sizeof, null);
    glBindBuffer(GL_ARRAY_BUFFER, 0);
    glBindVertexArray(0);

    foreach (dchar c; iota(cast(dchar) 0, cast(dchar) 128).chain("α")) {
        uint i = FT_Get_Char_Index(font, c);
        assert(FT_Load_Glyph(font, i, FT_LOAD_RENDER) == 0);
        // Generate texture
        GLuint texture;
        glGenTextures(1, &texture);
        glBindTexture(GL_TEXTURE_2D, texture);
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RED, font.glyph.bitmap.width,
                font.glyph.bitmap.rows, 0, GL_RED, GL_UNSIGNED_BYTE, font.glyph.bitmap.buffer);
        // Set texture options
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_BORDER);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        // Now store character for later use
        characters[c] = Character(texture,
                vec2i(font.glyph.bitmap.width, font.glyph.bitmap.rows),
                vec2i(font.glyph.bitmap_left, font.glyph.bitmap_top),
                cast(uint) font.glyph.advance.x);
    }

    FT_Done_Face(font);
    FT_Done_FreeType(lib);

    void RenderText(Program s, wstring text, GLfloat x, GLfloat y, GLfloat scale, vec3f color) {
        // Activate corresponding render state
        glUseProgram(s.id);
        glUniform3f(glGetUniformLocation(s.id, "textColor"), color.x, color.y, color.z);
        glActiveTexture(GL_TEXTURE0);
        glBindVertexArray(VAO);

        // Iterate through all characters
        foreach (dchar c; text) {
            if(c !in characters)
                c = '?';
            Character ch = characters[c];

            GLfloat xpos = x + ch.Bearing.x * scale;
            GLfloat ypos = y - ch.Bearing.y * scale;

            GLfloat w = ch.Size.x * scale;
            GLfloat h = ch.Size.y * scale;
            // Update VBO for each character
            GLfloat[4][6] vertices = [
                [xpos, ypos + h, 0.0, 1.0],
                [xpos, ypos, 0.0, 0.0],
                [xpos + w, ypos, 1.0, 0.0],

                [xpos, ypos + h, 0.0, 1.0],
                [xpos + w, ypos, 1.0, 0.0],
                [xpos + w, ypos + h, 1.0, 1.0]
            ];
            // Render glyph texture over quad
            glBindTexture(GL_TEXTURE_2D, ch.textureID);
            // Update content of VBO memory
            glBindBuffer(GL_ARRAY_BUFFER, VBO);
            glBufferSubData(GL_ARRAY_BUFFER, 0, vertices.sizeof, vertices.ptr);
            glBindBuffer(GL_ARRAY_BUFFER, 0);
            // Render quad
            glDrawArrays(GL_TRIANGLES, 0, 6);
            // Now advance cursors for next glyph (note that advance is number of 1/64 pixels)
            x += (ch.Advance >> 6) * scale; // Bitshift by 6 to get value in pixels (2^6 = 64)
        }
        glBindVertexArray(0);
        glBindTexture(GL_TEXTURE_2D, 0);
    }

    import std.utf : toUTF16;

    while (!w.shouldClose()) {
        glfwPollEvents();

        glClearColor(0, 0, 0, 0.0f);
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
        glUseProgram(p.id);
        glUniformMatrix4fv(glGetUniformLocation(p.id, "projection"), 1, true, proj.ptr);

        string data = cast(string) curBuffer.file[0..curBuffer.file.length];
        foreach(i, line; enumerate(data.split("\n"))) {
            RenderText(p, toUTF16(line), 10, i * 30 + 20, 1, vec3f(1, 1, 1));
        }

        w.swapBuffers();
    }
}
