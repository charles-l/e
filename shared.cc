#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>

#include "nanovg.h"
#define NANOVG_GL3_IMPLEMENTATION
#include "nanovg_gl.h"
#include "nanovg_gl_utils.h"

const GLuint WIDTH = 800, HEIGHT = 600;

struct context {
    NVGcontext *vg;
    GLFWwindow *win;
};

int loadFonts(NVGcontext* vg) {
    int font;
    font = nvgCreateFont(vg, "sans", "/usr/share/fonts/TTF/FiraMono-Regular.ttf");
    if (font == -1) {
	printf("Could not add font regular.\n");
	return -1;
    }
    font = nvgCreateFont(vg, "sans-bold", "/usr/share/fonts/TTF/FiraMono-Bold.ttf");
    if (font == -1) {
	printf("Could not add font bold.\n");
	return -1;
    }
    return 0;
}

extern "C" {
GLFWwindow *getWindow(context *c) {
    return c->win;
}

NVGcontext *getVGContext(context *c) {
    return c->vg;
}

bool shouldQuit(context *c) {
    return glfwWindowShouldClose(c->win);
}

void clear() {
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
}

void startFrame(context *c) {
    glfwPollEvents();
    clear();
    nvgBeginFrame(c->vg, 800, 600, 8.0 / 6.0);
}

void endFrame(context *c) {
    nvgEndFrame(c->vg);
    glfwSwapBuffers(c->win);
}

void setFill(context *c, unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    nvgFillColor(c->vg, nvgRGBA(r, g, b, a));
}

void setStrokeColor(context *c, unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    nvgStrokeColor(c->vg, nvgRGBA(r, g, b, a));
}

void renderText(context *c, float x, float y, char *text) {
    nvgText(c->vg, x, y, text, NULL);
}

void drawRect(context *c, float x, float y, float w, float h) {
    nvgBeginPath(c->vg);
    nvgRect(c->vg, x, y, w, h);
    nvgFill(c->vg);
    nvgClosePath(c->vg);
}

void drawLine(context *c, float x1, float y1, float x2, float y2) {
    nvgBeginPath(c->vg);
    nvgMoveTo(c->vg, x1, y1);
    nvgLineTo(c->vg, x2, y2);
    nvgStroke(c->vg);
}

float calcCharXPos(context *c, char *s, int i) {
#define MAX_GLYPHS 100
    NVGglyphPosition p[MAX_GLYPHS];
    nvgTextGlyphPositions(c->vg, 0, 0, s, NULL, p, MAX_GLYPHS);
    return p[i].x;
}

context *init() {
    auto *c = new context;

    if (!glfwInit()) {
	printf("Failed to init GLFW.");
	return NULL;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 2);
    glfwWindowHint(GLFW_OPENGL_FORWARD_COMPAT, GL_TRUE);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);
    glfwWindowHint(GLFW_OPENGL_DEBUG_CONTEXT, 1);

    glfwWindowHint(GLFW_SAMPLES, 4);
    c->win = glfwCreateWindow(WIDTH, HEIGHT, "x", NULL, NULL);
    if (!c->win) {
	glfwTerminate();
	return NULL;
    }

    glfwMakeContextCurrent(c->win);
    glewExperimental = GL_TRUE;
    if(glewInit() != GLEW_OK) {
	printf("Could not init glew.\n");
	return NULL;
    }
    // GLEW generates GL error because it calls glGetString(GL_EXTENSIONS), we'll consume it here.
    glGetError();

    glViewport(0, 0, WIDTH, HEIGHT);

    c->vg = nvgCreateGL3(NVG_ANTIALIAS | NVG_STENCIL_STROKES | NVG_DEBUG);
    if (c->vg == NULL) {
	printf("Could not init nanovg.\n");
	return NULL;
    }

    if (loadFonts(c->vg) == -1) {
	printf("Could not load fonts\n");
	return NULL;
    }

    return c;
}

void cleanup(context *c) {
    nvgDeleteGL3(c->vg);
    glfwTerminate();
    delete c;
}
}
