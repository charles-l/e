#include <stdio.h>
#include <GL/glew.h>
#include <GLFW/glfw3.h>
#include "nanovg.h"
#define NANOVG_GL3_IMPLEMENTATION
#include "nanovg_gl.h"
#include "nanovg_gl_utils.h"

const GLuint WIDTH = 800, HEIGHT = 600;

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

extern "C" NVGcontext *initVG() {
    NVGcontext* vg = NULL;
    vg = nvgCreateGL3(NVG_ANTIALIAS | NVG_STENCIL_STROKES | NVG_DEBUG);
    if (vg == NULL) {
	printf("Could not init nanovg.\n");
	return NULL;
    }

    if (loadFonts(vg) == -1) {
	printf("Could not load fonts\n");
	return NULL;
    }
    return vg;
}

extern "C" void clear() {
    glClearColor(0.1, 0.1, 0.1, 1.0);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT|GL_STENCIL_BUFFER_BIT);
}

extern "C" void setFill(NVGcontext *nvg, unsigned char r, unsigned char g, unsigned char b, unsigned char a) {
    nvgFillColor(nvg, nvgRGBA(r, g, b, a));
}

extern "C" void renderText(NVGcontext *nvg, float x, float y, char *text) {
    nvgText(nvg, x, y, text, NULL);
}

extern "C" GLFWwindow *init() {
    GLFWwindow* window;

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
    window = glfwCreateWindow(WIDTH, HEIGHT, "x", NULL, NULL);
    if (!window) {
	glfwTerminate();
	return NULL;
    }

    glfwMakeContextCurrent(window);
    glewExperimental = GL_TRUE;
    if(glewInit() != GLEW_OK) {
	printf("Could not init glew.\n");
	return NULL;
    }
    // GLEW generates GL error because it calls glGetString(GL_EXTENSIONS), we'll consume it here.
    glGetError();

    glViewport(0, 0, WIDTH, HEIGHT);

    return window;
}
