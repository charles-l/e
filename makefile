all:
	clang++ -g -fPIC -shared shared.cc -o shared.so `pkg-config --cflags freetype2` -lGLEW -lGL -lglfw `pkg-config --libs freetype2`
