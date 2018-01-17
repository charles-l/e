all:
	clang++ -g -fPIC -shared shared.cc -o shared.so -lGLEW -lGL -lglfw -lnanovg
