.PHONY: all

all: ssf ssf.dylib

ssf: ssf.h ssf.c ssf_sdl.c
	gcc -O2 -Wall -g ssf.c ssf_sdl.c -o ssf -framework SDL2

ssf.dylib: ssf.h ssf.c
	gcc -arch x86_64 -O2 -Wall -g -shared ssf.c -o ssf.dylib #  -fpic
