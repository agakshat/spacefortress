UNAME=$(shell uname)

ifeq ($(UNAME), Linux)
include Makefile.linux
else ifeq ($(UNAME), Darwin)
include Makefile.osx
endif

NAME=ssf
VERSION=v1
BUILD=$(shell git log --pretty=oneline | wc -l | sed 's/ //g')
SHA1=$(shell git rev-parse -q HEAD)
FULLNAME=$(NAME)\ $(VERSION).$(BUILD)

FLAGS += -DGGITSHA1=\"$(SHA1)\" -DGVERSION=\"$(VERSION).$(BUILD)\"

.PHONY: all clean

all: ssf libssf.$(LIB_EXT) ssf_cairo libssfcairo.$(LIB_EXT)

ssf: ssf.o ssf_sdl.o
	$(CC) $(FLAGS) $^ -o $@ $(SDL2_LINK_FLAGS)

libssf.$(LIB_EXT): ssf.o
	$(CC) $(FLAGS) -shared $^ -o $@

ssf_cairo: ssf.o ssf_cairo.o ssf_cairo_sdl.o
	$(CC) $(FLAGS) $^ -o $@ $(SDL2_LINK_FLAGS) $(CAIRO_LINK_FLAGS)

libssfcairo.$(LIB_EXT): ssf.o ssf_cairo.o
	$(CC) $(FLAGS) -shared $^ -o $@ $(CAIRO_LINK_FLAGS)

%.o: %.c *.c
	$(CC) $(FLAGS) -c $< -o $@ $(SDL2_LINK_FLAGS) $(CAIRO_LINK_FLAGS)

clean:
	rm -f *.o ssf libssf.$(LIB_EXT) ssf_cairo libssfcairo.$(LIB_EXT)
