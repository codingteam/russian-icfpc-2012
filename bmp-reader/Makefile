CFLAGS = `sdl-config --cflags` `pkg-config gl --cflags`
LIBS = `sdl-config --libs` `pkg-config gl --libs`

all: bmp-reader test

bmp-reader: main.c functions.c
	gcc $(CFLAGS) -o $@ main.c functions.c $(LIBS)

test: test.c functions.c
	gcc $(CFLAGS) -o $@ test.c functions.c $(LIBS)

clean:
	rm bmp-reader test *.o
