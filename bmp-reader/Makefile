CFLAGS = `sdl-config --cflags` `pkg-config gl --cflags`
LIBS = `sdl-config --libs` `pkg-config gl --libs`

bmp-reader: main.c
	gcc $(CFLAGS) -o bmp-reader main.c $(LIBS)
