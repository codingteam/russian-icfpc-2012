CFLAGS = `sdl-config --cflags` `pkg-config gl --cflags`
LIBS = `sdl-config --libs` `pkg-config gl --libs`

all: functions.o bmp-reader test dump-bmp bruteforce dump-data-segment

%: %.cpp functions.o
	g++ $(CFLAGS) -o $@ $< functions.o $(LIBS)

clean:
	rm bmp-reader test dump-bmp bruteforce dump-data-segment *.o
