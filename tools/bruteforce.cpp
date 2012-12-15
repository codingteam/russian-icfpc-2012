#include <cstdio>
#include <cstdlib>

#include <iostream>
#include <fstream>
#include <set>
#include <limits>
#include <utility>

#include <SDL/SDL.h>

#include "functions.h"

using namespace std;

void Read_Coords(set<pair<Sint8, Sint8> > &coords, const char *coord_file)
{
    ifstream fin(coord_file);

    Sint8 vx, vy;
    while(fin >> vx >> vy) {
        coords.insert(make_pair(vx, vy));
        coords.insert(make_pair(-vx, -vy));
    }
}

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./bruteforce <bmp-file> <coord-file1> <coord-file2> ...");
        return 1;
    }

    // BMP
    const char *bmp_file = argv[1];
    SDL_Surface *image = SDL_LoadBMP(bmp_file);
    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    // Coords
    set<pair<Sint8, Sint8> > coords;
    for(int i = 2; i < argc; ++i)
        Read_Coords(coords, argv[i]);

    // Bruteforce
    Sint8 v_min = numeric_limits<Sint8>::min();
    Sint8 v_max = numeric_limits<Sint8>::max();
    for(int x = 0; x < image->w; ++x)
        for(int y = 0; y < image->h; ++y)
            for(Sint8 vx = v_min; vx <= v_max; ++vx)
                for(Sint8 vy = v_min; vy <= v_max; ++vy)
                    if(Within_Surface(image, x + vx, y + vy)) {
                        int lines = 0;
                        int correct_lines = 0;

                        // ... drawing algorithm ...
                    }
    
    return 0;
}
