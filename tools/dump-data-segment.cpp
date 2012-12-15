#include <iostream>
#include <fstream>

#include <SDL/SDL.h>

#include "functions.h"

using namespace std;

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./dump-data-segment <bmp-file>\n");
        return 1;
    }

    const char *bmp_file = argv[1];

    SDL_Surface *image = SDL_LoadBMP(bmp_file);
    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    // for(int y = 0; y < image->h; ++y)
    for(int y = image->h - 1; y >= 0; --y)
        for(int x = 0; x < image->w; ++x)
            cout << Get_Pixel(image, x, y) << endl;

    return 0;
}
