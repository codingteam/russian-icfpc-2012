#include <stdio.h>
#include <stdlib.h>
#include <SDL/SDL.h>
#include <GL/gl.h>

#include "functions.h"

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./dump-bmp <bmp-file>\n");
        return 1;
    }

    const char *bmp_file = argv[1];

    SDL_Init(SDL_INIT_VIDEO);
    glOrtho(0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, -1, 1);
    SDL_Surface *image = SDL_LoadBMP(bmp_file);

    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    Dump_Image(image);

    SDL_Quit();

    return 0;
}
