#include <stdio.h>
#include <stdlib.h>
#include <SDL/SDL.h>
#include <GL/gl.h>

#include "functions.h"

int main(int argc, char **argv)
{
    if(argc < 6) {
        printf("Usage: ./test <bmp-file> <x> <y> <vx> <vy>\n");
        return 1;
    }

    const char *bmp_file = argv[1];
    int x = atoi(argv[2]);
    int y = atoi(argv[3]);
    Sint8 vx = atoi(argv[4]);
    Sint8 vy = atoi(argv[5]);

    SDL_Init(SDL_INIT_VIDEO);

    glOrtho(0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, -1, 1);

    SDL_Surface *image = SDL_LoadBMP(bmp_file);

    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    try_address(image, x, y, vx, vy);

    SDL_Quit();

    return 0;
}

