#include <stdio.h>
#include <stdlib.h>
#include <SDL/SDL.h>
#include <GL/gl.h>

#include "functions.h"

int main(int argc, char **argv)
{
    if(argc < 6) {
        printf("Usage: ./bmp-reader <bmp-file> <x> <y> <vx> <vy>\n");
        return 1;
    }

    const char *bmp_file = argv[1];
    int x = atoi(argv[2]);
    int y = atoi(argv[3]);
    Sint8 vx = atoi(argv[4]);
    Sint8 vy = atoi(argv[5]);

    SDL_Init(SDL_INIT_VIDEO);

    SDL_Surface *screen = SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT,
                                           32, SDL_DOUBLEBUF | SDL_OPENGL);

    if(screen == NULL) {
        fprintf(stderr, "Unable to set video mode: %s\n", SDL_GetError());
        return 1;
    }

    glOrtho(0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, -1, 1);

    SDL_Surface *image = SDL_LoadBMP(bmp_file);

    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    Draw_Image(image, x, y, vx, vy);
    Event_Loop();

    SDL_Quit();

    return 0;
}
