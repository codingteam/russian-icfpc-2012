#include <stdio.h>
#include <stdlib.h>
#include <SDL/SDL.h>
#include <GL/gl.h>

#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

void Get_Pixel(SDL_Surface *surface, int x, int y, Uint8 *r, Uint8 *g, Uint8 *b, Uint8 *a)
{
    if(0 <= x && x < surface->w)
        if(0 <= y && y < surface->h) {
            int bpp = surface->format->BytesPerPixel;
            Uint8 *p = (Uint8*)surface->pixels + y * surface->pitch + x * bpp;
            SDL_GetRGBA(*(Uint32*)p, surface->format, r, g, b, a);
        }
}

void Event_Loop()
{
    SDL_Event event;
    int exit = 0;

    while(!exit) {
        while(!exit && SDL_PollEvent(&event)) {
            switch(event.type) {
            case SDL_QUIT:
                exit = 1;
                break;

            case SDL_KEYDOWN:
                if(event.key.keysym.sym == SDLK_ESCAPE)
                    exit = 1;
                break;

            default: ;
            }
        }
    }
}

void Draw_Image(SDL_Surface *surface)
{
    /* int x = 70, y = 79; */
    /* Sint8 vx = 18, vy = 26; */
    int x = 50, y = 22;
    Sint8 vx = 24, vy = 34;
    int clr = 0;
    Uint8 a, b, c, t;

    glClear(GL_COLOR_BUFFER_BIT);

    Get_Pixel(surface, x, y, &c, &b, &a, &t);
    while(a != 0 || b != 0 || c != 0) {
        printf("%d %d\n", x, y);

        if(x >= surface->w || y >= surface->h)
            break;

        vx ^= a;
        vy ^= b;
        clr ^= c;
        if(clr) {
            glBegin(GL_LINES);
            glColor3f(1.0, 1.0, 1.0);
            glVertex2i(x, y);
            glVertex2i(x + vx, y + vy);
            glEnd();
        }
        x += vx;
        y += vy;
        Get_Pixel(surface, x, y, &c, &b, &a, &t);
    } 

    SDL_GL_SwapBuffers();
}

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./bmp-reader <bmp-file>\n");
        return 1;
    }

    SDL_Init(SDL_INIT_VIDEO);

    SDL_Surface *screen = SDL_SetVideoMode(SCREEN_WIDTH, SCREEN_HEIGHT,
                                           32, SDL_DOUBLEBUF | SDL_OPENGL);

    if(screen == NULL) {
        fprintf(stderr, "Unable to set video mode: %s\n", SDL_GetError());
        return 1;
    }

    glOrtho(0, SCREEN_WIDTH, SCREEN_HEIGHT, 0, -1, 1);

    SDL_Surface *image = SDL_LoadBMP(argv[1]);

    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    Draw_Image(image);
    Event_Loop();

    SDL_Quit();

    return 0;
}
