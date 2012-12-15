#include <stdio.h>
#include <stdlib.h>
#include <GL/gl.h>
#include "functions.h"

void Get_Pixel(SDL_Surface *surface, int x, int y, Uint8 *r, Uint8 *g, Uint8 *b, Uint8 *a)
{
    if(0 <= x && x < surface->w)
        if(0 <= y && y < surface->h) {
            int bpp = surface->format->BytesPerPixel;
            Uint8 *p = (Uint8*)surface->pixels + y * surface->pitch + x * bpp;
            SDL_GetRGBA(*(Uint32*)p, surface->format, r, g, b, a);
        }
}

bool Within_Surface(SDL_Surface *surface, int x, int y)
{
    return 0 <= x && x < surface->w && 0 <= y && y < surface->h;
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

void try_address(SDL_Surface *surface, int x, int y, Sint8 vx, Sint8 vy)
{
    int clr = 0;
    Uint8 a, b, c, t;
    int j = 0;

    Get_Pixel(surface, x, y, &c, &b, &a, &t);
    while(a != 0 || b != 0 || c != 0) {
        printf("%d: %d %d \t at 0x%x\n", j, x, y, 54 + (200*y + x)*3);

        if(x >= surface->w || y >= surface->h)
            break;

        vx ^= a;
        vy ^= b;
        clr ^= c;
        x += vx;
        y += vy;
        Get_Pixel(surface, x, y, &c, &b, &a, &t);
        j++;
    } 
}

void Dump_Image(SDL_Surface *surface)
{
    int x,y;
    Uint8 r, g, b, t;

    for (y=0; y < surface->h; y++) {
      for (x=0; x < surface->w; x++) {
        Get_Pixel(surface, x, y, &r, &g, &b, &t);
        printf("(%d,%d):\t%d %d %d\n", x,y, r,g,b);
      }
    }
}

void Draw_Image(SDL_Surface *surface, int x, int y, Sint8 vx, Sint8 vy)
{
    /* int x = 70, y = 79; */
    /* Sint8 vx = 18, vy = 26; */
    /* int x = 50, y = 22; */
    /* Sint8 vx = 24, vy = 34; */
    int clr = 0;
    Uint8 a, b, c, t;

    glClear(GL_COLOR_BUFFER_BIT);

    Get_Pixel(surface, x, y, &c, &b, &a, &t);
    while(a != 0 || b != 0 || c != 0) {
        //printf("%d %d\n", x, y);
        //printf("%d\n", 54 + (200*y + x)*3);

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
	    printf("%d %d\n", vx, vy);
        }
        x += vx;
        y += vy;
        Get_Pixel(surface, x, y, &c, &b, &a, &t);
#ifdef SLOWLY
        SDL_Delay(10);
        SDL_GL_SwapBuffers();
#endif
    } 

    SDL_GL_SwapBuffers();
}

