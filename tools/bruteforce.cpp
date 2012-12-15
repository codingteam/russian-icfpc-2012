#include <iostream>
#include <fstream>
#include <set>
#include <limits>

#include <SDL/SDL.h>

bool In_Image(SDL_Surface *image, int x, int y)
{
    return 0 <= x && x < image->w && 0 <= y && y < image->h;
}

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./bruteforce <bmp-file>");
        return 1;
    }

    const char *bmp_file = argv[1];

    SDL_Surface image = SDL_LoadBMP(bmp_file);

    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    Sint8 v_min = numeric_limits<Sint8>::min();
    Sint8 v_max = numeric_limits<Sint8>::max();
    for(int x = 0; x < image->w; ++x)
        for(int y = 0; y < image->h; ++y)
            for(Sint8 vx = v_min; vx <= v_max; ++vx)
                for(Sint8 vy = v_min; vy <= v_max; ++vy)
                    if(In_Image(image, x + vx, y + vy)) {
                        int lines = 0;
                        int correct_lines = 0;

                        
                    }
    
    return 0;
}
