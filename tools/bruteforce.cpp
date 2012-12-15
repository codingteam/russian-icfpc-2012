#include <cstdio>
#include <cstdlib>

#include <iostream>
#include <fstream>
#include <set>
#include <utility>

#include <SDL/SDL.h>

#include "functions.h"

using namespace std;

void Read_Coords(set<pair<Sint8, Sint8> > &coords, const char *coord_file)
{
    ifstream fin(coord_file);

    int vx, vy;
    while(fin >> vx >> vy) {
      //cout << vx << ":" << vy << endl;
      coords.insert(make_pair<Sint8, Sint8>(vx, vy));
        coords.insert(make_pair<Sint8, Sint8>(-vx, -vy));
    }
}

bool Check_Address(SDL_Surface *surface, int x, int y, Sint8 vx, Sint8 vy,
                   const set<pair<Sint8, Sint8> > &coords)
{
    int lines = 0;
    int correct_lines = 0;

    int clr = 0;
    Uint8 a, b, c, t;

    Get_Pixel(surface, x, y, &c, &b, &a, &t);
    while((a != 0 || b != 0 || c != 0) &&
          Within_Surface(surface, x, y) //&&
	  //(correct_lines + .0) / lines >= 0.95 &&
          /*lines < 2000*/)
        {

            vx ^= a;
            vy ^= b;
            clr ^= c;

            if(clr != 0) {
	      if (vx > 8 || vy > 8)
		return false;
	      lines++;
	      if(coords.find(make_pair(vx, vy)) != coords.end())
		correct_lines++;
            }

            x += vx;
            y += vy;
            Get_Pixel(surface, x, y, &c, &b, &a, &t);
        }
    //cout << correct_lines << "/" << lines << endl;
    return correct_lines > 10 && (correct_lines + .0) / lines >= 0.95;
}

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./bruteforce <bmp-file> <coord-file1> <coord-file2> ...");
        return 1;
    }

    // BMP
    cout << "Reading image... " << endl;
    const char *bmp_file = argv[1];
    SDL_Surface *image = SDL_LoadBMP(bmp_file);
    if(image == NULL) {
        fprintf(stderr, "Unable to load bitmap: %s\n", SDL_GetError());
        return 1;
    }

    // Coords
    cout << "Reading coords..." << endl;
    set<pair<Sint8, Sint8> > coords;
    for(int i = 2; i < argc; ++i)
        Read_Coords(coords, argv[i]);

    // Bruteforce
    cout << "Bruteforce..." << endl;
    for(int x = 0; x < image->w; ++x)
        for(int y = 0; y < image->h; ++y)
	  for(int vx = -x; vx < image->w - x; ++vx)
                for(int vy = -y; vy < image->h - y; ++vy)
		{
		  //cout << "Checking " << x << ' ' << y << ' ' << (int)vx << ' ' << (int)vy << endl;
		  if(Check_Address(image, x, y, vx, vy, coords))
		    cout << x << ' ' << y << ' ' << (int)vx << ' ' << (int)vy << endl;
		}

    return 0;
}
