#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>

#include <SDL/SDL.h>

#include "functions.h"

using namespace std;

int buffer[60000];

int main(int argc, char **argv)
{
    if(argc < 2) {
        printf("Usage: ./dump-data-segment <bmp-file> <data-file>\n");
        return 1;
    }

    const char *bmp_file = argv[1];
    const char *data_file = argv[2];

    FILE *data = fopen(data_file, "r");
    for(int i = 0; i < 60000; ++i)
        fscanf(data, "%d", &buffer[i]);
    fclose(data);

    FILE *bmp = fopen(bmp_file, "ab");
    fwrite(buffer, 3, 80000, bmp);
    fclose(bmp);

    return 0;
}
