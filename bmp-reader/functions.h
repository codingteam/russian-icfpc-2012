#define SCREEN_WIDTH 800
#define SCREEN_HEIGHT 600

void Get_Pixel(SDL_Surface *surface, int x, int y, Uint8 *r, Uint8 *g, Uint8 *b, Uint8 *a);

void Event_Loop();

void try_address(SDL_Surface *surface, int x, int y, Sint8 vx, Sint8 vy);

void Draw_Image(SDL_Surface *surface, int x, int y, Sint8 vx, Sint8 vy);

