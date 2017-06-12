#include <SDL2/SDL.h>

#include "ssf_cairo.h"

void pumpAndWait(int until) {
  while (SDL_GetTicks() < until) {
    SDL_PumpEvents();
    SDL_Delay(1);
  }
}

KeySym getKeySym(const SDL_Event *e) {
  switch (e->key.keysym.sym) {
  case SDLK_w:
    return THRUST_KEY;
  case SDLK_a:
    return LEFT_KEY;
  case SDLK_d:
    return RIGHT_KEY;
  case SDLK_SPACE:
    return FIRE_KEY;
  default:
    return NO_KEY;
  }
}

void readEvents(Game *g) {
  SDL_Event e;
  KeySym key;
  while (SDL_PollEvent(&e) == 1) {
    switch (e.type) {
    case SDL_KEYDOWN:
      if (e.key.keysym.sym == SDLK_ESCAPE) exit(0);
      key = getKeySym(&e);
      if (key != NO_KEY) pressKey(g, key);
      break;
    case SDL_KEYUP:
      key = getKeySym(&e);
      if (key != NO_KEY) releaseKey(g, key);
      break;
    case SDL_QUIT:
      exit(0);
      break;
    }
  }
}

void draw( SDL_Renderer *r, SDL_Texture *t, Game *g ) {
  void *pixels;
  SDL_Rect view;
  int pitch;

  SDL_RenderGetViewport( r, &view );
  SDL_LockTexture( t, NULL, &pixels, &pitch );

  cairo_surface_t *s = cairo_image_surface_create_for_data( pixels, CAIRO_FORMAT_ARGB32, view.w, view.h, pitch );
  drawGameState( g, s );
  cairo_surface_destroy( s );

  SDL_UnlockTexture( t );
  SDL_RenderCopy( r, t, NULL, NULL );
}

int main(int argc, char **argv) {
  SDL_Window *w;
  SDL_Renderer *r;
  Game *g;
  /* PixelBuffer *pb; */
  SDL_Texture *texture;

  /* g = makeAutoTurnGame(); */
  g = makeExplodeGame();

  /* int width = g->config.width; */
  /* int height = g->config.height; */
  int width = 405;
  int height = 450;

  SDL_Init(SDL_INIT_VIDEO);

  w = SDL_CreateWindow("Space Fortress",
                       SDL_WINDOWPOS_CENTERED,
                       SDL_WINDOWPOS_CENTERED,
                       width, height, 0);
  r = SDL_CreateRenderer(w, -1, SDL_RENDERER_ACCELERATED);

  texture = SDL_CreateTexture( r, SDL_PIXELFORMAT_ARGB8888,
                               SDL_TEXTUREACCESS_STREAMING,
                               width, height );


  openLog(g, "data/log.txt");

  int last;
  int now = SDL_GetTicks();
  while (!isGameOver(g)) {
    last = now;
    now = SDL_GetTicks();
    readEvents(g);
    stepOneTick(g, now-last);
    draw(r, texture, g);
    SDL_RenderPresent(r);
    logGameState(g);
    pumpAndWait(now + 33);
  }
  freeGame(g);

  return 0;
}
