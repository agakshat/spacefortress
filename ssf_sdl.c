#include <SDL2/SDL.h>
/* #include <SDL2_ttf/SDL_ttf.h> */
/* #include <SDL2_image/SDL_image.h> */
/* #include <SDL2_mixer/SDL_mixer.h> */

#include "ssf.h"

void drawWireFrame(SDL_Renderer *r, const WireFrame *wf, const Point *p, int angle) {
  Point points[MAX_WIREFRAME_POINTS];
  double s = sin(angle * M_PI / 180);
  double c = cos(angle * M_PI / 180);
  for (int i=0; i<wf->pointCount; i++) {
    points[i].x = wf->points[i].x * c - wf->points[i].y * s + p->x;
    points[i].y = -(wf->points[i].x * s + wf->points[i].y * c) + p->y;
  }

  SDL_SetRenderDrawColor(r, wf->r, wf->g, wf->b, SDL_ALPHA_OPAQUE);
  for (int i=0; i<wf->lineCount; i++) {
    SDL_RenderDrawLine(r,
                       points[wf->lines[i].from].x, points[wf->lines[i].from].y,
                       points[wf->lines[i].to].x, points[wf->lines[i].to].y);
  }
}

void drawHexagon(SDL_Renderer *r, const Hexagon *h) {
  SDL_SetRenderDrawColor(r, 0, 255, 0, SDL_ALPHA_OPAQUE);
  for (int i=0; i<6; i++) {
    SDL_RenderDrawLine(r,
                       h->points[i].x, h->points[i].y,
                       h->points[(i+1)%6].x, h->points[(i+1)%6].y);
  }
}

void drawGame(SDL_Renderer *r, Game *g) {
  SDL_SetRenderDrawColor(r, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(r);

  drawHexagon(r, &g->bigHex);
  drawHexagon(r, &g->smallHex);
  if (g->ship.o.alive) {
    drawWireFrame(r, &shipWireFrame, &g->ship.o.position, g->ship.o.angle);
  }
  if (g->fortress.o.alive) {
    drawWireFrame(r, &fortressWireFrame, &g->fortress.o.position, g->fortress.o.angle);
  }

  for (int i=0; i<MAX_MISSILES; i++) {
    if (g->missiles[i].o.alive) {
      drawWireFrame(r, &missileWireFrame, &g->missiles[i].o.position, g->missiles[i].o.angle);
    }
  }
  for (int i=0; i<MAX_SHELLS; i++) {
    if (g->shells[i].o.alive) {
      drawWireFrame(r, &shellWireFrame, &g->shells[i].o.position, g->shells[i].o.angle);
    }
  }

}

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

int main(int argc, char **argv) {
  SDL_Window *w;
  SDL_Renderer *r;
  Game *g;

  printf("game: %lu\n", sizeof(Game));
  printf("config: %lu\n", sizeof(Config));
  printf("ship: %lu\n", sizeof(Ship));
  printf("fortress: %lu\n", sizeof(Fortress));
  printf("missile: %lu\n", sizeof(Missile));
  printf("shell: %lu\n", sizeof(Shell));

  SDL_Init(SDL_INIT_VIDEO);

  w = SDL_CreateWindow("Space Fortress",
                       SDL_WINDOWPOS_CENTERED,
                       SDL_WINDOWPOS_CENTERED,
                       710, 600, 0);
  r = SDL_CreateRenderer(w, -1, SDL_RENDERER_ACCELERATED);


  int last;
  int now = SDL_GetTicks();
  printf("start\n");
  for (int i=0; i<2000; i++) {
    g = makeExplodeGame();
    while (!isGameOver(g)) stepOneTick(g, 33);
    freeGame(g);
  }
  printf("ellapse time: %d\n", SDL_GetTicks() - now);

  /* g = makeAutoTurnGame(); */
  g = makeExplodeGame();

  while (!isGameOver(g)) {
    last = now;
    now = SDL_GetTicks();
    readEvents(g);
    stepOneTick(g, now-last);
    drawGame(r, g);
    SDL_RenderPresent(r);
    pumpAndWait(now + 33);
  }

  free(g);

  return 0;
}
