#ifndef __SSF_CAIRO_H__
#define __SSF_CAIRO_H__

#include "ssf.h"
#include "cairo/cairo.h"

typedef struct {
  cairo_surface_t* surface;
  void* raw;
  int height, stride;
} PixelBuffer;

PixelBuffer* newPixelBuffer( Game* g, int width, int height );
void freePixelBuffer( PixelBuffer* pb );
void drawGameStateScaled( Game *g, cairo_surface_t *surface, float scale, float ls);

#endif