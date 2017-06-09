#ifndef __SSF_CAIRO_H__
#define __SSF_CAIRO_H__

#include "ssf.h"
#include "cairo/cairo.h"

typedef struct {
  cairo_surface_t* surface;
  cairo_t* ctx;
  void* raw;
} PixelBuffer;

PixelBuffer* newPixelBuffer();
void freePixelBuffer( PixelBuffer *pb );
void drawGameState( Game *g, cairo_t* ctx );

#endif
