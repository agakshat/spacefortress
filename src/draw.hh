#ifndef __SF_CAIRO_H__
#define __SF_CAIRO_H__

#include "space-fortress.hh"
#include "cairo/cairo.h"

extern "C" {

typedef struct {
  cairo_surface_t* surface;
  void* raw;
  int width, height, stride;
  double scale, line_width;
  bool grayscale;
} PixelBuffer;

PixelBuffer* newPixelBuffer( int width, int height, double scale, double line_width, bool grayscale );
void freePixelBuffer( PixelBuffer* pb );
void drawGameStateScaled( Game *g, cairo_surface_t *surface, float scale, float line_width, bool grayscale);

}

#endif
