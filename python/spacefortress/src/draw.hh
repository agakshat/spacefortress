#ifndef __SF_CAIRO_H__
#define __SF_CAIRO_H__

#include "space-fortress.hh"
#include "cairo/cairo.h"

extern "C" {

typedef struct {
  cairo_surface_t* surface;
  void* raw;
  int width, height, stride;
  int dx, dy;
  double scale_x, scale_y, line_width;
  bool grayscale;
} PixelBuffer;

PixelBuffer* newPixelBuffer( int width, int height, int vp_x, int vp_y, int vp_width, int vp_height, double line_width, bool grayscale );
void freePixelBuffer( PixelBuffer* pb );
void drawGameStateScaled( Game *g, PixelBuffer *pb);

}

#endif
