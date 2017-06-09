#include "ssf_cairo.h"

double deg2rad( double deg ) {
  return deg * M_PI / 180;
}

PixelBuffer* newPixelBuffer( Game* g ) {
  PixelBuffer* pb;

  pb = (PixelBuffer *)malloc( sizeof( PixelBuffer ));
  if( pb == NULL ) {
    return NULL;
  }
  pb->surface = cairo_image_surface_create( CAIRO_FORMAT_RGB24, 710, 626 );
  pb->ctx = cairo_create( pb->surface );

  return pb;
}


void freePixelBuffer( PixelBuffer* pb ) {
  free(pb);
}

void drawWireFrame( cairo_t *ctx, const WireFrame* wf, const Point* p, int angle ) {
  cairo_save( ctx );
  cairo_translate( ctx, p->x, p->y );
  cairo_rotate( ctx, -deg2rad( angle ));
  cairo_set_source_rgb( ctx, wf->r/255.0, wf->g/255.0, wf->b/255.0 );

  for (int i=0; i<wf->lineCount; i++) {
    cairo_move_to( ctx, wf->points[wf->lines[i].from].x, wf->points[wf->lines[i].from].y );
    cairo_line_to( ctx, wf->points[wf->lines[i].to].x, wf->points[wf->lines[i].to].y );
  }

  cairo_stroke( ctx );
  cairo_restore( ctx );
}

void drawHexagon( cairo_t *ctx, Hexagon *h ) {
  cairo_set_source_rgb( ctx, 0, 1, 0 );
  cairo_move_to( ctx, h->points[0].x, h->points[0].y );
  for (int i=1; i<6; i++) {
    cairo_line_to( ctx, h->points[i].x, h->points[i].y );
  }
  cairo_close_path( ctx );
  cairo_stroke( ctx );
}

void drawExplosion( cairo_t *ctx, const Point* p ) {
  cairo_set_line_width( ctx, 1.4 );
  int ofs = 0;
  for( int radius=15; radius<70; radius += 8 ) {
    ofs += 3;
    if( radius < 60 ) {
      cairo_set_source_rgb( ctx, 1,1,0 );
    } else {
      cairo_set_source_rgb( ctx, 1,0,0 );
    }
    for( int angle=0; angle<360; angle += 30 ) {
      cairo_arc( ctx, p->x, p->y, radius, deg2rad(angle+ofs),deg2rad(angle+ofs+10));
      cairo_stroke( ctx );
    }
  }
  cairo_set_source_rgb( ctx, 1,1,0);
  cairo_arc(ctx, p->x, p->y, 7, 0, M_PI*2);
  cairo_stroke( ctx );
}

void centeredText(cairo_t *ctx, char *text, int x, int y) {
  cairo_text_extents_t extents;
  cairo_text_extents(ctx, text, &extents);
  cairo_move_to(ctx, x - extents.width/2.0, y - extents.height/2.0);
  cairo_show_text(ctx, text);
}

void drawScore( cairo_t *ctx, int pnts, int vlner ) {
  int label_width = 89;
  int label_height = 32;
  /* int pad = 16; */
  int score_y = 520;
  double start = (710-89*2)/2;

  cairo_set_line_width( ctx, 1 );

  cairo_rectangle(ctx, start + 0.5, score_y + 0.5, label_width*2, label_height*2);
  cairo_set_source_rgb(ctx, 0, 0, 0);
  cairo_fill_preserve(ctx);

  cairo_move_to(ctx, start+89.5, score_y);
  cairo_line_to(ctx, start+89.5, score_y+label_height*2);
  cairo_move_to(ctx, start+0.5, score_y+label_height+0.5);
  cairo_line_to(ctx, start+0.5+label_width*2, score_y+label_height+0.5);
  cairo_set_source_rgb(ctx, 0, 1, 0);
  cairo_stroke(ctx);


  cairo_set_source_rgb(ctx, 0, 1, 0);
  cairo_select_font_face(ctx, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
  cairo_set_font_size( ctx, 20 );

  centeredText(ctx, "PNTS", start + 89/2, score_y+label_height); /* 18 */
  centeredText(ctx, "VLNER", start + 89 + 89/2, score_y+label_height); /* 18 */

  cairo_set_font_size( ctx, 24 );
  cairo_set_source_rgb(ctx, 1, 1, 0);

  char text[256];
  sprintf(text, "%d", pnts);
  centeredText(ctx, text, start + 89/2, score_y+label_height*2); /* 18 */
  sprintf(text, "%d", vlner);
  centeredText(ctx, text, start + 89 + 89/2, score_y+label_height*2); /* 18 */
}

void drawGameState( Game *g, cairo_t *ctx ) {
  cairo_set_line_width( ctx, 2 );
  cairo_set_source_rgb( ctx, 0, 0, 0);
  cairo_paint( ctx );

  drawHexagon( ctx, &g->bigHex );
  drawHexagon( ctx, &g->smallHex );

  if( g->ship.o.alive ) {
    drawWireFrame( ctx, &shipWireFrame, &g->ship.o.position, g->ship.o.angle );
  } else {
    drawExplosion( ctx, &g->ship.o.position );
  }
  if( g->fortress.o.alive ) {
    drawWireFrame( ctx, &fortressWireFrame, &g->fortress.o.position, g->fortress.o.angle );
  } else {
    drawExplosion( ctx, &g->fortress.o.position );
  }
  for (int i=0; i<MAX_MISSILES; i++) {
    if (g->missiles[i].o.alive) {
      drawWireFrame(ctx, &missileWireFrame, &g->missiles[i].o.position, g->missiles[i].o.angle);
    }
  }
  for (int i=0; i<MAX_SHELLS; i++) {
    if (g->shells[i].o.alive) {
      drawWireFrame(ctx, &shellWireFrame, &g->shells[i].o.position, g->shells[i].o.angle);
    }
  }
  drawScore( ctx, g->score.points, g->score.vulnerability );
}
