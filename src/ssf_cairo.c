#include "ssf_cairo.h"

#if CAIRO_HAS_FT_FONT
#include <cairo/cairo-ft.h>

cairo_font_face_t *loadFont( const char *filename ) {
  static FT_Library *cairoFT = NULL;
  static cairo_font_face_t *cairoFont = NULL;
  FT_Face face;
  FT_Error error;
  static const cairo_user_data_key_t key;
  int status;

  if( cairoFont != NULL ) return cairoFont;

  if( cairoFT == NULL ) {
    cairoFT = (FT_Library*)malloc( sizeof( FT_Library ));
    error = FT_Init_FreeType( cairoFT );
    if( error ) return NULL;
  }

  error = FT_New_Face (*cairoFT,
                       filename,
                       0,
                       &face);
  printf("the errors: %d %d\n", FT_Err_Ok, error);
  if (error != FT_Err_Ok) {
    if ( error == FT_Err_Unknown_File_Format ) {
      printf ("Unknown file format\n");
      return NULL;
    } else {
      printf ("some other error %d\n", error);
      return NULL;
    }
  }

  printf("try\n");
  cairoFont = cairo_ft_font_face_create_for_ft_face (face, 0);
  printf("create %p\n", cairoFont);
  status = cairo_font_face_set_user_data (cairoFont, &key,
                                          face, (cairo_destroy_func_t) FT_Done_Face);
  printf("grrr! %p\n", cairoFont);
  if (status) {
    printf("can't set user data\n");
    cairo_font_face_destroy (cairoFont);
    FT_Done_Face (face);
    cairoFont = NULL;
  }

  return cairoFont;
}

#endif

double deg2rad( double deg ) {
  return deg * M_PI / 180;
}

PixelBuffer* newPixelBuffer( Game* g, int width, int height ) {
  PixelBuffer* pb;

  pb = (PixelBuffer *)malloc( sizeof( PixelBuffer ));
  if( pb == NULL ) {
    return NULL;
  }
  pb->surface = cairo_image_surface_create( CAIRO_FORMAT_RGB24,
                                            width==0 ? g->config.width : width,
                                            height==0 ? g->config.height : height );
  pb->raw = cairo_image_surface_get_data( pb->surface );
  pb->height = cairo_image_surface_get_height( pb->surface );
  pb->stride = cairo_image_surface_get_stride( pb->surface );

  printf("new pb: %p %d %d : %d\n", pb->raw, pb->height, pb->stride, ((char *)pb->raw)[10]);

  return pb;
}

void freePixelBuffer( PixelBuffer* pb ) {
  cairo_surface_destroy( pb->surface );
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
  /* cairo_set_line_width( ctx, 1.4 ); */
  cairo_set_line_width( ctx, 2 );
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

  /* cairo_save(ctx); */
  /* cairo_rectangle(ctx, x - extents.width/2.0, y - extents.height/2.0, extents.width+1, extents.height+1); */
  /* cairo_set_source_rgb(ctx, .5, .5, .5); */
  /* cairo_fill(ctx); */
  /* cairo_restore(ctx); */

  cairo_move_to(ctx, x - extents.width/2.0, y + extents.height/2.0);
  cairo_show_text(ctx, text);
}

void drawScore( cairo_t *ctx, int pnts, int vlner ) {
  int label_width = 89;
  int label_height = 32;
  /* int pad = 16; */
  int score_y = 504;
  double start = (580-89*2)/2;

  cairo_set_line_width( ctx, 1.7 );

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
  cairo_select_font_face(ctx, "sans-serif", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);
  /* cairo_set_font_face( ctx, loadFont( "freesansbold.ttf" )); */
  cairo_set_font_size( ctx, 20 );

  centeredText(ctx, "PNTS", start + 89/2, score_y+label_height/2); /* 18 */
  centeredText(ctx, "VLNER", start + 89 + 89/2, score_y+label_height/2); /* 18 */

  cairo_set_font_size( ctx, 24 );
  cairo_set_source_rgb(ctx, 1, 1, 0);

  char text[256];
  sprintf(text, "%d", pnts);
  centeredText(ctx, text, start + 89/2, score_y+label_height*1.5); /* 18 */
  sprintf(text, "%d", vlner);
  centeredText(ctx, text, start + 89 + 89/2, score_y+label_height*1.5); /* 18 */
}

void centerSanely( Game *g, cairo_t *ctx, int sw, int sh ) {
  int bighex_height = g->config.bigHex*sin(M_PI*2/3) * 2;
  int score_pad = 520-(315+bighex_height/2);
  int score_height = 32*2;
  int pad = 10;
  int ofsx, ofsy;

  ofsx = (sw - g->config.width)/2;
  ofsy = (sh - g->config.height)/2;

  /* printf("use this one %d %d %d %d\n", sh, bighex_height, score_pad, score_height); */
  if( sh < bighex_height + score_pad + score_height + pad) {
    ofsy = -(g->config.height/2 - bighex_height/2)+2;
  } else {
    ofsy = sh/2 - (bighex_height + score_pad + score_height)/2 - (g->config.height/2 - bighex_height/2);
  }

  cairo_translate( ctx, ofsx, ofsy );
}

void tinyCenterSanely( Game *g, cairo_t *ctx, int sw, int sh ) {
  int bighex_height = g->config.bigHex*sin(M_PI*2/3) * 2;
  int score_pad = 520-(315+bighex_height/2);
  int score_height = 32*2;
  int pad = 10;
  int ofsx, ofsy;

  ofsx = (sw - g->config.width)/2+185;
  ofsy = (sh - g->config.height)/2;

  /* printf("use this one %d %d %d %d\n", sh, bighex_height, score_pad, score_height); */
  if( sh < bighex_height + score_pad + score_height + pad) {
    ofsy = -(g->config.height/2 - bighex_height/2)+2;
  } else {
    ofsy = sh/2 - (bighex_height + score_pad + score_height)/2 - (g->config.height/2 - bighex_height/2);
  }

  ofsy += 71;
  cairo_translate( ctx, ofsx, ofsy );
}

void drawJustGameStuff( cairo_t *ctx, Game *g ) {
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
    double d = sqrt(pow(g->shells[i].o.position.x - g->fortress.o.position.x, 2) + pow(g->shells[i].o.position.y - g->fortress.o.position.y, 2));
    if (g->shells[i].o.alive && d > 21) {
      drawWireFrame(ctx, &shellWireFrame, &g->shells[i].o.position, g->shells[i].o.angle);
    }
  }
}

void drawGameState( Game *g, cairo_surface_t *surface ) {
  cairo_t *ctx = cairo_create( surface );
  centerSanely( g, ctx, cairo_image_surface_get_width( surface ), cairo_image_surface_get_height( surface ));
  cairo_set_line_width( ctx, 2 );
  cairo_set_source_rgb( ctx, 0, 0, 0 );
  cairo_paint( ctx );

  drawJustGameStuff( ctx, g );
  drawScore( ctx, g->score.points, g->score.vulnerability );
  cairo_destroy( ctx );
}

void drawTinyGameState( Game *g, cairo_surface_t *surface ) {
  cairo_t *ctx = cairo_create( surface );
  cairo_scale( ctx, .3, .3 );
  tinyCenterSanely( g, ctx, cairo_image_surface_get_width( surface ), cairo_image_surface_get_height( surface ));

  cairo_set_line_width( ctx, 3 );
  cairo_set_source_rgb( ctx, 0, 0, 0 );
  cairo_paint( ctx );

  drawJustGameStuff( ctx, g );
  drawScore( ctx, g->score.points, g->score.vulnerability );
  cairo_destroy( ctx );
}

void drawGameStateScaled( Game *g, cairo_surface_t *surface, float scale) {
  cairo_t *ctx = cairo_create( surface );
  if ( scale < 1 )
    cairo_scale( ctx, scale, scale );

  cairo_set_line_width( ctx, 2 );
  cairo_set_source_rgb( ctx, 0, 0, 0 );
  cairo_paint( ctx );

  drawJustGameStuff( ctx, g );
  drawScore( ctx, g->score.points, g->score.vulnerability );
  cairo_destroy( ctx );
}
