#include "draw.hh"

#if CAIRO_HAS_FT_FONT
#include <cairo/cairo-ft.h>

static cairo_font_face_t *loadFont( const char *filename ) {
  static FT_Library *cairoFT = NULL;
  static cairo_font_face_t *cairoFont = NULL;
  FT_Face face;
  FT_Error error;
  static const cairo_user_data_key_t key = {};
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

PixelBuffer* newPixelBuffer( int width, int height, int vp_x, int vp_y, int vp_width, int vp_height, double line_width, bool grayscale ) {
  PixelBuffer* pb;

  pb = (PixelBuffer *)malloc( sizeof( PixelBuffer ));
  if( pb == NULL ) {
    return NULL;
  }
  pb->surface = cairo_image_surface_create( CAIRO_FORMAT_RGB24, width, height );
  pb->raw = cairo_image_surface_get_data( pb->surface );
  pb->width = cairo_image_surface_get_width( pb->surface );
  pb->height = cairo_image_surface_get_height( pb->surface );
  pb->stride = cairo_image_surface_get_stride( pb->surface );
  pb->grayscale = grayscale;
  pb->dx = -vp_x;
  pb->dy = -vp_y;
  pb->scale_x = (double)width / (double)vp_width;
  pb->scale_y = (double)height / (double)vp_height;
  pb->line_width = line_width;

  return pb;
}

void freePixelBuffer( PixelBuffer* pb ) {
  cairo_surface_destroy( pb->surface );
  free(pb);
}

static void drawWireFrame( cairo_t *ctx, const WireFrame* wf, const Vector* p, int angle, double ls, double grayscale ) {
  int i;
  cairo_save( ctx );
  cairo_translate( ctx, p->mX, p->mY );
  cairo_rotate( ctx, deg2rad( angle ));
  cairo_set_line_width( ctx, ls);
  if (grayscale<0 || grayscale>1)
    cairo_set_source_rgb( ctx, wf->r/255.0, wf->g/255.0, wf->b/255.0 );
  else
    cairo_set_source_rgb( ctx, grayscale, grayscale, grayscale );

  for (i=0; i<wf->lineCount; i++) {
    cairo_move_to( ctx, wf->points[wf->lines[i].from].mX, wf->points[wf->lines[i].from].mY );
    cairo_line_to( ctx, wf->points[wf->lines[i].to].mX, wf->points[wf->lines[i].to].mY );
  }

  cairo_stroke( ctx );
  cairo_restore( ctx );
}

static void drawHexagon( cairo_t *ctx, Hexagon *h, double grayscale ) {
  int i;
  if (grayscale<0 || grayscale>1)
    cairo_set_source_rgb( ctx, 0, 1, 0 );
  else
    cairo_set_source_rgb( ctx, grayscale, grayscale, grayscale );
  cairo_move_to( ctx, h->mPoints[0].mX, h->mPoints[0].mY );
  for (i=1; i<6; i++) {
    cairo_line_to( ctx, h->mPoints[i].mX, h->mPoints[i].mY );
  }
  cairo_close_path( ctx );
  cairo_stroke( ctx );
}

static void drawExplosion( cairo_t *ctx, const Vector* p, float ls, bool grayscale ) {
  int radius, angle, ofs;
  /* cairo_set_line_width( ctx, 1.4 ); */
  cairo_set_line_width( ctx, ls );
  ofs = 0;
  for (radius=15; radius<70; radius += 8 ) {
    ofs += 3;
    if( radius < 60 ) {
      if (grayscale)
        cairo_set_source_rgb( ctx, .75, .75, .75 );
      else
        cairo_set_source_rgb( ctx, 1,1,0 );
    } else {
      if (grayscale)
        cairo_set_source_rgb( ctx, .5, .5, .5 );
      else
        cairo_set_source_rgb( ctx, 1,0,0 );
    }
    for (angle=0; angle<360; angle += 30 ) {
      cairo_arc( ctx, p->mX, p->mY, radius, deg2rad(angle+ofs),deg2rad(angle+ofs+10));
      cairo_stroke( ctx );
    }
  }
  if (grayscale)
    cairo_set_source_rgb( ctx, .75, .75, .75 );
  else
    cairo_set_source_rgb( ctx, 1,1,0);
  cairo_arc(ctx, p->mX, p->mY, 7, 0, M_PI*2);
  cairo_stroke( ctx );
}

static void centeredText(cairo_t *ctx, char *text, int x, int y) {
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

static void drawScore( cairo_t *ctx, int pnts, float ls, bool grayscale) {
  int score_y = 290;
  double start = 355;

  cairo_select_font_face(ctx, "monospace", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_BOLD);

  cairo_set_font_size( ctx, 30 );
  cairo_set_source_rgb(ctx, .5, .5, .5);

  char text[256];
  sprintf(text, "%07d", pnts);
  centeredText(ctx, text, start, score_y-193);
}

static void drawKeyState( cairo_t *ctx, float ls, bool left, bool right, bool thrust, bool fire, bool grayscale) {
  int score_y = 210;
  double start = 210;

  cairo_set_line_width( ctx, ls-1 );

  if (fire)
    cairo_set_source_rgb(ctx, .75, .75, .75);
  else
    cairo_set_source_rgb(ctx, .25, .25, .25);
  cairo_rectangle(ctx, start-87.5, score_y-195, 25, 10);
  cairo_fill(ctx);
  if (thrust)
    cairo_set_source_rgb(ctx, .75, .75, .75);
  else
    cairo_set_source_rgb(ctx, .25, .25, .25);
  cairo_rectangle(ctx, start-37.5, score_y-195, 25, 10);
  cairo_fill(ctx);
  if (left)
    cairo_set_source_rgb(ctx, .75, .75, .75);
  else
    cairo_set_source_rgb(ctx, .25, .25, .25);
  cairo_rectangle(ctx, start+12.5, score_y-195, 25, 10);
  cairo_fill(ctx);
  if (right)
    cairo_set_source_rgb(ctx, .75, .75, .75);
  else
    cairo_set_source_rgb(ctx, .25, .25, .25);
  cairo_rectangle(ctx, start+62.5, score_y-195, 25, 10);
  cairo_fill(ctx);
}

static void drawVlner( cairo_t *ctx, int vlner, bool kill, float ls, bool grayscale) {
  // int score_y = 210;
  // double start = 210;
  int score_y = 315+20;
  double start = 355;

  cairo_set_line_width( ctx, ls-1 );

  cairo_set_source_rgb(ctx, .33, .33, .33);
  cairo_rectangle(ctx, start-100, score_y+187, 200, 10);
  cairo_fill(ctx);

  if (kill)
    cairo_set_source_rgb(ctx, 1, 1, 1);
  else
    cairo_set_source_rgb(ctx, .66, .66, .66);
  cairo_rectangle(ctx, start-100, score_y+187, 20*(vlner>10 ? 10 : vlner), 10);
  cairo_fill(ctx);
}

static void drawJustGameStuff( cairo_t *ctx, Game *g, float ls, bool grayscale ) {
  int i;

  drawHexagon( ctx, &g->mBighex, grayscale ? 1 : -1 );
  drawHexagon( ctx, &g->mSmallhex, grayscale ? 1 : -1 );

  if( g->mShip.mAlive ) {
    drawWireFrame( ctx, &shipWireFrame, &g->mShip.mPos, g->mShip.mAngle, ls, grayscale ? 1 : -1 );
  } else {
    drawExplosion( ctx, &g->mShip.mPos, ls, grayscale);
  }
  if( g->mFortress.mAlive ) {
    drawWireFrame( ctx, &fortressWireFrame, &g->mFortress.mPos, g->mFortress.mAngle, ls, grayscale ? 1 : -1 );
  } else {
    drawExplosion( ctx, &g->mFortress.mPos, ls, grayscale);
  }
  for (i=0; i<MAX_MISSILES; i++) {
    if (g->mMissiles[i].mAlive) {
      drawWireFrame(ctx, &missileWireFrame, &g->mMissiles[i].mPos, g->mMissiles[i].mAngle, ls, grayscale ? 1 : -1 );
    }
  }
  for (i=0; i<MAX_SHELLS; i++) {
    double d = sqrt(pow(g->mShells[i].mPos.mX - g->mFortress.mPos.mX, 2) + pow(g->mShells[i].mPos.mY - g->mFortress.mPos.mY, 2));
    if (g->mShells[i].mAlive && d > 21) {
      drawWireFrame(ctx, &shellWireFrame, &g->mShells[i].mPos, g->mShells[i].mAngle, ls, grayscale ? 1 : -1 );
    }
  }
}

void drawGameStateScaled( Game *g, PixelBuffer *pb) {
  cairo_t *ctx = cairo_create( pb->surface );

  cairo_scale( ctx, pb->scale_x, pb->scale_y );
  cairo_translate( ctx, pb->dx, pb->dy );
  cairo_set_line_width( ctx, pb->line_width );
  cairo_set_source_rgb( ctx, 0, 0, 0 );
  cairo_paint( ctx );

  drawJustGameStuff( ctx, g, pb->line_width, pb->grayscale );
  // drawKeyState( ctx, ls, g->keys.left, g->keys.right, g->keys.thrust, g->keys->fire, grayscale);
  drawScore( ctx, g->mScore.mPoints, pb->line_width, pb->grayscale);
  drawVlner( ctx, g->mScore.mVulnerability, (g->mScore.mVulnerability > 10) && (g->mFortress.mVulnerabilityTimer < g->mConfig->getInt("fortressVulnerabilityTime")), pb->line_width, pb->grayscale);
  cairo_destroy( ctx );
}
