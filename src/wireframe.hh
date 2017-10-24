#include "space-fortress.hh"

#define MAX_WIREFRAME_POINTS 10
#define MAX_WIREFRAME_LINES 10

typedef struct {
  int from, to;
} WireFrameLine;

typedef struct {
  Vector points[MAX_WIREFRAME_POINTS];
  int pointCount;
  WireFrameLine lines[MAX_WIREFRAME_LINES];
  int lineCount;
  int r, g, b;
} WireFrame;

extern WireFrame missileWireFrame, shellWireFrame, shipWireFrame, fortressWireFrame;
void initWireframes();
