#include "space-fortress.hh"

WireFrame missileWireFrame;
WireFrame shellWireFrame;
WireFrame shipWireFrame;
WireFrame fortressWireFrame;

void initWireframes () {
  static bool initialized = false;
  if( initialized ) return;
  missileWireFrame.points[0] = {0,0};
  missileWireFrame.points[1] = { -25, 0 };
  missileWireFrame.points[2] = { -5, 5 };
  missileWireFrame.points[3] = { -5, -5 };
  missileWireFrame.pointCount = 4;
  missileWireFrame.lines[0] = { 0, 1 };
  missileWireFrame.lines[1] = { 0, 2 };
  missileWireFrame.lines[2] = { 0, 3 };
  missileWireFrame.lineCount = 3;
  missileWireFrame.r = 255;
  missileWireFrame.g = 255;
  missileWireFrame.b = 255;

  shellWireFrame.points[0] = { -8, 0 };
  shellWireFrame.points[1] = { 0, -6 };
  shellWireFrame.points[2] = { 16, 0 };
  shellWireFrame.points[3] = { 0, 6 };
  shellWireFrame.points[4] = { -8, 0 };
  shellWireFrame.pointCount = 5;
  shellWireFrame.lines[0] = { 0, 1 };
  shellWireFrame.lines[1] = { 1, 2 };
  shellWireFrame.lines[2] = { 2, 3 };
  shellWireFrame.lines[3] = { 3, 0 };
  shellWireFrame.lineCount = 4;
  shellWireFrame.r = 255;
  shellWireFrame.g = 0;
  shellWireFrame.b = 0;

  shipWireFrame.points[0] = { -18, 0 };
  shipWireFrame.points[1] = { 18, 0 };
  shipWireFrame.points[2] = { 0, 0 };
  shipWireFrame.points[3] = { -18, 18 };
  shipWireFrame.points[4] = { -18, -18 };
  shipWireFrame.pointCount = 5;
  shipWireFrame.lines[0] = { 0, 1 };
  shipWireFrame.lines[1] = { 3, 2 };
  shipWireFrame.lines[2] = { 2, 4 };
  shipWireFrame.lineCount = 3;
  shipWireFrame.r = 255;
  shipWireFrame.g = 255;
  shipWireFrame.b = 0;

  fortressWireFrame.points[0] = { 0, 0 };
  fortressWireFrame.points[1] = { 36, 0 };
  fortressWireFrame.points[2] = { 18, -18 };
  fortressWireFrame.points[3] = { 0, -18 };
  fortressWireFrame.points[4] = { 18, 18 };
  fortressWireFrame.points[5] = { 0, 18 };
  fortressWireFrame.pointCount = 6;
  fortressWireFrame.lines[0] = { 0, 1 };
  fortressWireFrame.lines[1] = { 3, 2 };
  fortressWireFrame.lines[2] = { 2, 4 };
  fortressWireFrame.lines[3] = { 4, 5 };
  fortressWireFrame.lineCount = 4;
  fortressWireFrame.r = 255;
  fortressWireFrame.g = 255;
  fortressWireFrame.b = 0;

  initialized = true;
}
