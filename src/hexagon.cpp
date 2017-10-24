#include "space-fortress.hh"

Hexagon::Hexagon() {
  mRadius = 0;
}

Hexagon::~Hexagon() { }

void Hexagon::operator=( const Hexagon &h ) {
  setRadius( h.mRadius );
}

void Hexagon::setRadius( int radius ) {
  double x1 = floor(355-radius);
  double x2 = floor(355-radius*0.5);
  double x3 = floor(355+radius*0.5);
  double x4 = floor(355+radius);
  double y1 = 315;
  double y2 = floor(315-radius*sin(M_PI*2/3));
  double y3 = floor(315+radius*sin(M_PI*2/3));
  mPoints[0].mX = x1;
  mPoints[0].mY = y1;
  mPoints[1].mX = x2;
  mPoints[1].mY = y2;
  mPoints[2].mX = x3;
  mPoints[2].mY = y2;
  mPoints[3].mX = x4;
  mPoints[3].mY = y1;
  mPoints[4].mX = x3;
  mPoints[4].mY = y3;
  mPoints[5].mX = x2;
  mPoints[5].mY = y3;
  mRadius = radius;
}

bool Hexagon::isInside(const Vector &p) {
  for (int i=0; i<6; i++) {
    double nx, ny, dx, dy;
    nx = -(mPoints[(i+1)%6].mY - mPoints[i].mY);
    ny =   mPoints[(i+1)%6].mX - mPoints[i].mX;
    dx = p.mX - mPoints[i].mX;
    dy = p.mY - mPoints[i].mY;
    if (nx * dx + ny * dy < 0) {
      return false;
    }
  }
  return true;
}
