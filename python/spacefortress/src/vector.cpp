#include "space-fortress.hh"

Vector::Vector() {
  mX = 0;
  mY = 0;
}

Vector::Vector( double x, double y ) {
  mX = x;
  mY = y;
}

Vector::~Vector() { }

void Vector::operator+( const Vector &v ) {
  mX += v.mX;
  mY += v.mY;
}

void Vector::operator-( const Vector &v ) {
  mX -= v.mX;
  mY -= v.mY;
}

void Vector::operator*( const double x ) {
  mX *= x;
  mY *= x;
}

double Vector::norm() const {
  return sqrt(mX*mX+mY*mY);
}

double deg2rad(double a) {
  return a * M_PI / 180;
}

double rad2deg(double a) {
  return a / M_PI * 180;
}

double stdAngle(double a) {
  if (a <= -360 || a >= 360) a = fmod(a, 360);
  if (a < 0) a += 360;
  return a;
}

double angleTo(const Vector &p1, const Vector &p2) {
  double a = atan2(p2.mY-p1.mY, p2.mX-p1.mX);
  if (a < 0) a += M_PI*2;
  return rad2deg(a);
}
