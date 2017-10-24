#include "space-fortress.hh"

class Vector {
public:
  double mX, mY;

  Vector();
  Vector( double x, double y );
  ~Vector();

  void operator+( const Vector &v );
  void operator-( const Vector &v );
  void operator*( const double x );
  double norm() const;
};

double deg2rad(double a);
double rad2deg(double a);
double stdAngle(double a);
double angleTo(const Vector &p1, const Vector &p2);
