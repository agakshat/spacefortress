#include "space-fortress.hh"

class Hexagon {
public:
  Vector mPoints[6];
  int mRadius;
  Hexagon();
  ~Hexagon();

  void operator=( const Hexagon &h );
  void setRadius( int radius );
  bool isInside( const Vector &point );
};
