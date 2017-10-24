#include "space-fortress.hh"

class Object {
public:
  Vector mPos, mVel;
  double mAngle;
  int mCollisionRadius;
  bool mAlive;

  Object();
  ~Object();

  bool collided(const Object &o);
};
