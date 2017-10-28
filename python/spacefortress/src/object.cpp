#include "space-fortress.hh"

Object::Object() {
  mAlive = false;
  mAngle = 0;
  mCollisionRadius = 0;
}

Object::~Object() {
}

bool Object::collided(const Object &o) {
  double d = sqrt(pow(mPos.mX - o.mPos.mX, 2) + pow(mPos.mY - o.mPos.mY, 2));
  return d <= (mCollisionRadius + o.mCollisionRadius);
}
