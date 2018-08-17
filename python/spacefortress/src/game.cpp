#include "space-fortress.hh"

Ship::Ship() : Object() {
   mFireFlag = false;
   mThrustFlag = false;
   mLeftFlag = false;
   mRightFlag = false;
}

Ship::~Ship() { }

Fortress::Fortress() : Object() {
  mLastAngle = 0;
}

Fortress::~Fortress() { }

Game::Game( Config *config ) {
  mConfig = config;

  mKeys.events.clear();
  mKeys.processed = false;

  mBighex.setRadius( config->getInt( "bigHex" ));
  mSmallhex.setRadius( config->getInt( "smallHex" ));

  mShip.mCollisionRadius = mConfig->getInt("shipCollisionRadius");
  mShip.mFireFlag = false;
  mShip.mThrustFlag = false;
  mShip.mLeftFlag = false;
  mShip.mRightFlag = false;
  mShip.mTurnFlag = NO_TURN;

  resetShip();

  mFortress.mAlive = true;
  mFortress.mCollisionRadius = mConfig->getInt("fortressCollisionRadius");
  mFortress.mPos.mX = 355;
  mFortress.mPos.mY = 315;
  mFortress.mAngle = 180;
  mFortress.mLastAngle = 0;

  mScore.mPoints = 0;
  mScore.mRawPoints = 0;
  mScore.mVulnerability = 0;
  mBonus = 0;
  mDestroyFortressExtraPoints = 0;
  mReward = 0;

  mStats.bigHexDeaths = 0;
  mStats.smallHexDeaths = 0;
  mStats.shellDeaths = 0;
  mStats.shipDeaths = 0;
  mStats.resets = 0;
  mStats.destroyedFortresses = 0;
  mStats.missedShots = 0;
  mStats.totalShots = 0;
  mStats.totalThrusts = 0;
  mStats.totalLefts = 0;
  mStats.totalRights = 0;
  mStats.vlnerIncs = 0;
  mStats.maxVlner = 0;

  mThrustDurations.clear();
  mShotDurations.clear();
  mShotIntervalsInvul.clear();
  mShotIntervalsVul.clear();

  mTick = 0;
  mTime = 0;

  mFortress.mTimer = 0;
  mFortress.mDeathTimer = 0;
  mShip.mFireTimer = 0;
  mShip.mThrustTimer = 0;
  mShip.mLeftTimer = 0;
  mShip.mRightTimer = 0;
  mFortress.mVulnerabilityTimer += mConfig->getInt("fortressVulnerabilityTime");
  mShip.mDeathTimer = 0;

  resetTick();
}

Game::~Game() {
}

void Game::playSound( Sound s ) {
}

void Game::maybeResetKeyEvents() {
  if( mKeys.processed == true ) {
    mKeys.events.clear();
    mKeys.processed = false;
  }
}

void Game::reward(float amount) {
  mReward += amount;
  mScore.mRawPoints += amount;
  mScore.mPoints += amount;
  if( mScore.mPoints < 0 ) mScore.mPoints = 0;
}

void Game::penalize(float amount) {
  reward( -amount );
}

void Game::pressKey(KeySym sym) {
  Key k;
  k.sym = sym;
  k.state = true;
  maybeResetKeyEvents();
  mKeys.events.push_back(k);
}

void Game::releaseKey(KeySym sym) {
  Key k;
  k.sym = sym;
  k.state = false;
  maybeResetKeyEvents();
  mKeys.events.push_back(k);
}

void Game::addEvent( std::string s ) {
  // printf( "%s\n", s.c_str() );
  mEvents.push_back( s );
}

bool Game::isOutsideGameArea(const Vector &p) {
  return p.mX < 0 || p.mX > mConfig->getInt("width") || p.mY > mConfig->getInt("height") || p.mY < 0;
}

void Game::resetShip() {
  mShip.mAlive = true;
  bool flag = true;
  while (flag){
    mShip.mPos.mX = rand()%380 + 170;
    mShip.mPos.mY = rand()%330 + 150;
    if (mBighex.isInside(mShip.mPos) && !mSmallhex.isInside(mShip.mPos)){
      flag = false;
    } 
  }
  //mShip.mPos.mX = mConfig->getDouble("shipStartX");
  //mShip.mPos.mY = mConfig->getDouble("shipStartY");
  mShip.mVel.mX = mConfig->getDouble("shipStartVelX");
  mShip.mVel.mY = mConfig->getDouble("shipStartVelY");
  //mShip.mAngle = mConfig->getDouble("shipStartAngle");
  mShip.mAngle = rand()%360;
}

void Game::monitorShipRespawn() {
  if( !mShip.mAlive && mShip.mDeathTimer >= mConfig->getInt( "shipExplodeDuration" )) {
    resetShip();
    mFortress.mTimer = 0;
    addEvent( "ship-respawn" );
  }
}

void Game::fireShell( const Vector &p, double angle ) {
  for (int i=0; i<MAX_SHELLS; i++) {
    if (!mShells[i].mAlive) {
      mShells[i].mAlive = true;
      mShells[i].mCollisionRadius = mConfig->getInt("shellCollisionRadius");
      mShells[i].mPos.mX = p.mX;
      mShells[i].mPos.mY = p.mY;
      mShells[i].mAngle = angle;
      mShells[i].mVel.mX = mConfig->getInt("shellSpeed") * cos(deg2rad(angle));
      mShells[i].mVel.mY = mConfig->getInt("shellSpeed") * sin(deg2rad(angle));
      addEvent( "fortress-fired" );
      return;
    }
  }
}

void Game::fireMissile( const Vector &p, double angle ) {
  if( mShip.mAlive ) {
    for (int i=0; i<MAX_MISSILES; i++) {
      if (!mMissiles[i].mAlive) {
        mMissiles[i].mAlive = true;
        mMissiles[i].mCollisionRadius = mConfig->getInt("missileCollisionRadius");
        mMissiles[i].mPos.mX = p.mX;
        mMissiles[i].mPos.mY = p.mY;
        mMissiles[i].mAngle = angle;
        mMissiles[i].mVel.mX = mConfig->getInt("missileSpeed") * cos(deg2rad(angle));
        mMissiles[i].mVel.mY = mConfig->getInt("missileSpeed") * sin(deg2rad(angle));
        addEvent( "missile-fired" );
        penalize( mConfig->getDouble( "missilePenalty" ));
        return;
      }
    }
  }
}

void Game::updateFortress() {
  double dx = mShip.mPos.mX - mFortress.mPos.mX;
  double dy = mShip.mPos.mY - mFortress.mPos.mY;
  double angle_to_ship = stdAngle(rad2deg(atan2(dy,dx)));

  if (!mFortress.mAlive && mFortress.mDeathTimer > 1000) {
    mFortress.mTimer = 0;
    mFortress.mAlive = true;
    addEvent( "fortress-respawn");
  }

  if (mShip.mAlive) {
    mFortress.mAngle = stdAngle(ceil(angle_to_ship / mConfig->getInt("fortressSectorSize")) * mConfig->getInt("fortressSectorSize"));
    if (mFortress.mAngle != mFortress.mLastAngle) {
      mFortress.mLastAngle = mFortress.mAngle;
      mFortress.mTimer = 0;
    }
    if (mFortress.mTimer >= mConfig->getInt("fortressLockTime") && mShip.mAlive && mFortress.mAlive) {
      fireShell(mFortress.mPos, angle_to_ship);
      mFortress.mTimer = 0;
    }
  }
}

void Game::processKeyState() {
  // std::vector<std::string> keyNames = {"none", "fire", "thrust", "left", "right"};
  std::string keyNames[] = {"none", "fire", "thrust", "left", "right"};
  maybeResetKeyEvents();
  for(size_t i=0; i<mKeys.events.size(); i++) {
    addEvent(( mKeys.events[i].state ? "press-":"release-" ) + keyNames[mKeys.events[i].sym] );
    if (mKeys.events[i].state == true) {
      if (mKeys.events[i].sym == LEFT_KEY && !mShip.mLeftFlag) {
        mShip.mLeftFlag = true;
        mShip.mLeftTimer = 0;
        mStats.totalLefts += 1;
      } else if (mKeys.events[i].sym == RIGHT_KEY && !mShip.mRightFlag) {
        mShip.mRightFlag = true;
        mShip.mRightTimer = 0;
        mStats.totalRights += 1;
      } else if (mKeys.events[i].sym == THRUST_KEY && !mShip.mThrustFlag) {
        mShip.mThrustFlag = true;
        mShip.mThrustTimer = 0;
        mStats.totalThrusts += 1;
      } else if (mKeys.events[i].sym == FIRE_KEY && !mShip.mFireFlag) {
        fireMissile(mShip.mPos, mShip.mAngle);
        mShip.mFireFlag = true;
        if (mScore.mVulnerability > 10)
          mShotIntervalsVul.push_back( abs(mShip.mFireTimer) );
        else
          mShotIntervalsInvul.push_back( abs(mShip.mFireTimer) );
        mShip.mFireTimer = 0;
        mStats.totalShots += 1;
      }
    } else {
      if (mKeys.events[i].sym == LEFT_KEY && mShip.mLeftFlag) {
        mShip.mLeftFlag = false;
        mShip.mLeftTimer = 0;
      } else if (mKeys.events[i].sym == RIGHT_KEY && mShip.mRightFlag) {
        mShip.mRightFlag = false;
        mShip.mRightTimer = 0;
      } else if (mKeys.events[i].sym == THRUST_KEY && mShip.mThrustFlag) {
        mShip.mThrustFlag = false;
        mThrustDurations.push_back( mShip.mThrustTimer );
        mShip.mThrustTimer = 0;
      } else if (mKeys.events[i].sym == FIRE_KEY && mShip.mFireFlag) {
        mShip.mFireFlag = false;
        mShotDurations.push_back( mShip.mFireTimer );
        mShip.mFireTimer = 0;
      }
    }
  }
  if (mShip.mLeftFlag && !mShip.mRightFlag)
    mShip.mTurnFlag = TURN_LEFT;
  else if (!mShip.mLeftFlag && mShip.mRightFlag)
    mShip.mTurnFlag = TURN_RIGHT;
  else
    mShip.mTurnFlag = NO_TURN;
  mKeys.processed = true;
}

void Game::killShip() {
  if (mShip.mAlive) {
    mShip.mAlive = false;
    mShip.mDeathTimer = 0;
    mStats.shipDeaths += 1;
  }
}

static double normDist(double fdist, double bigHex, double smallHex) {
  return -1 + (fdist - smallHex) / ((bigHex - smallHex) / 2.0);
}

static double vdir(const Object &ship, const Object &fortress) {
  if (ship.mVel.norm() == 0.0) {
    return 0.0;
  }
  double o = atan2(-(fortress.mPos.mY-ship.mPos.mY),
                   fortress.mPos.mX-ship.mPos.mX);
  double v = atan2(ship.mVel.mY, ship.mVel.mX);
  double diff = v - o;
  if (diff > M_PI) diff -= M_PI*2;
  if (diff < -M_PI) diff += M_PI*2;
  return rad2deg(diff);
}

static double aim(const Object &ship, const Object &fortress) {
  double o = atan2((ship.mPos.mY-fortress.mPos.mY), (ship.mPos.mX-fortress.mPos.mX));
  o = rad2deg(o) - ship.mAngle + 180;
  if (o < -180)
    o = o + 360;
  return o;
}

void Game::computeExtra() {
  mExtra.vdir = vdir( mShip, mFortress );
  mExtra.aim = aim( mShip, mFortress );
  mExtra.fdist = sqrt(pow(mShip.mPos.mX - mFortress.mPos.mX, 2) + pow(mShip.mPos.mY - mShip.mPos.mY, 2));
  mExtra.ndist = normDist(mExtra.fdist, mBighex.mRadius, mSmallhex.mRadius);
}

void Game::updateShip() {
  if (mShip.mAlive) {
    double currentAngle = mShip.mAngle;
    double desiredAngle = stdAngle(ceil(angleTo(mShip.mPos, mFortress.mPos)));
    if (mConfig->getInt("autoTurn")) {
      mShip.mAngle = stdAngle(ceil(angleTo(mShip.mPos, mFortress.mPos)));
    } else {
      if (mShip.mTurnFlag == TURN_LEFT) {
        mShip.mAngle = stdAngle(mShip.mAngle - mConfig->getInt("shipTurnSpeed"));
      } else if (mShip.mTurnFlag == TURN_RIGHT) {
        mShip.mAngle = stdAngle(mShip.mAngle + mConfig->getInt("shipTurnSpeed"));
      }
    }
    if (mShip.mThrustFlag) {
      mShip.mVel.mX += mConfig->getDouble("shipAcceleration") * cos(deg2rad(mShip.mAngle));
      mShip.mVel.mY += mConfig->getDouble("shipAcceleration") * sin(deg2rad(mShip.mAngle));
    }
    // printf( "ship x: %f y: %f\n", mShip.mPos.mX, mShip.mPos.mY );
    mShip.mPos.mX += mShip.mVel.mX;
    mShip.mPos.mY += mShip.mVel.mY;

    computeExtra();

    if (!mBighex.isInside(mShip.mPos)) {
      killShip();
      penalize(mConfig->getInt("shipDeathPenalty"));
      mStats.bigHexDeaths += 1;
      mCollisions.bigHex = true;
      addEvent("explode-bighex");
    } else if (mSmallhex.isInside(mShip.mPos)) {
      killShip();
      penalize(mConfig->getInt("shipDeathPenalty"));
      mStats.smallHexDeaths += 1;
      mCollisions.smallHex = true;
      addEvent("explode-smallhex");
    }

    if (abs(mShip.mAngle-desiredAngle)<10 && abs(currentAngle-desiredAngle)>10) {
      reward(mConfig->getDouble("turningReward"));
    } else if (abs(mShip.mAngle-desiredAngle)>10 && abs(currentAngle-desiredAngle)<10) {
      //penalize(mConfig->getInt("turningReward"));
    }
  }
}

void Game::updateMissiles() {
  int i;
  for(i=0; i<MAX_MISSILES; i++) {
    if (mMissiles[i].mAlive) {
      mMissiles[i].mPos.mX += mMissiles[i].mVel.mX;
      mMissiles[i].mPos.mY += mMissiles[i].mVel.mY;
      if (mMissiles[i].collided(mFortress)) {
        mMissiles[i].mAlive = false;
        mCollisions.missileFortress = true;
        if (mFortress.mAlive) {
          addEvent("hit-fortress");
          if (mFortress.mVulnerabilityTimer >= mConfig->getInt("fortressVulnerabilityTime")) {
            playSound( VLNER_INCREASE_SOUND );
            mScore.mVulnerability += 1;
            // if (mScore.mVulnerability < 11)
            //   reward( mScore.mVulnerability * mScore.mVulnerability );
            // if (mScore.mVulnerability > 10)
            //   penalize( 2 );
            addEvent("vlner-increased");
            mStats.vlnerIncs += 1;
            if (mScore.mVulnerability > mStats.maxVlner)
              mStats.maxVlner = mScore.mVulnerability;
          } else {
            if (mScore.mVulnerability >= mConfig->getInt("fortressVulnerabilityThreshold") + 1) {
              playSound( VLNER_INCREASE_SOUND );
              mFortress.mAlive = false;
              mFortress.mDeathTimer = 0;
              reward( mConfig->getInt("destroyFortress") + mDestroyFortressExtraPoints );
              addEvent("fortress-destroyed");
              mStats.destroyedFortresses += 1;
            } else {
              playSound( VLNER_RESET_SOUND );
              addEvent("vlner-reset");
              // penalize( mScore.mVulnerability+1 );
              mStats.resets += 1;
            }
            mScore.mVulnerability = 0;
          }
          mFortress.mVulnerabilityTimer = 0;
        } else {
          addEvent( "hit-dead-fortress" );
        }
      } else if (isOutsideGameArea(mMissiles[i].mPos)) {
        mMissiles[i].mAlive = false;
        penalize( mConfig->getInt( "missPenalty" ));
        mStats.missedShots += 1;
      }
    }
  }
}

void Game::updateShells() {
  int i;
  for(i=0; i<MAX_SHELLS; i++) {
    if (mShells[i].mAlive) {
      mShells[i].mPos.mX += mShells[i].mVel.mX;
      mShells[i].mPos.mY += mShells[i].mVel.mY;
      if (mShip.mAlive && mShells[i].collided(mShip)) {
        mCollisions.shellShip = true;
        mShells[i].mAlive = false;
        killShip();
        // penalize(mConfig->getInt("shipDeathPenalty")/10);
        penalize(mConfig->getInt("shipDeathPenalty"));
        mStats.shellDeaths += 1;
        addEvent("shell-hit-ship");
      } else if (isOutsideGameArea(mShells[i].mPos)) {
        mShells[i].mAlive = false;
      }
    }
  }
}

void Game::stepTimers(int ms) {
  mTick += 1;
  mFortress.mTimer += ms;
  mFortress.mDeathTimer += ms;
  mFortress.mVulnerabilityTimer += ms;
  mShip.mDeathTimer += ms;

  if (mShip.mFireFlag)
    mShip.mFireTimer += 1;
  else
    mShip.mFireTimer -= 1;

  if (mShip.mThrustFlag)
    mShip.mThrustTimer += 1;
  else
    mShip.mThrustTimer -= 1;

  if (mShip.mLeftFlag)
    mShip.mLeftTimer += 1;
  else
    mShip.mLeftTimer -= 1;

  if (mShip.mRightFlag)
    mShip.mRightTimer += 1;
  else
    mShip.mRightTimer -= 1;
}

void Game::resetCollisions() {
  mCollisions.bigHex = false;
  mCollisions.smallHex = false;
  mCollisions.missileFortress = false;
  mCollisions.shellShip = false;
}

void Game::resetEvents() {
  mEvents.clear();
}

void Game::resetTick() {
  resetCollisions();
  resetEvents();
}

void Game::updateTime(int ms) {
  mTime += ms;
}

int Game::stepOneTick(int ms) {
  mReward = 0;
  updateTime(ms);
  resetTick();
  processKeyState();
  monitorShipRespawn();
  updateShip();
  updateFortress();
  updateShells();
  updateMissiles();
  stepTimers(ms);
  return mReward;
}

bool Game::isGameOver() {
  return mTime >= mConfig->getInt("gameTime");
}

void Game::maybeAdjustHexSizes(int &bigHex, int &smallHex) {
  if( mConfig->getInt( "staircase" )) {
    if( mStats.shipDeaths <= 1 ) {
      bigHex -= mConfig->getInt( "hexContraction" );
      smallHex += mConfig->getInt( "hexContraction" );
      // Don't make it too small.
      if (bigHex - smallHex < mConfig->getInt( "minHexDistance" )) {
        bigHex += mConfig->getInt( "hexContraction" );
        smallHex -= mConfig->getInt( "hexContraction" );
      }
    } else {
      bigHex += mConfig->getInt( "hexExpansion" );
      smallHex -= mConfig->getInt( "hexExpansion" );
      // don't exceed original size
      if (bigHex > mConfig->getInt( "bigHex" )) {
        bigHex = mConfig->getInt( "bigHex" );
      }
      if (smallHex < mConfig->getInt( "smallHex" )) {
        smallHex = mConfig->getInt( "smallHex" );
      }
    }
  }
}

void Game::calculateBonus() {
  mBonus = round( mScore.mPoints * mConfig->getInt( "maxBonus" ) / float( mConfig->getInt( "maxPoints" )));
}

std::string Game::dumpState() {
  bool first;
  std::stringstream out;
  out << std::fixed;
  out << "[" << mTime << ","
      << (mShip.mAlive?1:0) << ","
      << std::setprecision( 3 )
      << mShip.mPos.mX << ","
      << mShip.mPos.mY << ","
      << mShip.mVel.mX << ","
      << mShip.mVel.mY << ","
      << std::setprecision( 1 )
      << mShip.mAngle << ","
      << (mFortress.mAlive?1:0) << ","
      << mFortress.mAngle << ",";

  out << "[";;
  first = true;
  for (int i=0; i<MAX_MISSILES; i++) {
    if (mMissiles[i].mAlive) {
      out << (first?"":",")
          << std::setprecision( 3 )
          << mMissiles[i].mPos.mX << ","
          << mMissiles[i].mPos.mY << ","
          << std::setprecision( 1 )
          << mMissiles[i].mAngle;
      first = false;
    }
  }
  out << "],[";
  first = true;
  for (int i=0; i<MAX_SHELLS; i++) {
    if (mShells[i].mAlive) {
      out << (first?"":",")
          << std::setprecision( 3 )
          << mShells[i].mPos.mX << ","
          << mShells[i].mPos.mY << ","
          << std::setprecision( 1 )
          << mShells[i].mAngle;
      first = false;
    }
  }
  out << "],";
  out << mScore.mPoints << ","
      << mScore.mVulnerability << ","
      << (mShip.mThrustFlag?1:0) << ","
      << mShip.mTurnFlag << ",";
  /* Game Events */
  out << "[";
  for( size_t i=0; i<mEvents.size(); i++ ) {
    out << (i==0?"":",")
        << "\"" << mEvents[i] << "\"";
  }
  out << "]";
  out << "]";

  return out.str();
}
