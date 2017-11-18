#include "space-fortress.hh"

const int MAX_MISSILES = 20;
const int MAX_SHELLS = 20;

typedef enum {
  VLNER_INCREASE_SOUND,
  VLNER_RESET_SOUND
} Sound;

typedef int Timer;

typedef enum { NO_TURN, TURN_LEFT, TURN_RIGHT } Turn;

typedef enum {
  NO_KEY, FIRE_KEY, THRUST_KEY, LEFT_KEY, RIGHT_KEY
} KeySym;

typedef struct {
  KeySym sym;
  bool state;
} Key;

typedef struct {
  std::vector<Key> events;
  bool processed;
} Keys;

typedef struct {
  int bigHexDeaths;
  int smallHexDeaths;
  int shellDeaths;
  int shipDeaths;
  int resets;
  int destroyedFortresses;
  int missedShots;
  int totalShots;
  int totalThrusts;
  int totalLefts;
  int totalRights;
  int vlnerIncs;
  int maxVlner;
} Stats;

typedef struct {
  bool bigHex, smallHex, missileFortress, shellShip;
} Collisions;

typedef struct {
  float mPoints, mRawPoints;
  int mVulnerability;
} GameScore;

typedef struct {
  double vdir, fdist, ndist, aim;
} ExtraGameValues;

class Ship: public Object {
public:
  Timer mDeathTimer;
  Timer mFireTimer;
  Timer mThrustTimer;
  Timer mLeftTimer;
  Timer mRightTimer;
  bool mThrustFlag;
  bool mFireFlag;
  bool mLeftFlag;
  bool mRightFlag;
  Turn mTurnFlag;

  Ship();
  ~Ship();
};

class Fortress: public Object {
public:
  Timer mDeathTimer, mVulnerabilityTimer, mTimer;
  double mLastAngle;

  Fortress();
  ~Fortress();
};

class Game {
public:
  Config *mConfig;
  Keys mKeys;
  Ship mShip;
  Fortress mFortress;
  Object mMissiles[MAX_MISSILES];
  Object mShells[MAX_MISSILES];
  Hexagon mBighex, mSmallhex;
  int mTick, mTime;
  Collisions mCollisions;
  Stats mStats;
  ExtraGameValues mExtra;
  std::vector<std::string> mEvents;
  std::vector<int> mThrustDurations;
  std::vector<int> mShotDurations;
  std::vector<int> mShotIntervalsInvul;
  std::vector<int> mShotIntervalsVul;

public:
  GameScore mScore;
  int mBonus;
  int mDestroyFortressExtraPoints;
  float mReward;

  Game( Config *config );
  virtual ~Game();

  virtual void playSound( Sound s );

  void reward(float amt);
  void penalize(float amt);
  void pressKey(KeySym sym);
  void releaseKey(KeySym sym);
  void addEvent( std::string s );
  void maybeResetKeyEvents();

  bool isOutsideGameArea(const Vector &p);
  void resetShip();
  void monitorShipRespawn();
  void fireShell( const Vector &p, double angle );
  void fireMissile( const Vector &p, double angle );
  void updateFortress();
  void processKeyState();
  void killShip();
  void computeExtra();
  void updateShip();
  void updateMissiles();
  void updateShells();
  void stepTimers(int ms);
  void resetCollisions();
  void resetEvents();
  void resetTick();
  void updateTime(int ms);
  int stepOneTick(int ms);
  bool isGameOver();
  void maybeAdjustHexSizes(int &bigHex, int &smallHex);
  void calculateBonus();
  std::string dumpState();
};
