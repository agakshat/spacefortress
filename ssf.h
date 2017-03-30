#ifndef __SSF_H__
#define __SSF_H__

#define MAX_EVENTS 50
#define MAX_KEY_EVENTS 50
#define MAX_MISSILES 50
#define MAX_SHELLS 50

typedef enum {
  FIRE_SHELL_SOUND,
  FIRE_MISSILE_SOUND,
  EXPLOSION_SOUND,
  VLNER_RESET_SOUND
} Sound;

typedef enum { NO_TURN, TURN_LEFT, TURN_RIGHT } Turn;

typedef enum {
  FIRE_KEY, THRUST_KEY, LEFT_KEY, RIGHT_KEY
} KeySym;

typedef struct {
  KeySym sym;
  bool state;
} Key;

typedef enum {
  MISSILE_FIRED_EVENT,
  FORTRESS_FIRED_EVENT,
  SHELL_HIT_SHIP_EVENT,
  HIT_FORTRESS_EVENT,
  VLNER_INCREASED_EVENT,
  VLNER_RESET_EVENT,
  FORTRESS_DESTROYED_EVENT,
  FORTRESS_RESPAWN_EVENT,
  EXPLODE_BIGHEX_EVENT,
  EXPLODE_SMALLHEX_EVENT
} Event;

typedef int Timer;

typedef struct {
  double x, y;
} Point;

typedef struct {
  Point position, velocity;
  double angle;
  Point startLocation, startVelocity;
  double startAngle;
  double collisionRadius;
  bool alive;
} Object;

typedef struct {
  Point points[6];
} Hexagon;

typedef struct {
  Object o;
  double acceleration, turnSpeed, maxVelocity;
  Timer deathTimer;
  bool thrustFlag;
  Turn turnFlag;
} Ship;

typedef struct {
  Object o;
  Timer deathTimer, vulnerabilityTimer, timer;
  double lastAngle;
} Fortress;

typedef struct {
  Object o;
} Missile;

typedef struct {
  Object o;
} Shell;

typedef struct {
  int points, rawPoints, vulnerability;
} Score;

typedef struct {
  int shipDeaths;
} Stats;

typedef struct {
  bool thrust, left, right, fire;
  Key events[MAX_KEY_EVENTS];
  int eventCount;
} Keys;

typedef struct {
  Event events[MAX_EVENTS];
  int count;
} Events;

typedef struct {
  bool bigHex, smallHex, missileFortress, shellShip;
} Collisions;

typedef struct {
  int width, height;
  int gameTime;
  /* Points */
  int destroyFortress, shipDeathPenalty, missilePenalty;
  struct {
    int shellSpeed, sectorSize, lockTime, vulnerabilityTime, vulnerabilityThreshold;
    int collisionRadius;
  } fortress;
  /* Hexagons */
  int bigHex, smallHex;
  struct {
    int explodeDuration, turnSpeed, collisionRadius;
    double acceleration;
    Point startPosition, startVelocity;
  } ship;
  /* Game Modes */
  bool autoTurn;
} Config;

typedef struct {
  Config config;
  Keys keys;
  Ship ship;
  Fortress fortress;
  Missile missiles[MAX_MISSILES];
  Shell shells[MAX_SHELLS];
  Hexagon bigHex, smallHex;
  Score score;
  Stats stats;

  int tick, time;
  Collisions collisions;
  Events events;
} Game;

void pressKey(Game *game, KeySym sym);
void releaseKey(Game *game, KeySym sym);
void StepOneTick(Game *game);

Game* makeAutoTurnGame();
Game* makeExplodeGame();

#endif
