#ifndef __SSF_H__
#define __SSF_H__

#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef M_PI
#define M_PI           3.14159265358979323846
#endif

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
  NO_KEY, FIRE_KEY, THRUST_KEY, LEFT_KEY, RIGHT_KEY
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
  SHIP_RESPAWN_EVENT,
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
  int collisionRadius;
  bool alive;
} Object;

typedef struct {
  Point points[6];
  int radius;
} Hexagon;

typedef struct {
  Object o;
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
  int eventCount;
  Key events[MAX_KEY_EVENTS];
  bool processed;
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
    int speed;
    int collisionRadius;
  } shell;
  struct {
    int sectorSize, lockTime, vulnerabilityTime, vulnerabilityThreshold;
    int collisionRadius;
  } fortress;
  /* Hexagons */
  int bigHex, smallHex;
  struct {
    int speed;
    int collisionRadius;
  } missile;
  struct {
    int explodeDuration, turnSpeed, collisionRadius;
    double acceleration;
    Point startPosition, startVelocity;
    int startAngle;
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
  FILE *logStream;
} Game;

#define MAX_WIREFRAME_POINTS 10
#define MAX_WIREFRAME_LINES 10

typedef struct {
  int from, to;
} WireFrameLine;

typedef struct {
  Point points[MAX_WIREFRAME_POINTS];
  int pointCount;
  WireFrameLine lines[MAX_WIREFRAME_LINES];
  int lineCount;
  int r, g, b;
} WireFrame;

extern WireFrame missileWireFrame, shellWireFrame, shipWireFrame, fortressWireFrame;

void pressKey(Game *game, KeySym sym);
void releaseKey(Game *game, KeySym sym);
void stepOneTick(Game *game, int ms);
bool isGameOver(Game *game);

Game* makeAutoTurnGame();
Game* makeExplodeGame();
void freeGame(Game *game);

void dumpSexpGameState(Game *game, char *buf, size_t size);

bool openLog(Game *game, char *path);
void closeLog(Game *game);
bool logGameState(Game *game);

#endif
