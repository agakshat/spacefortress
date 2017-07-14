#include "ssf.h"

#define BUFSIZE 8192

WireFrame crap = { .points[0] = {0,0},
		   .r = 255 };

WireFrame missileWireFrame = { .points[0] = {0,0},
                                           .points[1] = { -25, 0 },
                                           .points[2] = { -5, 5 },
                                           .points[3] = { -5, -5 },
                                           .pointCount = 4,
                                           .lines[0] = { 0, 1 },
                                           .lines[1] = { 0, 2 },
                                           .lines[2] = { 0, 3 },
                                           .lineCount = 3,
                                           .r = 255,
                                           .g = 255,
                                           .b = 255 };

WireFrame shellWireFrame =  { .points[0] = { -8, 0 },
                                         .points[1] = { 0, -6 },
                                         .points[2] = { 16, 0 },
                                         .points[3] = { 0, 6 },
                                         .points[4] = { -8, 0 },
                                         .pointCount = 5,
                                         .lines[0] = { 0, 1 },
                                         .lines[1] = { 1, 2 },
                                         .lines[2] = { 2, 3 },
                                         .lines[3] = { 3, 0 },
                                         .lineCount = 4,
                                         .r = 255,
                                         .g = 0,
                                         .b = 0 };

WireFrame shipWireFrame =  { .points[0] = { -18, 0 },
                                        .points[1] = { 18, 0 },
                                        .points[2] = { 0, 0 },
                                        .points[3] = { -18, 18 },
                                        .points[4] = { -18, -18 },
                                        .pointCount = 5,
                                        .lines[0] = { 0, 1 },
                                        .lines[1] = { 3, 2 },
                                        .lines[2] = { 2, 4 },
                                        .lineCount = 3,
                                        .r = 255,
                                        .g = 255,
                                        .b = 0 };

WireFrame fortressWireFrame =  { .points[0] = { 0, 0 },
                                            .points[1] = { 36, 0 },
                                            .points[2] = { 18, -18 },
                                            .points[3] = { 0, -18 },
                                            .points[4] = { 18, 18 },
                                            .points[5] = { 0, 18 },
                                            .pointCount = 6,
                                            .lines[0] = { 0, 1 },
                                            .lines[1] = { 3, 2 },
                                            .lines[2] = { 2, 4 },
                                            .lines[3] = { 4, 5 },
                                            .lineCount = 4,
                                            .r = 255,
                                            .g = 255,
                                            .b = 0 };

void initWireframes () {
  missileWireFrame.points[0] = (Point){ 0, 0 };
  missileWireFrame.points[1] = (Point){ -25, 0 };
  missileWireFrame.points[2] = (Point){ -5, 5 };
  missileWireFrame.points[3] = (Point){ -5, -5 };
  missileWireFrame.pointCount = 4;
  missileWireFrame.lines[0] = (WireFrameLine){ 0, 1 };
  missileWireFrame.lines[1] = (WireFrameLine){ 0, 2 };
  missileWireFrame.lines[2] = (WireFrameLine){ 0, 3 };
  missileWireFrame.lineCount = 3;
  missileWireFrame.r = 255;
  missileWireFrame.g = 255;
  missileWireFrame.b = 255;

  shellWireFrame.points[0] = (Point){ -8, 0 };
  shellWireFrame.points[1] = (Point){ 0, -6 };
  shellWireFrame.points[2] = (Point){ 16, 0 };
  shellWireFrame.points[3] = (Point){ 0, 6 };
  shellWireFrame.points[4] = (Point){ -8, 0 };
  shellWireFrame.pointCount = 5;
  shellWireFrame.lines[0] = (WireFrameLine){ 0, 1 };
  shellWireFrame.lines[1] = (WireFrameLine){ 1, 2 };
  shellWireFrame.lines[2] = (WireFrameLine){ 2, 3 };
  shellWireFrame.lines[3] = (WireFrameLine){ 3, 0 };
  shellWireFrame.lineCount = 4;
  shellWireFrame.r = 255;
  shellWireFrame.g = 0;
  shellWireFrame.b = 0;

  shipWireFrame.points[0] = (Point){ -18, 0 };
  shipWireFrame.points[1] = (Point){ 18, 0 };
  shipWireFrame.points[2] = (Point){ 0, 0 };
  shipWireFrame.points[3] = (Point){ -18, 18 };
  shipWireFrame.points[4] = (Point){ -18, -18 };
  shipWireFrame.pointCount = 5;
  shipWireFrame.lines[0] = (WireFrameLine){ 0, 1 };
  shipWireFrame.lines[1] = (WireFrameLine){ 3, 2 };
  shipWireFrame.lines[2] = (WireFrameLine){ 2, 4 };
  shipWireFrame.lineCount = 3;
  shipWireFrame.r = 255;
  shipWireFrame.g = 255;
  shipWireFrame.b = 0;

  fortressWireFrame.points[0] = (Point){ 0, 0 };
  fortressWireFrame.points[1] = (Point){ 36, 0 };
  fortressWireFrame.points[2] = (Point){ 18, -18 };
  fortressWireFrame.points[3] = (Point){ 0, -18 };
  fortressWireFrame.points[4] = (Point){ 18, 18 };
  fortressWireFrame.points[5] = (Point){ 0, 18 };
  fortressWireFrame.pointCount = 6;
  fortressWireFrame.lines[0] = (WireFrameLine){ 0, 1 };
  fortressWireFrame.lines[1] = (WireFrameLine){ 3, 2 };
  fortressWireFrame.lines[2] = (WireFrameLine){ 2, 4 };
  fortressWireFrame.lines[2] = (WireFrameLine){ 4, 5 };
  fortressWireFrame.lineCount = 4;
  fortressWireFrame.r = 255;
  fortressWireFrame.g = 255;
  fortressWireFrame.b = 0;
}

double deg(double rad) {
  return rad / M_PI * 180;
}

double rad(double d) {
  return d * M_PI / 180;
}

double norm(double x, double y) {
  return sqrt(x*x + y*y);
}

double stdAngle(double a) {
  if (a <= -360 || a >= 360) a = fmod(a, 360);
  if (a < 0) a += 360;
  return a;
}

double angleTo(const Point *p1, const Point *p2) {
  double a = atan2(-(p2->y-p1->y), p2->x-p1->x);
  if (a < 0) a += M_PI*2;
  return deg(a);
}

int triangularNumber(int n) {
  return n * (n + 1) / 2;
}

void initHexagon(Hexagon *h, int radius) {
  double x1 = floor(210-radius);
  double x2 = floor(210-radius*0.5);
  double x3 = floor(210+radius*0.5);
  double x4 = floor(210+radius);
  double y1 = 210;
  double y2 = floor(210-radius*sin(M_PI*2/3));
  double y3 = floor(210+radius*sin(M_PI*2/3));
  h->points[0].x = x1;
  h->points[0].y = y1;
  h->points[1].x = x2;
  h->points[1].y = y2;
  h->points[2].x = x3;
  h->points[2].y = y2;
  h->points[3].x = x4;
  h->points[3].y = y1;
  h->points[4].x = x3;
  h->points[4].y = y3;
  h->points[5].x = x2;
  h->points[5].y = y3;
  h->radius = radius;
}

bool insideHexagon(const Hexagon *h, const Point *p) {
    int i;
    /* printf("hex: "); */
    /* for (i=0; i<6; i++) { printf(" %0.f,%0.f", h->points[i].x, h->points[i].y); } */
    /* printf("\n"); */

    for (i=0; i<6; i++) {
        double nx, ny, dx, dy;
        nx = -(h->points[(i+1)%6].y - h->points[i].y);
        ny =   h->points[(i+1)%6].x - h->points[i].x;
        dx = p->x - h->points[i].x;
        dy = p->y - h->points[i].y;
        if (nx * dx + ny * dy < 0) {
            return false;
        }
    }
    return true;
}

bool collided(const Object *e1, const Object *e2) {
  double d = sqrt(pow(e1->position.x - e2->position.x, 2) + pow(e1->position.y - e2->position.y, 2));
  return d <= e1->collisionRadius + e2->collisionRadius;
}

void maybeResetKeyEvents(Game *game) {
  if( game->keys.processed == true ) {
    game->keys.eventCount = 0;
    game->keys.processed = false;
  }
}

void pressKey(Game *game, KeySym sym) {
  maybeResetKeyEvents(game);
  if (game->keys.eventCount < MAX_KEY_EVENTS) {
    game->keys.events[game->keys.eventCount].sym = sym;
    game->keys.events[game->keys.eventCount].state = true;
    game->keys.eventCount += 1;
  }
}

void releaseKey(Game *game, KeySym sym) {
  maybeResetKeyEvents(game);
  if (game->keys.eventCount < MAX_KEY_EVENTS) {
    game->keys.events[game->keys.eventCount].sym = sym;
    game->keys.events[game->keys.eventCount].state = false;
    game->keys.eventCount += 1;
  }
}

void addEvent(Game *game, Event e) {
  game->events.events[game->events.count] = e;
  game->events.count += 1;
}

void playSound(Sound s) {
}

void reward(Game *game, int amount) {
  game->reward += amount;
  game->score.rawPoints += amount;
  game->score.points += amount;
  if (game->score.points < 0) game->score.points = 0;
}

void penalize(Game *game, int amount) {
  reward(game, -amount);
}

int isOutsideGameArea(Game *game, Point *p) {
  return p->x < 0 || p->x > game->config.width || p->y > game->config.height || p->y < 0;
}

void resetShip (Game *game) {
  game->ship.o.alive = true;
  game->ship.o.position.x = game->config.ship.startPosition.x;
  game->ship.o.position.y = game->config.ship.startPosition.y;
  game->ship.o.velocity.x = game->config.ship.startVelocity.x;
  game->ship.o.velocity.y = game->config.ship.startVelocity.y;
  game->ship.o.angle = game->config.ship.startAngle;
  game->ship.thrustFlag = false;
  game->ship.turnFlag == NO_TURN;
}

void monitorShipRespawn(Game *game) {
  if (!game->ship.o.alive && game->ship.deathTimer >= game->config.ship.explodeDuration) {
    resetShip(game);
    game->fortress.timer = 0;
    addEvent(game, SHIP_RESPAWN_EVENT);
  }
}

void fireShell(Game *game, double x, double y, double angle) {
  int i;
  for (i=0; i<MAX_SHELLS; i++) {
    if (!game->shells[i].o.alive) {
      game->shells[i].o.alive = true;
      game->missiles[i].o.collisionRadius = game->config.shell.collisionRadius;
      game->shells[i].o.position.x = x;
      game->shells[i].o.position.y = y;
      game->shells[i].o.angle = angle;
      game->shells[i].o.velocity.x = game->config.shell.speed * cos(rad(angle));
      game->shells[i].o.velocity.y = -game->config.shell.speed * sin(rad(angle));
      playSound(FIRE_SHELL_SOUND);
      addEvent(game, FORTRESS_FIRED_EVENT);
      return;
    }
  }
}

void fireMissile(Game *game) {
  if (game->ship.o.alive) {
    int i;
    for (i=0; i<MAX_MISSILES; i++) {
      if (!game->missiles[i].o.alive) {
        game->missiles[i].o.alive = true;
        game->missiles[i].o.collisionRadius = game->config.missile.collisionRadius;
        game->missiles[i].o.position.x = game->ship.o.position.x;
        game->missiles[i].o.position.y = game->ship.o.position.y;
        game->missiles[i].o.angle = game->ship.o.angle;
        game->missiles[i].o.velocity.x = game->config.missile.speed * cos(rad(game->ship.o.angle));
        game->missiles[i].o.velocity.y = -game->config.missile.speed * sin(rad(game->ship.o.angle));

        // penalize(game, game->config.missilePenalty);
        playSound(FIRE_MISSILE_SOUND);
        addEvent(game, MISSILE_FIRED_EVENT);
        return;
      }
    }

  }
}

void updateFortress(Game *game) {
  double dx = game->ship.o.position.x - game->fortress.o.position.x;
  double dy = -(game->ship.o.position.y - game->fortress.o.position.y);
  double angle_to_ship = stdAngle(deg(atan2(dy,dx)));

  if (!game->fortress.o.alive && game->fortress.deathTimer > 1000) {
    game->fortress.timer = 0;
    game->fortress.o.alive = true;
    addEvent(game, FORTRESS_RESPAWN_EVENT);
  }

  if (game->ship.o.alive) {
    game->fortress.o.angle = stdAngle(floor(angle_to_ship / game->config.fortress.sectorSize) * game->config.fortress.sectorSize);
    if (game->fortress.o.angle != game->fortress.lastAngle) {
      game->fortress.lastAngle = game->fortress.o.angle;
      game->fortress.timer = 0;
    }
    if (game->fortress.timer >= game->config.fortress.lockTime && game->ship.o.alive && game->fortress.o.alive) {
      fireShell(game, game->fortress.o.position.x, game->fortress.o.position.y, angle_to_ship);
      game->fortress.timer = 0;
    }
  }
}

void processKeyState (Game *game) {
  int i;
  maybeResetKeyEvents(game);
  bool newTurn = false;
  for(i=0; i<game->keys.eventCount; i++) {
    if (game->keys.events[i].state == true) {
      if (game->keys.events[i].sym == LEFT_KEY) {
        if (game->ship.turnFlag == NO_TURN) newTurn = true;
        game->ship.turnFlag = TURN_LEFT;
        game->keys.left = true;
      } else if (game->keys.events[i].sym == RIGHT_KEY) {
        if (game->ship.turnFlag == NO_TURN) newTurn = true;
        game->ship.turnFlag = TURN_RIGHT;
        game->keys.right = true;
      } else if (game->keys.events[i].sym == THRUST_KEY) {
        game->ship.thrustFlag = true;
        game->keys.thrust = true;
      } else if (game->keys.events[i].sym == FIRE_KEY) {
        game->keys.fire = true;
        fireMissile(game);
      }
    } else {
      if (!newTurn && (game->keys.events[i].sym == LEFT_KEY || game->keys.events[i].sym == RIGHT_KEY)) {
        if (game->keys.events[i].sym == LEFT_KEY)
          game->keys.left = false;
        else
          game->keys.right = false;
        game->ship.turnFlag = NO_TURN;
      } else if (game->keys.events[i].sym == THRUST_KEY) {
        game->ship.thrustFlag = false;
        game->keys.thrust = false;
      } else if (game->keys.events[i].sym == FIRE_KEY) {
        game->keys.fire = false;
      }
    }
  }
  game->keys.processed = true;
}

void killShip(Game *game) {
  if (game->ship.o.alive) {
    penalize(game, game->config.shipDeathPenalty);
    game->ship.o.alive = false;
    game->stats.shipDeaths += 1;
    game->ship.deathTimer = 0;
    playSound(EXPLOSION_SOUND);
  }
}

void updateShip(Game *game) {
  if (game->ship.o.alive) {
    if (game->config.autoTurn) {
      game->ship.o.angle = floor(angleTo(&game->ship.o.position, &game->fortress.o.position));
    } else {
      if (game->ship.turnFlag == TURN_LEFT) {
        game->ship.o.angle = stdAngle(game->ship.o.angle + game->config.ship.turnSpeed);
      } else if (game->ship.turnFlag == TURN_RIGHT) {
        game->ship.o.angle = stdAngle(game->ship.o.angle - game->config.ship.turnSpeed);
      }
    }
    if (game->ship.thrustFlag) {
      game->ship.o.velocity.x += game->config.ship.acceleration * cos(rad(game->ship.o.angle));
      game->ship.o.velocity.y += game->config.ship.acceleration * sin(rad(game->ship.o.angle));
    }
    game->ship.o.position.x += game->ship.o.velocity.x;
    game->ship.o.position.y -= game->ship.o.velocity.y;
    game->ship.vdir = vdir(&game->ship.o, &game->fortress.o);
    game->ship.speed = norm(game->ship.o.velocity.x, game->ship.o.velocity.y);
    game->ship.fdist = norm(game->ship.o.position.x-game->fortress.o.position.x, game->ship.o.position.y-game->fortress.o.position.y);
    game->ship.ndist = normDist(game->ship.fdist, game->config.bigHex, game->config.smallHex);

    if (!insideHexagon(&game->bigHex, &game->ship.o.position)) {
      killShip(game);
      game->collisions.bigHex = true;
      addEvent(game, EXPLODE_BIGHEX_EVENT);
    } else if (insideHexagon(&game->smallHex, &game->ship.o.position)) {
      killShip(game);
      game->collisions.smallHex = true;
      addEvent(game, EXPLODE_SMALLHEX_EVENT);
    }
  }
}

void updateMissiles(Game *game) {
  int i;
  for(i=0; i<MAX_MISSILES; i++) {
    if (game->missiles[i].o.alive) {
      game->missiles[i].o.position.x += game->missiles[i].o.velocity.x;
      game->missiles[i].o.position.y += game->missiles[i].o.velocity.y;
      if (collided(&game->missiles[i].o, &game->fortress.o)) {
        game->missiles[i].o.alive = false;
        game->collisions.missileFortress = true;
        addEvent(game, HIT_FORTRESS_EVENT);
        if (game->fortress.o.alive) {
          if (game->fortress.vulnerabilityTimer >= game->config.fortress.vulnerabilityTime) {
            game->score.vulnerability += 1;
            if (game->score.vulnerability < 11)
              reward(game, triangularNumber(game->score.vulnerability));
            addEvent(game, VLNER_INCREASED_EVENT);
          } else {
            if (game->score.vulnerability >= game->config.fortress.vulnerabilityThreshold + 1) {
              game->fortress.o.alive = false;
              game->fortress.deathTimer = 0;
              reward(game, game->config.destroyFortress);
              playSound(EXPLOSION_SOUND);
              addEvent(game, FORTRESS_DESTROYED_EVENT);
            } else {
              penalize(game, game->score.vulnerability);
              playSound(VLNER_RESET_SOUND);
              addEvent(game, VLNER_RESET_EVENT);
            }
            game->score.vulnerability = 0;
          }
          game->fortress.vulnerabilityTimer = 0;
        }
      } else if (isOutsideGameArea(game, &game->missiles[i].o.position)) {
        penalize(game, game->config.missilePenalty);
        game->missiles[i].o.alive = false;
      }
    }
  }
}

void updateShells(Game *game) {
  int i;
  for(i=0; i<MAX_SHELLS; i++) {
    if (game->shells[i].o.alive) {
      game->shells[i].o.position.x += game->shells[i].o.velocity.x;
      game->shells[i].o.position.y += game->shells[i].o.velocity.y;
      if (game->ship.o.alive && collided(&game->shells[i].o, &game->ship.o)) {
        game->collisions.shellShip = true;
        game->shells[i].o.alive = false;
        killShip(game);
        addEvent(game, SHELL_HIT_SHIP_EVENT);
      } else if (isOutsideGameArea(game, &game->shells[i].o.position)) {
        game->shells[i].o.alive = false;
      }
    }
  }
}

void stepTimers(Game *game, int ms) {
  game->tick += 1;
  game->fortress.timer += ms;
  game->fortress.deathTimer += ms;
  game->fortress.vulnerabilityTimer += ms;
  game->ship.deathTimer += ms;
}

void resetCollisions(Game *game) {
  game->collisions.bigHex = false;
  game->collisions.smallHex = false;
  game->collisions.missileFortress = false;
  game->collisions.shellShip = false;
}

void resetEvents(Game *game) {
  game->events.count = 0;
}

void resetTick(Game *game) {
  resetCollisions(game);
  resetEvents(game);
}

void updateTime(Game *game, int ms) {
  game->time += ms;
}

void stepOneTick(Game *game, int ms) {
  game->reward = 0;
  updateTime(game, ms);
  resetTick(game);
  processKeyState(game);
  monitorShipRespawn(game);
  updateShip(game);
  updateFortress(game);
  updateShells(game);
  updateMissiles(game);
  stepTimers(game, ms);
}

bool isGameOver(Game *game) {
  return game->time >= game->config.gameTime;
}

void baseConfig(Game *game, bool autoturn, bool grayscale) {
  Config *config = &game->config;
  config->grayscale = grayscale;
  config->width = 420;
  config->height = 420;
  config->gameTime = 180000;
  /* Points */
  config->destroyFortress = 1000;
  config->shipDeathPenalty = 100;
  config->missilePenalty = 2;
  // config->hitReward = 1;
  /* Projectiles */
  config->shell.collisionRadius = 3;
  config->shell.speed = 6;
  config->missile.collisionRadius = 5;
  config->missile.speed = 20;
  /* Fortress */
  config->fortress.sectorSize = 10;
  config->fortress.lockTime = 1000;
  config->fortress.vulnerabilityTime = 250;
  config->fortress.vulnerabilityThreshold = 10;
  config->fortress.collisionRadius = 18;
  /* Hexagons */
  config->bigHex = 200;
  config->smallHex = 40;
  /* ship */
  config->ship.explodeDuration = 1000;
  config->ship.turnSpeed = 6;
  config->ship.acceleration = 0.3;
  config->ship.collisionRadius = 10;
  config->ship.startPosition.x = 110;
  config->ship.startPosition.y = 210;
  config->ship.startVelocity.x = cos(rad(60));
  config->ship.startVelocity.y = sin(rad(60));
  config->ship.startAngle = 0;
  config->ship.vdir = vdir(&game->ship.o, &game->fortress.o);
  config->ship.speed = norm(game->ship.o.velocity.x, game->ship.o.velocity.y);
  config->ship.fdist = norm(game->ship.o.position.x-game->fortress.o.position.x, game->ship.o.position.y-game->fortress.o.position.y);
  config->ship.ndist = normDist(game->ship.fdist, game->config.bigHex, game->config.smallHex);
  /* Game Modes */
  config->autoTurn = autoturn;
}

void autoTurnConfig(Game *game, bool grayscale) {
  baseConfig(game, true, grayscale);
}

void explodeConfig(Game *game, bool grayscale) {
  baseConfig(game, false, grayscale);
}

void initGame(Game *game) {
  int i;

  game->keys.thrust = false;
  game->keys.left = false;
  game->keys.right = false;
  game->keys.fire = false;
  game->keys.eventCount = 0;
  game->keys.processed = false;

  game->ship.o.collisionRadius = game->config.ship.collisionRadius;
  resetShip(game);

  game->fortress.o.alive = true;
  game->fortress.o.collisionRadius = game->config.fortress.collisionRadius;
  game->fortress.o.position.x = 210;
  game->fortress.o.position.y = 210;
  game->fortress.o.angle = 180;
  game->fortress.lastAngle = 0;

  for (i=0; i<MAX_MISSILES; i++) { game->missiles[i].o.alive = false; }
  for (i=0; i<MAX_SHELLS; i++) { game->shells[i].o.alive = false; }

  initHexagon(&game->bigHex, game->config.bigHex);
  initHexagon(&game->smallHex, game->config.smallHex);

  game->reward = 0;

  game->grayscale = game->config.grayscale;

  game->score.points = 0;
  game->score.rawPoints = 0;
  game->score.vulnerability = 0;
  game->stats.shipDeaths = 0;

  game->tick = 0;
  game->time = 0;
  resetTick(game);
}

Game* makeAutoTurnGame(bool grayscale) {
  Game *g = malloc(sizeof(Game));
  memset((void *)g, 0, sizeof(Game));
  if (g != NULL) {
    autoTurnConfig(g, grayscale);
    initGame(g);
  }
  return g;
}

Game* makeExplodeGame(bool grayscale) {
  Game *g = malloc(sizeof(Game));
  memset((void *)g, 0, sizeof(Game));
  if (g != NULL) {
    explodeConfig(g, grayscale);
    initGame(g);
  }
  return g;
}

void freeGame(Game *game) {
  if (game->logStream) {
    closeLog(game);
  }
  free(game);
}

int dumpProjectile(const Object *o, char *buf, size_t size) {
  int n = 0;
  n = snprintf(buf, size, "(:x %.3f :y %.3f :vx %.3f :vy %.3f :orientation %.1f)",
               o->position.x,
               o->position.y,
               o->velocity.x,
               o->velocity.y,
               o->angle);
  return n;
}

double normDist(double fdist, double bigHex, double smallHex) {
  return -1 + (fdist - smallHex) / ((bigHex - smallHex) / 2.0);
}

double vdir(const Object *ship, const Object *fortress) {
  if (norm(ship->velocity.x, ship->velocity.y) == 0.0) {
    return 0.0;
  }
  double o = atan2(-(fortress->position.y-ship->position.y),
                   fortress->position.x-ship->position.x);
  double v = atan2(ship->velocity.y, ship->velocity.x);
  double diff = v - o;
  if (diff > M_PI) diff -= M_PI*2;
  if (diff < -M_PI) diff += M_PI*2;
  return deg(diff);
}

void dumpSexpGameState(Game *game, char *buf, size_t size) {
  char missiles[BUFSIZE];
  char shells[BUFSIZE];
  int n, i;

  n = 0;
  missiles[0] = 0;
  for (i=0; i<MAX_MISSILES; i++) {
    if (game->missiles[i].o.alive) {
      n += dumpProjectile(&game->missiles[i].o, missiles, BUFSIZE-n);
      if (n >= BUFSIZE-1) break;
    }
  }
  n = 0;
  shells[0] = 0;
  for (i=0; i<MAX_SHELLS; i++) {
    if (game->shells[i].o.alive) {
      n += dumpProjectile(&game->shells[i].o, missiles, BUFSIZE-n);
      if (n >= BUFSIZE-1) break;
    }
  }

  n = snprintf(buf, size,
               "(:screen-type \"game\""
               " :mode \"events\""
               " :game %s"
               " :time %d"
               " :missiles (%s)"
               " :shells (%s)"
               " :ship (:alive %s :x %.3f :y %.3f :vx %.3f :vy %.3f :orientation %.1f :distance-from-fortress %.3f :vdir %.3f :speed %.3f)"
               " :fortress (:alive %s :x %.3f :y %.3f :orientation %.1f)"
               " :bighex %d"
               " :smallhex %d"
               " :bonus 0"
               " :pnts %d"
               " :rawpnts %d"
               " :vlner %d"
               " :collisions %s"
               " :active t"
               " :events %s"
               " :keys nil"
               ")",
               game->config.autoTurn ? "\"autoturn\"" : "\"explode\"",
               game->time,
               missiles,
               shells,
               game->ship.o.alive ? "t":"nil",
               game->ship.o.position.x,
               game->ship.o.position.y,
               game->ship.o.velocity.x,
               game->ship.o.velocity.y,
               game->ship.o.angle,
               game->ship.fdist,
               game->ship.vdir,
               game->ship.speed,
               game->fortress.o.alive ? "t":"nil",
               game->fortress.o.position.x,
               game->fortress.o.position.y,
               game->fortress.o.angle,
               game->config.bigHex,
               game->config.smallHex,
               game->score.points,
               game->score.rawPoints,
               game->score.vulnerability,
               "nil", // collisions
               "nil"); // events
}

void saveClassicGameState(Game *game) {
  fprintf(game->logStream, "%d %.6f %s %.3f %.3f %.3f %.3f %.1f n - - %s %.1f %s %s 0 %d 0 0 %d 0 0 0 0 %s %s %s %s n n y\n",
          game->time,
          0.0,
          game->ship.o.alive ? "y":"n",
          game->ship.o.position.x,
          game->ship.o.position.y,
          game->ship.o.velocity.x,
          game->ship.o.velocity.y,
          game->ship.o.angle,
          /* mine alive */
          /* mine x */
          /* mine y */
          game->fortress.o.alive ? "y":"n",
          game->fortress.o.angle,
          "[]",
          "[]",
          /* bonus */
          game->score.points,
          /* cntrl */
          /* vlcty */
          game->score.vulnerability,
          /* iff */
          /* intrvl */
          /* speed */
          /* shots */
          game->ship.thrustFlag ? "y":"n",
          game->ship.turnFlag == TURN_LEFT ? "y":"n",
          game->ship.turnFlag == TURN_RIGHT ? "y":"n",
          "n"
          /* iff key */
          /* shots key */
          /* pnts key */
          /* game active */
          );
}

bool logGameStateHeaders(Game *game) {
  if (game->logStream) {
    fprintf(game->logStream, "{");
    /* FIXME: All config variables should be recorded. But how to do that programmatically? */
    fprintf(game->logStream, "\"version\":%s,", GVERSION);
    fprintf(game->logStream, "\"sha1\":%s,", GGITSHA1);
    fprintf(game->logStream, "\"autoTurn\":%d,", game->config.autoTurn);
    fprintf(game->logStream, "\"bigHex\":%d,", game->config.bigHex);
    fprintf(game->logStream, "\"smallHex\":%d,", game->config.smallHex);
    fprintf(game->logStream, "\"headers\":[\"game_time\",\"ship_alive\",\"ship_x\",\"ship_y\",\"ship_vel_x\",\"ship_vel_y\",\"ship_angle\",\"fortress_alive\",\"fortress_angle\",\"missiles\",\"shells\",\"points\",\"vulnerability\",\"thrusting\",\"turning_left\",\"turning_right\",\"game_events\",\"input_events\"]");
    fprintf(game->logStream, "}\n");
    fflush(game->logStream);
    return true;
  }
  return false;
}

bool logGameStateFooters(Game *game) {
  if (game->logStream) {
    fprintf(game->logStream, "{");
    fprintf(game->logStream, "\"points\":%d,", game->score.points);
    fprintf(game->logStream, "\"rawPoints\":%d", game->score.rawPoints);
    fprintf(game->logStream, "}\n");
    fflush(game->logStream);
    return true;
  }
  return false;
}

bool openLog(Game *game, char *path) {
  game->logStream = fopen(path, "a");
  if (game->logStream == NULL) {
    return false;
  }
  logGameStateHeaders( game );
  return true;
}

void closeLog(Game *game) {
  if (game->logStream) {
    logGameStateFooters( game );
    fclose(game->logStream);
    game->logStream = NULL;
  }
}

bool logGameState(Game *game) {
  int i;
  if (game->logStream) {
    fprintf(game->logStream, "[%d,%d,%.3f,%.3f,%.3f,%.3f,%.1f,%d,%.1f,",
            game->time,
            game->ship.o.alive?1:0,
            game->ship.o.position.x,
            game->ship.o.position.y,
            game->ship.o.velocity.x,
            game->ship.o.velocity.y,
            game->ship.o.angle,
            game->fortress.o.alive?1:0,
            game->fortress.o.angle);

    fprintf(game->logStream, "[");
    for (i=0; i<MAX_MISSILES; i++) {
      if (game->missiles[i].o.alive) {
        fprintf(game->logStream, "%s[%.3f,%.3f,%.1f]",
                i==0?"":",",
                game->missiles[i].o.position.x,
                game->missiles[i].o.position.y,
                game->missiles[i].o.angle);
      }
    }
    fprintf(game->logStream, "],[");
    for (i=0; i<MAX_SHELLS; i++) {
      if (game->shells[i].o.alive) {
        fprintf(game->logStream, "%s[%.3f,%.3f,%.1f]",
                i==0?"":",",
                game->shells[i].o.position.x,
                game->shells[i].o.position.y,
                game->shells[i].o.angle);
      }
    }
    fprintf(game->logStream, "],");
    fprintf(game->logStream, "%d,%d,%d,%d,%d,",
            game->score.points,
            game->score.vulnerability,
            game->ship.thrustFlag?1:0,
            game->ship.turnFlag == TURN_LEFT ? 1:0,
            game->ship.turnFlag == TURN_RIGHT ? 1:0);
    /* Game Events */
    fprintf(game->logStream, "[");
    for(i=0; i<game->events.count; i++ ) {
      fprintf(game->logStream, "%s%d",
              i==0?"":",",
              game->events.events[i]);
    }
    fprintf(game->logStream, "],[");
    /* Input Events */
    for(i=0; i<game->keys.eventCount; i++ ) {
      fprintf(game->logStream, "%s[%d,%d]",
              i==0?"":",",
              game->keys.events[i].state?1:0,
              game->keys.events[i].sym);
    }
    fprintf(game->logStream, "]");
    fprintf(game->logStream, "]\n");
    fflush(game->logStream);
    return true;
  } else {
    return false;
  }
}
