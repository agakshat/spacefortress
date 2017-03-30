#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "ssf.h"

double deg(double rad) {
  return rad / M_PI * 180;
}

double rad(double d) {
  return d * M_PI / 180;
}

double angleTo(const Point *p1, const Point *p2) {
  return 0;
}

bool insideHexagon(const Hexagon *h, const Point *p) {
  return false;
}

bool collided(const Object *e1, const Object *e2) {
  double d = sqrt(pow(e1->position.x - e2->position.x, 2) + pow(e1->position.y - e2->position.y, 2));
  return d <= e1->collisionRadius + e2->collisionRadius;
}

void pressKey(Game *game, KeySym sym) {
  if (game->keys.eventCount < MAX_KEY_EVENTS) {
    game->keys.events[game->keys.eventCount].sym = sym;
    game->keys.events[game->keys.eventCount].state = true;
    game->keys.eventCount += 1;
  }
}

void releaseKey(Game *game, KeySym sym) {
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
  game->score.points += amount;
}

void penalize(Game *game, int amount) {
  reward(game, -amount);
}

int isOutsideGameArea(Game *game, Point *p) {
  return p->x < 0 || p->x > game->config.width || p->y > game->config.height || p->y < 0;
}

void monitorShipRespawn(Game *game) {
}

void fireShell(Game *game) {
  int i;
  for (i=0; i<MAX_SHELLS; i++) {
    if (!game->shells[i].o.alive) {
      game->shells[i].o.alive = true;
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
        penalize(game, game->config.missilePenalty);
        playSound(FIRE_MISSILE_SOUND);
        addEvent(game, MISSILE_FIRED_EVENT);
        return;
      }
    }

  }
}

void updateFortress(Game *game) {
  double dx = game->ship.o.position.x - game->fortress.o.position.x;
  double dy = game->ship.o.position.y - game->fortress.o.position.y;
  double angle_to_ship = fmod(deg(atan2(dy,dx)), 360);

  if (!game->fortress.o.alive && game->fortress.deathTimer > 1000) {
    game->fortress.o.alive = true;
    addEvent(game, FORTRESS_RESPAWN_EVENT);
  }

  if (game->ship.o.alive) {
    game->fortress.o.angle = fmod(floor(angle_to_ship / game->config.fortress.sectorSize) * game->config.fortress.sectorSize, 360);
    if (game->fortress.o.angle != game->fortress.lastAngle) {
      game->fortress.lastAngle = game->fortress.o.angle;
      game->fortress.timer = 0;
    }
    if (game->fortress.timer >= game->config.fortress.lockTime && game->ship.o.alive && game->fortress.o.alive) {
      fireShell(game);
    }
  }
}

void processKeyState (Game *game) {
  int i;
  for(i=0; i<game->keys.eventCount; i++) {
    if (game->keys.events[i].state == true) {
      if (game->keys.events[i].sym == LEFT_KEY) {
        game->ship.turnFlag = TURN_LEFT;
      } else if (game->keys.events[i].sym == RIGHT_KEY) {
        game->ship.turnFlag = TURN_RIGHT;
      } else if (game->keys.events[i].sym == THRUST_KEY) {
        game->ship.thrustFlag = true;
      } else if (game->keys.events[i].sym == FIRE_KEY) {
        fireMissile(game);
      }
    } else {
      if (game->keys.events[i].sym == LEFT_KEY || game->keys.events[i].sym == RIGHT_KEY) {
        game->ship.turnFlag = NO_TURN;
      } else if (game->keys.events[i].sym == THRUST_KEY) {
        game->ship.thrustFlag = false;
      }
    }
  }
  game->keys.eventCount = 0;
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
        game->ship.o.angle = fmod(game->ship.o.angle + game->config.ship.turnSpeed, 360);
      } else if (game->ship.turnFlag == TURN_RIGHT) {
        game->ship.o.angle = fmod(game->ship.o.angle - game->config.ship.turnSpeed, 360);
      }
    }
    if (game->ship.thrustFlag) {
      game->ship.o.velocity.x += game->ship.acceleration * cos(rad(game->ship.o.angle));
      game->ship.o.velocity.y += game->ship.acceleration * sin(rad(game->ship.o.angle));
    }
    game->ship.o.position.x += game->ship.o.velocity.x;
    game->ship.o.position.y += game->ship.o.velocity.y;

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
            addEvent(game, VLNER_INCREASED_EVENT);
          } else {
            if (game->score.vulnerability >= game->config.fortress.vulnerabilityThreshold + 1) {
              game->fortress.o.alive = false;
              game->fortress.deathTimer = 0;
              reward(game, game->config.destroyFortress);
              playSound(EXPLOSION_SOUND);
              addEvent(game, FORTRESS_DESTROYED_EVENT);
            } else {
              playSound(VLNER_RESET_SOUND);
              addEvent(game, VLNER_RESET_EVENT);
            }
            game->score.vulnerability = 0;
          }
          game->fortress.vulnerabilityTimer = 0;
        }
      } else if (isOutsideGameArea(game, &game->missiles[i].o.position)) {
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
  game->time += ms;
  game->fortress.timer += ms;
  game->fortress.deathTimer += ms;
  game->fortress.vulnerabilityTimer += ms;
  game->ship.deathTimer += ms;
}

void resetTick(Game *game) {
  game->collisions.bigHex = false;
  game->collisions.smallHex = false;
  game->collisions.missileFortress = false;
  game->collisions.shellShip = false;

  game->events.count = 0;
}

void StepOneTick(Game *game) {
  resetTick(game);
  stepTimers(game, 33);
  processKeyState(game);
  monitorShipRespawn(game);
  updateShip(game);
  updateFortress(game);
  updateShells(game);
  updateMissiles(game);
}

void baseConfig(Config *config) {
  config->width = 710;
  config->height = 600;
  config->gameTime = 60000;
  /* Points */
  config->destroyFortress = 100;
  config->shipDeathPenalty = 100;
  config->missilePenalty = 2;
  /* Fortress */
  config->fortress.shellSpeed = 6;
  config->fortress.sectorSize = 10;
  config->fortress.lockTime = 100;
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
  config->ship.startPosition.x = 235;
  config->ship.startPosition.y = 315;
  config->ship.startVelocity.x = cos(rad(60));
  config->ship.startVelocity.y = sin(rad(60));
  /* Game Modes */
  config->autoTurn = false;
}

void autoTurnConfig(Config *config) {
  baseConfig(config);
  config->autoTurn = true;
}

void explodeConfig(Config *config) {
  baseConfig(config);
}

void initGame(Game *game) {
  game->ship.o.collisionRadius = game->config.ship.collisionRadius;
  game->fortress.o.collisionRadius = game->config.fortress.collisionRadius;
  game->fortress.o.position.x = 355;
  game->fortress.o.position.x = 315;
  game->fortress.o.angle = 0;
}

Game* makeAutoTurnGame() {
  Game *g = malloc(sizeof(Game));
  if (g != NULL) {
    autoTurnConfig(&g->config);
    initGame(g);
  }
  return g;
}

Game* makeExplodeGame() {
  Game *g = malloc(sizeof(Game));
  if (g != NULL) {
    explodeConfig(&g->config);
    initGame(g);
  }
  return g;
}

int main(int argc, char **argv) {
  return 0;
}
