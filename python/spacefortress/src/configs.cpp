#include "space-fortress.hh"

void baseConfig(Config *c) {
  c->set("width", 710);
  c->set("height", 626);
  c->set("gameTime", 60000);
  c->set("destroyFortress", 100);
  c->set("shipDeathPenalty", 100);
  c->set("missilePenalty", 2);
  c->set("missPenalty", 0);
  c->set("incReward", 0);

  c->set("maxPoints", 1249);
  c->set("maxBonus", 90);

  c->set("shellSpeed", 6);
  c->set("shellCollisionRadius", 3);

  c->set("missileSpeed", 20);
  c->set("missileCollisionRadius", 5);

  c->set("autoTurn", false);
  c->set("staircase", false);
  c->set("fortressPointsPerContraction", 10);
  // Fortress
  c->set("fortressSectorSize", 10);
  c->set("fortressLockTime", 1000);
  c->set("fortressVulnerabilityTime", 250);
  c->set("fortressVulnerabilityThreshold", 10);
  c->set("fortressCollisionRadius", 18);
  // Hexagons
  c->set("bigHex", 200);
  c->set("smallHex", 40);
  c->set("hexContraction", 5);
  c->set("hexExpansion", 15);
  c->set("minHexDistance", 20);
  // Ship
  c->set("shipExplodeDuration", 1000);
  c->set("shipStartX", 235.0);
  c->set("shipStartY", 315.0);
  c->set("shipStartVelX", cos(deg2rad(-60)));
  c->set("shipStartVelY", sin(deg2rad(-60)));
  c->set("shipStartAngle", 0.0);
  c->set("shipCollisionRadius", 10);
  c->set("shipAcceleration", 0.3);
  c->set("shipTurnSpeed", 6);
}

Config *trainingConfig() {
  Config *c = new Config;
  baseConfig(c);
  c->set("autoTurn", true);
  return c;
}

Config *staircaseConfig() {
  Config *c = new Config;
  baseConfig(c);
  c->set("autoTurn", true);
  c->set("staircase", true);
  return c;
}

Config *autoturnConfig() {
  Config *c = new Config;
  baseConfig(c);
  c->set("autoTurn", true);
  c->set("gameTime", 180000);
  c->set("maxPoints", 3748);
  return c;
}

Config *explodeConfig() {
  Config *c = new Config;
  baseConfig(c);
  c->set("gameTime", 180000);
  c->set("maxPoints", 3748);
  return c;
}

void deepConfig(Config *c) {
  c->set("missilePenalty", 0);
  c->set("missPenalty", 2);
  c->set("incReward", 10);
}

Config *deepAutoturnConfig() {
  Config *c = autoturnConfig();
  deepConfig(c);
  return c;
}

Config *deepExplodeConfig() {
  Config *c = explodeConfig();
  deepConfig(c);
  return c;
}
