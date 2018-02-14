#include "space-fortress.hh"
#include <Python.h>
#include "PythonHelper.h"

typedef struct {
  PyObject_HEAD
  Game *game;
  Config *config;
  PixelBuffer *buffer;
} PySpaceFortressGameObject;

#define DEFGET(name, type, thing) \
static PyObject*get_##name(PySpaceFortressGameObject *self) {        \
  return Py_BuildValue(type, thing); \
}

DEFGET(game_tick, "i", self->game->mTick);
DEFGET(game_time, "i", self->game->mTime);
DEFGET(max_game_time, "i", self->game->mConfig->getInt("gameTime"));
DEFGET(ship_alive, "N", PyBool_FromLong(self->game->mShip.mAlive));
DEFGET(ship_x, "d", self->game->mShip.mPos.mX);
DEFGET(ship_y, "d", self->game->mShip.mPos.mY);
DEFGET(ship_vx, "d", self->game->mShip.mVel.mX);
DEFGET(ship_vy, "d", self->game->mShip.mVel.mY);
DEFGET(ship_angle, "d", self->game->mShip.mAngle);
DEFGET(vdir, "d", self->game->mExtra.vdir);
DEFGET(aim, "d", self->game->mExtra.aim);
DEFGET(ndist, "d", self->game->mExtra.ndist);
DEFGET(fortress_alive, "N", PyBool_FromLong(self->game->mFortress.mAlive));
DEFGET(fortress_angle, "d", self->game->mFortress.mAngle);
DEFGET(bighex, "i", self->game->mBighex.mRadius);
DEFGET(smallhex, "i", self->game->mSmallhex.mRadius);
DEFGET(points, "d", self->game->mScore.mPoints);
DEFGET(max_points, "d", self->game->mConfig->getDouble("maxPoints"));
DEFGET(raw_points, "d", self->game->mScore.mRawPoints);
DEFGET(vulnerability, "i", self->game->mScore.mVulnerability);
DEFGET(vulnerability_timer, "d", self->game->mFortress.mVulnerabilityTimer);
DEFGET(vulnerability_time, "d", self->game->mConfig->getInt("fortressVulnerabilityTime"));
DEFGET(thrust_flag, "N", PyBool_FromLong(self->game->mShip.mThrustFlag));
DEFGET(turn_flag, "i", self->game->mShip.mTurnFlag);


static PyObject *
get_stats(PySpaceFortressGameObject *self) {
  return Py_BuildValue("iiiiiiiiiiiiidd",
    self->game->mStats.bigHexDeaths,
    self->game->mStats.smallHexDeaths,
    self->game->mStats.shellDeaths,
    self->game->mStats.shipDeaths,
    self->game->mStats.resets,
    self->game->mStats.destroyedFortresses,
    self->game->mStats.missedShots,
    self->game->mStats.totalShots,
    self->game->mStats.totalThrusts,
    self->game->mStats.totalLefts,
    self->game->mStats.totalRights,
    self->game->mStats.vlnerIncs,
    self->game->mStats.maxVlner,
    self->game->mScore.mPoints,
    self->game->mScore.mRawPoints );
}

static PyObject *
get_timers(PySpaceFortressGameObject *self) {
  return Py_BuildValue("iiii",
    self->game->mShip.mFireTimer,
    self->game->mShip.mThrustTimer,
    self->game->mShip.mLeftTimer,
    self->game->mShip.mRightTimer );
}

static PyObject *
get_projectiles(Object *projectiles, int max) {
  int count = 0;
  for( int i=0; i<max; i++ ) if( projectiles[i].mAlive ) count++;
  PyObject* retval = PyTuple_New( count );
  if( !retval ) return NULL;

  for( int j=0, i=0; i<max; i++ ) {
    if( projectiles[i].mAlive ) {
      PyTuple_SET_ITEM( retval, j, Py_BuildValue("ddd",
                                                 projectiles[i].mPos.mX,
                                                 projectiles[i].mPos.mY,
                                                 projectiles[i].mAngle ));
      j++;
    }
  }
  return Py_BuildValue("O", retval);
}

static PyObject *
get_missiles(PySpaceFortressGameObject *self) {
  return get_projectiles( self->game->mMissiles, MAX_MISSILES );
}

static PyObject *
get_shells(PySpaceFortressGameObject *self) {
  return get_projectiles( self->game->mMissiles, MAX_MISSILES );
}

static PyObject *
get_events(PySpaceFortressGameObject *self) {
  PyObject* retval = PyTuple_New( self->game->mEvents.size() );
  for( size_t i=0; i < self->game->mEvents.size(); i++ ) {
    PyTuple_SET_ITEM( retval, i, Py_BuildValue("s", self->game->mEvents[i].c_str() ));
  }
  return Py_BuildValue("O", retval);
}

static PyObject *
get_thrust_durations(PySpaceFortressGameObject *self) {
  PyObject* retval = PyTuple_New( self->game->mThrustDurations.size() );
  for( size_t i=0; i < self->game->mThrustDurations.size(); i++ ) {
    PyTuple_SET_ITEM( retval, i, Py_BuildValue("i", self->game->mThrustDurations[i] ));
  }
  return Py_BuildValue("O", retval);
}

static PyObject *
get_shot_durations(PySpaceFortressGameObject *self) {
  PyObject* retval = PyTuple_New( self->game->mShotDurations.size() );
  for( size_t i=0; i < self->game->mShotDurations.size(); i++ ) {
    PyTuple_SET_ITEM( retval, i, Py_BuildValue("i", self->game->mShotDurations[i] ));
  }
  return Py_BuildValue("O", retval);
}

static PyObject *
get_shot_intervals_invul(PySpaceFortressGameObject *self) {
  PyObject* retval = PyTuple_New( self->game->mShotIntervalsInvul.size() );
  for( size_t i=0; i < self->game->mShotIntervalsInvul.size(); i++ ) {
    PyTuple_SET_ITEM( retval, i, Py_BuildValue("i", self->game->mShotIntervalsInvul[i] ));
  }
  return Py_BuildValue("O", retval);
}

static PyObject *
get_shot_intervals_vul(PySpaceFortressGameObject *self) {
  PyObject* retval = PyTuple_New( self->game->mShotIntervalsVul.size() );
  for( size_t i=0; i < self->game->mShotIntervalsVul.size(); i++ ) {
    PyTuple_SET_ITEM( retval, i, Py_BuildValue("i", self->game->mShotIntervalsVul[i] ));
  }
  return Py_BuildValue("O", retval);
}

static PyObject *
get_collisions(PySpaceFortressGameObject *self) {
  int count = 0;
  if( self->game->mCollisions.bigHex ) count++;
  if( self->game->mCollisions.smallHex ) count++;
  if( self->game->mCollisions.missileFortress ) count++;
  if( self->game->mCollisions.shellShip ) count++;

  PyObject* tuple = PyTuple_New( count );
  if( self->game->mCollisions.bigHex ) { PyTuple_SET_ITEM( tuple, count-1, Py_BuildValue("s", "bighex")); count--; }
  if( self->game->mCollisions.smallHex ) { PyTuple_SET_ITEM( tuple, count-1, Py_BuildValue("s", "smallhex")); count--; }
  if( self->game->mCollisions.missileFortress ) { PyTuple_SET_ITEM( tuple, count-1, Py_BuildValue("s", "missile")); count--; }
  if( self->game->mCollisions.shellShip ) { PyTuple_SET_ITEM( tuple, count-1, Py_BuildValue("s", "shell")); count--; }

  return Py_BuildValue("O", tuple);
}


static PyObject *
pressKey(PySpaceFortressGameObject *self, PyObject *args)
{
  KeySym sym;
  if (!PyArg_ParseTuple(args, "i", &sym))
    return NULL;

  self->game->pressKey( sym );

  Py_RETURN_NONE;
}

static PyObject *
releaseKey(PySpaceFortressGameObject *self, PyObject *args)
{
  KeySym sym;
  if (!PyArg_ParseTuple(args, "i", &sym))
    return NULL;

  self->game->releaseKey( sym );

  Py_RETURN_NONE;
}

static PyObject *
stepOneTick(PySpaceFortressGameObject *self, PyObject *args)
{
  int ms;
  if (!PyArg_ParseTuple(args, "i", &ms))
    return NULL;

  return Py_BuildValue("i", self->game->stepOneTick( ms ));
}

static PyObject *
isGameOver(PySpaceFortressGameObject *self, PyObject *args)
{
  if (self->game->isGameOver())
    Py_RETURN_TRUE;
  else
    Py_RETURN_FALSE;
}

static PyObject *
draw(PySpaceFortressGameObject *self, PyObject *args) {
  drawGameStateScaled( self->game, self->buffer );

  Py_RETURN_NONE;
}

static PyObject *
get_pixels(PySpaceFortressGameObject *self) {
  PyObject *buf;
  buf = PyBuffer_FromMemory( (char *)self->buffer->raw, self->buffer->height * self->buffer->stride );
  return Py_BuildValue("O", buf);
}

static PyObject *
get_pixels_width(PySpaceFortressGameObject *self) {
  return Py_BuildValue("i", self->buffer->width);
}

static PyObject *
get_pixels_height(PySpaceFortressGameObject *self) {
  return Py_BuildValue("i", self->buffer->height);
}

static PyObject *
get_config_value(PySpaceFortressGameObject *self, PyObject *args) {
  char *key;
  std::string k;
  if (!PyArg_ParseTuple(args, "s", &key))
    return NULL;
  if( self->game->mConfig->mValues.find(key) == self->game->mConfig->mValues.end() ) {
    PyErr_Format(PyExc_ValueError, "No config value for `%s'", key);
    return NULL;
  }
  k = key;
  switch( self->game->mConfig->mValues[k].mType ) {
  case ConfigType_Double:
    return Py_BuildValue("d", self->game->mConfig->mValues[k].mDouble);
  case ConfigType_Int:
    return Py_BuildValue("i", self->game->mConfig->mValues[k].mInt);
  case ConfigType_String:
    return Py_BuildValue("s", self->game->mConfig->mValues[k].mString.c_str());
  default:
    PyErr_Format(PyExc_ValueError, "Unknown config type for value `%s'", key);
    return NULL;
  }
}

static PyObject *
dumpState(PySpaceFortressGameObject *self, PyObject *args) {
  return Py_BuildValue("s", self->game->dumpState().c_str());
}

static void
PySpaceFortressGame_delete(PySpaceFortressGameObject *self)
{
  if( self->game ) delete self->game;
  if( self->config ) delete self->config;
  if( self->buffer ) freePixelBuffer(self->buffer);
  Py_TYPE(self)->tp_free((PyObject*)self);
}

PyObject* PySpaceFortressGame_Repr(PySpaceFortressGameObject* self)
{
  return Py_BuildValue("s", "<Game>");
}

static PyObject*
PySpaceFortressGame_new(PyTypeObject* type, PyObject*, PyObject*)
{
  PySpaceFortressGameObject* self = (PySpaceFortressGameObject*)type->tp_alloc(type, 0);
  self->game = NULL;
  self->config = NULL;
  self->buffer = NULL;
  return reinterpret_cast<PyObject*>(self);
}

static int
PySpaceFortressGame_init(PySpaceFortressGameObject* self, PyObject* args, PyObject *kwargs) {
  static const char* kw_names[] = {"config", "lw", "grayscale", "width", "height", "viewport", NULL};
  static char** kwlist = const_cast<char**>(kw_names);

  int width = -1, height = -1;
  int vp_x = 0, vp_y = 0, vp_width = -1, vp_height = -1;
  double line_width = 2.0;
  int grayscale = 0;
  char *config_name;
  if( !PyArg_ParseTupleAndKeywords( args, kwargs, "s|diii(iiii)", kwlist, &config_name, &line_width, &grayscale, &width, &height, &vp_x, &vp_y, &vp_width, &vp_height ))
    return -1;

  if( strcmp(config_name, "staircase-training") == 0 ) {
    self->config = trainingConfig();
  } else if( strcmp(config_name, "staircase") == 0 ) {
    self->config = staircaseConfig();
  } else if( strcmp(config_name, "autoturn") == 0 ) {
    self->config = autoturnConfig();
  } else if( strcmp(config_name, "explode") == 0 ) {
    self->config = explodeConfig();
  // } else if( strcmp(config_name, "deep-autoturn") == 0 ) {
  //   self->config = deepAutoturnConfig();
  // } else if( strcmp(config_name, "deep-explode") == 0 ) {
  //   self->config = deepExplodeConfig();
  } else {
    PyErr_Format(PyExc_RuntimeError, "cannot initialize %s. Unknown config value: `%s'", Py_TYPE(self)->tp_name, config_name);
    return -1;
  }

  self->game = new Game(self->config);
  if (vp_width == -1) vp_width = self->config->getInt("width");
  if (vp_height == -1) vp_height = self->config->getInt("height");
  if (width == -1) width = vp_width;
  if (height == -1) height = vp_height;

  self->buffer = newPixelBuffer( width, height, vp_x, vp_y, vp_width, vp_height, line_width, grayscale );

  return 0;
}

static PyObject*
PySpaceFortressGame_RichCompare(PySpaceFortressGameObject* self, PyObject* other, int op) {
  Py_RETURN_FALSE;
}

static PyMethodDef PySpaceFortressGame_methods[] = {
  {"press_key", (PyCFunction)pressKey, METH_VARARGS, NULL},
  {"release_key", (PyCFunction)releaseKey, METH_VARARGS, NULL},
  {"step_one_tick", (PyCFunction)stepOneTick, METH_VARARGS, NULL},
  {"is_game_over", (PyCFunction)isGameOver, METH_NOARGS, NULL},
  {"draw", (PyCFunction)draw, METH_NOARGS, NULL},
  {"config", (PyCFunction)get_config_value, METH_VARARGS, NULL},
  {"dump", (PyCFunction)dumpState, METH_NOARGS, NULL},
  {NULL, NULL, 0, NULL}
};

static PyGetSetDef PySpaceFortressGame_getset[] = {
  {(char*)"tick", (getter)get_game_tick, NULL, NULL, NULL},
  {(char*)"time", (getter)get_game_time, NULL, NULL, NULL},
  {(char*)"max_time", (getter)get_max_game_time, NULL, NULL, NULL},
  {(char*)"ship_alive", (getter)get_ship_alive, NULL, NULL, NULL},
  {(char*)"ship_x", (getter)get_ship_x, NULL, NULL, NULL},
  {(char*)"ship_y", (getter)get_ship_y, NULL, NULL, NULL},
  {(char*)"ship_vx", (getter)get_ship_vx, NULL, NULL, NULL},
  {(char*)"ship_vy", (getter)get_ship_vy, NULL, NULL, NULL},
  {(char*)"ship_angle", (getter)get_ship_angle, NULL, NULL, NULL},
  {(char*)"vdir", (getter)get_vdir, NULL, NULL, NULL},
  {(char*)"aim", (getter)get_aim, NULL, NULL, NULL},
  {(char*)"ndist", (getter)get_ndist, NULL, NULL, NULL},
  {(char*)"fortress_alive", (getter)get_fortress_alive, NULL, NULL, NULL},
  {(char*)"fortress_angle", (getter)get_fortress_angle, NULL, NULL, NULL},
  {(char*)"missiles", (getter)get_missiles, NULL, NULL, NULL},
  {(char*)"shells", (getter)get_shells, NULL, NULL, NULL},
  {(char*)"bighex", (getter)get_bighex, NULL, NULL, NULL},
  {(char*)"smallhex", (getter)get_smallhex, NULL, NULL, NULL},
  {(char*)"points", (getter)get_points, NULL, NULL, NULL},
  {(char*)"max_points", (getter)get_max_points, NULL, NULL, NULL},
  {(char*)"raw_points", (getter)get_raw_points, NULL, NULL, NULL},
  {(char*)"vulnerability", (getter)get_vulnerability, NULL, NULL, NULL},
  {(char*)"vulnerability_time", (getter)get_vulnerability_time, NULL, NULL, NULL},
  {(char*)"vulnerability_timer", (getter)get_vulnerability_timer, NULL, NULL, NULL},
  {(char*)"thrust_flag", (getter)get_thrust_flag, NULL, NULL, NULL},
  {(char*)"turn_flag", (getter)get_turn_flag, NULL, NULL, NULL},
  {(char*)"events", (getter)get_events, NULL, NULL, NULL},
  {(char*)"thrust_durations", (getter)get_thrust_durations, NULL, NULL, NULL},
  {(char*)"shot_durations", (getter)get_shot_durations, NULL, NULL, NULL},
  {(char*)"shot_intervals_invul", (getter)get_shot_intervals_invul, NULL, NULL, NULL},
  {(char*)"shot_intervals_vul", (getter)get_shot_intervals_vul, NULL, NULL, NULL},
  {(char*)"collisions", (getter)get_collisions, NULL, NULL, NULL},
  {(char*)"pb_pixels", (getter)get_pixels, NULL, NULL, NULL},
  {(char*)"pb_width", (getter)get_pixels_width, NULL, NULL, NULL},
  {(char*)"pb_height", (getter)get_pixels_height, NULL, NULL, NULL},
  {(char*)"stats", (getter)get_stats, NULL, NULL, NULL},
  {(char*)"timers", (getter)get_timers, NULL, NULL, NULL},
  {NULL, NULL, NULL, NULL, NULL}
};

PyTypeObject PySpaceFortressGame_Type = {
    PyVarObject_HEAD_INIT(0, 0)
    "Game",                                       /* tp_name */
    sizeof(PySpaceFortressGameObject),            /* tp_basicsize */
    0,                                            /* tp_itemsize */
    (destructor)PySpaceFortressGame_delete,       /* tp_dealloc */
    NULL,                                         /* tp_print */
    NULL,                                         /* tp_getattr */
    NULL,                                         /* tp_setattr */
    NULL,                                         /* tp_compare */
    (reprfunc)PySpaceFortressGame_Repr,           /* tp_repr */
    NULL,                                         /* tp_as_number */
    NULL,                                         /* tp_as_sequence */
    NULL,                                         /* tp_as_mapping */
    NULL,                                         /* tp_hash */
    NULL,                                         /* tp_call */
    (reprfunc)PySpaceFortressGame_Repr,           /* tp_str */
    NULL,                                         /* tp_getattro */
    NULL,                                         /* tp_setattro */
    NULL,                                         /* tp_as_buffer */
    Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,     /* tp_flags */
    NULL,                                         /* tp_doc */
    NULL,                                         /* tp_traverse */
    NULL,                                         /* tp_clear */
    (richcmpfunc)PySpaceFortressGame_RichCompare, /* tp_richcompare */
    NULL,                                         /* tp_weaklistoffset */
    NULL,                                         /* tp_iter */
    NULL,                                         /* tp_iternext */
    PySpaceFortressGame_methods,                  /* tp_methods */
    NULL,                                         /* tp_members */
    PySpaceFortressGame_getset,                   /* tp_getset */
    NULL,                                         /* tp_base */
    NULL,                                         /* tp_dict */
    NULL,                                         /* tp_descr_get */
    NULL,                                         /* tp_descr_set */
    NULL,                                         /* tp_dictoffset */
    (initproc)PySpaceFortressGame_init,           /* tp_init */
    NULL,                                         /* tp_alloc */
    PySpaceFortressGame_new,                      /* tp_new */
};

static PyMethodDef SpaceFortressMethods[] = {
  {NULL, NULL, 0, NULL}
};

static void add_to_module(PyObject *m) {
  PySpaceFortressGame_Type.tp_new = PyType_GenericNew;
  if (PyType_Ready(&PySpaceFortressGame_Type) < 0)
    return;

  Py_INCREF(&PySpaceFortressGame_Type);
  if(PyModule_AddObject(m, "Game", (PyObject *)&PySpaceFortressGame_Type) < 0)
    return;

  PyModule_AddObject(m, "LEFT_KEY", Py_BuildValue( "i", LEFT_KEY ));
  PyModule_AddObject(m, "RIGHT_KEY", Py_BuildValue( "i", RIGHT_KEY ));
  PyModule_AddObject(m, "THRUST_KEY", Py_BuildValue( "i", THRUST_KEY ));
  PyModule_AddObject(m, "FIRE_KEY", Py_BuildValue( "i", FIRE_KEY ));

  PyModule_AddObject(m, "MAX_MISSILES", Py_BuildValue( "i", MAX_MISSILES ));
  PyModule_AddObject(m, "MAX_SHELLS", Py_BuildValue( "i", MAX_SHELLS ));
}

#if PY_MAJOR_VERSION == 2

PyMODINIT_FUNC
init_spacefortress ()
{
  PyObject *m;

  m = Py_InitModule("_spacefortress", SpaceFortressMethods);
  if( m == NULL ) return;
  initWireframes();
  add_to_module(m);
}

#elif PY_MAJOR_VERSION == 3

static struct PyModuleDef SpaceFortressModule = {
  PyModuleDef_HEAD_INIT,
  "_spacefortress",
  NULL,
  -1,
  SpaceFortressMethods,
  NULL, NULL, NULL, NULL
};

PyMODINIT_FUNC
PyInit__spacefortress ()
{
  PyObject *m = PyModule_Create(&SpaceFortressModule);
  if( m == NULL ) return NULL;
  initWireframes();
  add_to_module(m);
  return m;
}
#endif
