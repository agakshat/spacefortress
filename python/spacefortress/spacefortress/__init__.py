import os.path
from ctypes import *
from ctypes.util import find_library

__all__ = []

try:
    dll_name = "libssfcairo.so"
    dll = cdll.LoadLibrary(os.path.join(os.path.dirname(os.path.realpath(__file__)), dll_name))
except RuntimeError as exc:
    raise ImportError(exc)

MAX_MISSILES = 50
MAX_SHELLS = 50

NO_TURN = 0
TURN_LEFT = 1
TURN_RIGHT = 2

Turn = c_uint

NO_KEY = 0
FIRE_KEY = 1
THRUST_KEY = 2
LEFT_KEY = 3
RIGHT_KEY = 4

KeySym = c_uint

class Key(Structure):
    _fields_ = [("sym", KeySym),
                ("state", c_bool)]

MISSILE_FIRED_EVENT = 0
FORTRESS_FIRED_EVENT = 1
SHELL_HIT_SHIP_EVENT = 2
HIT_FORTRESS_EVENT = 3
VLNER_INCREASED_EVENT = 4
VLNER_RESET_EVENT = 5
FORTRESS_DESTROYED_EVENT = 6
FORTRESS_RESPAWN_EVENT = 7
SHIP_RESPAWN_EVENT = 8
EXPLODE_BIGHEX_EVENT = 9
EXPLODE_SMALLHEX_EVENT = 10

Event = c_uint

Timer = c_int

class Point(Structure):
    _fields_ = [("x", c_double),
                ("y", c_double)]

class Object(Structure):
    _fields_ = [("position", Point),
                ("velocity", Point),
                ("angle", c_double),
                ("collisionRadius", c_int),
                ("alive", c_bool)]

class Hexagon(Structure):
    _fields_ = [("points", Point * 6),
                ("radius", c_int)]

class Ship(Structure):
    _fields_ = [("o", Object),
                ("deathTimer", Timer),
                ("thrustFlag", c_bool),
                ("turnFlag", Turn),
                ("vdir", c_double),
                ("speed", c_double),
                ("fdist", c_double),
                ("ndist", c_double)]

class Fortress(Structure):
    _fields_ = [("o", Object),
                ("deathTimer", Timer),
                ("vulnerabilityTimer", Timer),
                ("timer", Timer),
                ("lastAngle", c_double)]

class Missile(Structure):
    _fields_ = [("o", Object)]

class Shell(Structure):
    _fields_ = [("o", Object)]

class Score(Structure):
    _fields_ = [("points", c_int),
                ("rawPoints", c_int),
                ("vulnerability", c_int)]

class Stats(Structure):
    _fields_ = [("shipDeaths", c_int)]

class Keys(Structure):
    _fields_ = [("thrust", c_bool),
                ("left", c_bool),
                ("right", c_bool),
                ("fire", c_bool),
                ("eventCount", c_int),
                ("events", Key * 50),
                ("processed", c_bool)]

class Events(Structure):
    _fields_ = [("events", Event * 50),
                ("count", c_int)]

class Collisions(Structure):
    _fields_ = [("bigHex", c_bool),
                ("smallHex", c_bool),
                ("missileFortress", c_bool),
                ("shellShip", c_bool)]

class ProjectileConfig(Structure):
    _fields_ = [("speed", c_int),
                ("collisionRadius", c_int)]

class FortressConfig(Structure):
    _fields_ = [("sectorSize", c_int),
                ("lockTime", c_int),
                ("vulnerabilityTime", c_int),
                ("vulnerabilityThreshold", c_int),
                ("collisionRadius", c_int)]

class ShipConfig(Structure):
    _fields_ = [("explodeDuration", c_int),
                ("turnSpeed", c_int),
                ("collisionRadius", c_int),
                ("acceleration", c_double),
                ("startPosition", Point),
                ("startVelocity", Point),
                ("startAngle", c_int),
                ("vdir", c_double),
                ("speed", c_double),
                ("fdist", c_double),
                ("ndist", c_double)]

class Config(Structure):
    _fields_ = [("width", c_int),
                ("height", c_int),
                ("gameTime", c_int),
                ("destroyFortress", c_int),
                ("shipDeathPenalty", c_int),
                ("missilePenalty", c_int),
                ("hitReward", c_int),
                # shell
                ("shell", ProjectileConfig),
                # fortress
                ("fortress", FortressConfig),
                ("bigHex", c_int),
                ("smallHex", c_int),
                # missile
                ("missile", ProjectileConfig),
                # ship
                ("ship", ShipConfig),
                ("autoTurn", c_bool),
                ("grayscale", c_bool)]

class Game(Structure):
    _fields_ = [("config", Config),
                ("keys", Keys),
                ("ship", Ship),
                ("fortress", Fortress),
                ("missiles", Missile * MAX_MISSILES),
                ("shells", Shell * MAX_SHELLS),
                ("bigHex", Hexagon),
                ("smallHex", Hexagon),
                ("score", Score),
                ("stats", Stats),
                ("reward", c_int),
                ("grayscale", c_bool),
                ("tick", c_int),
                ("time", c_int),
                ("collisions", Collisions),
                ("events", Events)]

class WireFrameLine(Structure):
    _fields_ = [("_from", c_int), # from is reserved
                ("to", c_int)]

MAX_WIREFRAME_POINTS = 10
MAX_WIREFRAME_LINES = 10

class WireFrame(Structure):
    _fields_ = [("points", Point * MAX_WIREFRAME_POINTS),
                ("pointCount", c_int),
                ("lines", WireFrameLine * MAX_WIREFRAME_LINES),
                ("lineCount", c_int),
                ("r", c_int),
                ("g", c_int),
                ("b", c_int)]

missileWireFrame = WireFrame.in_dll(dll, "missileWireFrame")
shellWireFrame = WireFrame.in_dll(dll, "shellWireFrame")
shipWireFrame = WireFrame.in_dll(dll, "shipWireFrame")
fortressWireFrame = WireFrame.in_dll(dll, "fortressWireFrame")

_initGame = dll.initGame
_initGame.argtypes = [POINTER(Game)]

def initGame(game):
    _initGame(game)

_makeExplodeGame = dll.makeExplodeGame
_makeExplodeGame.argtypes = [c_bool]
_makeExplodeGame.restype = POINTER(Game)

def makeExplodeGame(grayscale=False):
    return _makeExplodeGame(grayscale)

_makeAutoTurnGame = dll.makeAutoTurnGame
_makeAutoTurnGame.argtypes = [c_bool]
_makeAutoTurnGame.restype = POINTER(Game)

def makeAutoTurnGame(grayscale=False):
    return _makeAutoTurnGame(grayscale)

freeGame = dll.freeGame
freeGame.argtypes = [POINTER(Game)]

pressKey = dll.pressKey
pressKey.argtypes = [POINTER(Game), KeySym]

releaseKey = dll.releaseKey
releaseKey.argtypes = [POINTER(Game), KeySym]

stepOneTick = dll.stepOneTick
stepOneTick.argtypes = [POINTER(Game), c_int]

isGameOver = dll.isGameOver
isGameOver.argtypes = [POINTER(Game)]
isGameOver.restype = c_bool

_dumpSexpGameState = dll.dumpSexpGameState
_dumpSexpGameState.argtypes = [POINTER(Game), c_char_p, c_size_t]

def dumpSexpGameState(game):
    n = 1000
    buf = create_string_buffer(n)
    _dumpSexpGameState(game, buf, n)
    return buf.value

openLog = dll.openLog
openLog.argtypes = [POINTER(Game), c_char_p]
openLog.restype = c_bool
closeLog = dll.closeLog
closeLog.argtypes = [POINTER(Game)]

logGameState = dll.logGameState
logGameState.argtypes = [POINTER(Game)]
logGameState.restype = c_bool

# Cairo related functionality

buffer_from_memory = pythonapi.PyBuffer_FromMemory
buffer_from_memory.argtypes = [c_void_p, c_int]
buffer_from_memory.restype = py_object

class PixelBuffer(Structure):
    _fields_ = [("surface", c_void_p),
                ("raw", c_void_p),
                ("height", c_int),
                ("stride", c_int)]

newPixelBuffer = dll.newPixelBuffer
newPixelBuffer.argtypes = [POINTER(Game), c_int, c_int]
newPixelBuffer.restype = POINTER(PixelBuffer)

freePixelBuffer = dll.freePixelBuffer
freePixelBuffer.argtypes = [POINTER(PixelBuffer)]

_drawGameStateScaled = dll.drawGameStateScaled
_drawGameStateScaled.argtypes = [POINTER(Game), c_void_p, c_float, c_float]

def drawGameStateScaled(game, pb, scale, ls):
    _drawGameStateScaled(game, pb.contents.surface, scale, ls)

def get_pixel_buffer_data(pb):
    return buffer_from_memory(pb.contents.raw, pb.contents.height * pb.contents.stride)


# print('game', sizeof(Game))
# print('config', sizeof(Config))
# print('ship', sizeof(Ship))
# print('fortress', sizeof(Fortress))
# print('missile', sizeof(Missile))
# print('shell', sizeof(Shell))
#
# game: 5928
# config: 144
# ship: 64
# fortress: 72
# missile: 48
# shell: 48
