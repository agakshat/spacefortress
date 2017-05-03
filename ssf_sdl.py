import math
import sdl2
import ssf

def drawWireFrame(r, wf, p, angle):
    s = math.sin(angle * math.pi / 180);
    c = math.cos(angle * math.pi / 180);
    points = []
    for i in range(wf.pointCount):
        np = ssf.Point()
        np.x = wf.points[i].x * c - wf.points[i].y * s + p.x
        np.y = -(wf.points[i].x * s + wf.points[i].y * c) + p.y
        points.append(np)

    sdl2.SDL_SetRenderDrawColor(r, wf.r, wf.g, wf.b, sdl2.SDL_ALPHA_OPAQUE)

    for i in range(wf.lineCount):
        sdl2.SDL_RenderDrawLine(r,
                                int(points[wf.lines[i]._from].x), int(points[wf.lines[i]._from].y),
                                int(points[wf.lines[i].to].x), int(points[wf.lines[i].to].y))

def drawHexagon(r, h):
    sdl2.SDL_SetRenderDrawColor(r, 0, 255, 0, sdl2.SDL_ALPHA_OPAQUE)
    for i in range(6):
        sdl2.SDL_RenderDrawLine(r,
                                int(h.points[i].x), int(h.points[i].y),
                                int(h.points[(i+1)%6].x), int(h.points[(i+1)%6].y))

def drawGame(r, g):
    sdl2.SDL_SetRenderDrawColor(r, 0, 0, 0, sdl2.SDL_ALPHA_OPAQUE)
    sdl2.SDL_RenderClear(r);

    drawHexagon(r, g.contents.bigHex);
    drawHexagon(r, g.contents.smallHex);

    if g.contents.ship.o.alive:
        drawWireFrame(r, ssf.shipWireFrame, g.contents.ship.o.position, g.contents.ship.o.angle)

    if g.contents.fortress.o.alive:
        drawWireFrame(r, ssf.fortressWireFrame, g.contents.fortress.o.position, g.contents.fortress.o.angle)

    for i in range(ssf.MAX_MISSILES):
        if g.contents.missiles[i].o.alive:
            drawWireFrame(r, ssf.missileWireFrame, g.contents.missiles[i].o.position, g.contents.missiles[i].o.angle)

    for i in range(ssf.MAX_SHELLS):
        if g.contents.shells[i].o.alive:
            drawWireFrame(r, ssf.shellWireFrame, g.contents.shells[i].o.position, g.contents.shells[i].o.angle)

def pumpAndWait(until):
    while sdl2.SDL_GetTicks() < until:
        sdl2.SDL_PumpEvents()
        sdl2.SDL_Delay(1)

def getKeySym(e):
    if e.key.keysym.sym == sdl2.SDLK_w:
        return ssf.THRUST_KEY
    elif e.key.keysym.sym == sdl2.SDLK_a:
        return ssf.LEFT_KEY
    elif e.key.keysym.sym == sdl2.SDLK_d:
        return ssf.RIGHT_KEY
    elif e.key.keysym.sym == sdl2.SDLK_SPACE:
        return ssf.FIRE_KEY
    else:
        return ssf.NO_KEY

def readEvents(g):
    e = sdl2.SDL_Event()
    while sdl2.SDL_PollEvent(e) == 1:
        if e.type == sdl2.SDL_KEYDOWN:
            k = getKeySym(e)
            if k != ssf.NO_KEY:
                ssf.pressKey(g, k)
        elif e.type == sdl2.SDL_KEYUP:
            k = getKeySym(e)
            if k != ssf.NO_KEY:
                ssf.releaseKey(g, k)
        elif e.type == sdl2.SDL_QUIT:
            exit(0)

if __name__ == '__main__':
    sdl2.SDL_Init(sdl2.SDL_INIT_VIDEO)

    w = sdl2.SDL_CreateWindow("Space Fortress",
                              sdl2.SDL_WINDOWPOS_CENTERED,
                              sdl2.SDL_WINDOWPOS_CENTERED,
                              710, 600, 0)
    r = sdl2.SDL_CreateRenderer(w, -1, sdl2.SDL_RENDERER_ACCELERATED)

    now = sdl2.SDL_GetTicks()
    g = ssf.makeExplodeGame()
    while not ssf.isGameOver(g):
        last = now
        now = sdl2.SDL_GetTicks()
        readEvents(g)
        ssf.stepOneTick(g, now-last)
        drawGame(r, g)
        sdl2.SDL_RenderPresent(r)
        pumpAndWait(now + 33)

    print ssf.dumpSexpGameState(g)
