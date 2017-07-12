from __future__ import division
import pyglet
from pyglet.window import key
import numpy as np

import spacefortress as sf

class SSF_Game(object):

    FPS = 30

    def __init__(self, args):
        if args.gametype == "explode":
            self.g = sf.makeExplodeGame(grayscale=False)
        elif args.gametype == "autoturn":
            self.g = sf.makeAutoTurnGame(grayscale=False)
        self.w = self.g.contents.config.width
        self.h = self.g.contents.config.height

        platform = pyglet.window.get_platform()
        display = platform.get_default_display()
        screen = display.get_default_screen()
        self.screen_width = screen.width
        self.screen_height = screen.height

        # Should start with visible=False and move window while hidden but this is not working
        self.game_window = pyglet.window.Window(self.w, self.h, resizable=False, visible=True)
        self.game_window.set_location(int(self.screen_width/2-self.w/2), int(self.screen_height/2-self.h/2))
        self.game_window.set_visible(True)

        self.game_window.push_handlers(self)

        self.fire = False
        self.thrust = False
        self.turn = 0

        self.pb = sf.newPixelBuffer(self.g, self.w, self.h)
        self.raw_pixels = sf.get_pixel_buffer_data(self.pb)
        self.draw()

        pyglet.clock.schedule_interval(self.update, 1/self.FPS)

    def draw(self):
        sf.drawGameStateScaled(self.g, self.pb, 1, 2)
        self.game_state = np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4)
        image = pyglet.image.ImageData(self.w, self.h, 'BGRA', self.game_state.tobytes(), pitch=self.w * -4)
        self.game_window.clear()
        self.game_window.switch_to()
        self.game_window.dispatch_events()
        image.blit(0,0)
        self.game_window.flip()

    def on_key_press(self, symbol, modifiers):
        if symbol == key.SPACE:
            sf.pressKey(self.g, sf.FIRE_KEY)
        elif symbol == key.W:
            sf.pressKey(self.g, sf.THRUST_KEY)
        elif symbol == key.A:
            sf.pressKey(self.g, sf.LEFT_KEY)
        elif symbol == key.D:
            sf.pressKey(self.g, sf.RIGHT_KEY)

    def on_key_release(self, symbol, modifiers):
        if symbol == key.SPACE:
            sf.releaseKey(self.g, sf.FIRE_KEY)
        elif symbol == key.W:
            sf.releaseKey(self.g, sf.THRUST_KEY)
        elif symbol == key.A:
            sf.releaseKey(self.g, sf.LEFT_KEY)
        elif symbol == key.D:
            sf.releaseKey(self.g, sf.RIGHT_KEY)

    def update(self, dt):
        sf.stepOneTick(self.g, int(np.round(dt*1000)))
        self.draw()
        if sf.isGameOver(self.g):
            self.cleanup()
            pyglet.app.exit()
