from __future__ import division
import argparse
import pyglet
from pyglet.window import key
import numpy as np

import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)),"../"))
import ssf

class SSF_Game(object):

    def __init__(self, args):
        if args.gametype == "explode":
            self.g = ssf.makeExplodeGame(grayscale=False)
        elif args.gametype == "autoturn":
            self.g = ssf.makeAutoTurnGame(grayscale=False)
        self.w = self.g.contents.config.width
        self.h = self.g.contents.config.height
        self.game_window = pyglet.window.Window(self.w, self.h)
        self.game_window.push_handlers(self)

        self.fire = False
        self.thrust = False
        self.turn = 0

        self.pb = ssf.newPixelBuffer(self.g, self.w, self.h)
        self.raw_pixels = ssf.get_pixel_buffer_data(self.pb)
        self.draw()

        pyglet.clock.schedule_interval(self.update, 1/30)

    def draw(self):
        ssf.drawGameStateScaled(self.g, self.pb, 1, 2)
        self.game_state = np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4)
        image = pyglet.image.ImageData(self.w, self.h, 'BGRA', self.game_state.tobytes(), pitch=self.w * -4)
        self.game_window.clear()
        self.game_window.switch_to()
        self.game_window.dispatch_events()
        image.blit(0,0)
        self.game_window.flip()

    def on_key_press(self, symbol, modifiers):
        if symbol == key.SPACE:
            ssf.pressKey(self.g, ssf.FIRE_KEY)
        elif symbol == key.W:
            ssf.pressKey(self.g, ssf.THRUST_KEY)
        elif symbol == key.A:
            ssf.pressKey(self.g, ssf.LEFT_KEY)
        elif symbol == key.D:
            ssf.pressKey(self.g, ssf.RIGHT_KEY)

    def on_key_release(self, symbol, modifiers):
        if symbol == key.SPACE:
            ssf.releaseKey(self.g, ssf.FIRE_KEY)
        elif symbol == key.W:
            ssf.releaseKey(self.g, ssf.THRUST_KEY)
        elif symbol == key.A:
            ssf.releaseKey(self.g, ssf.LEFT_KEY)
        elif symbol == key.D:
            ssf.releaseKey(self.g, ssf.RIGHT_KEY)

    def update(self, dt):
        # if not self.fire and key.SPACE in self.keys and self.keys[key.SPACE]:
        #     ssf.pressKey(self.g, ssf.FIRE_KEY)
        #     self.fire = True
        # else:
        #     ssf.releaseKey(self.g, ssf.FIRE_KEY)
        #     self.fire = False
        ssf.stepOneTick(self.g, int(np.round(dt*1000)))
        self.draw()

if __name__ == '__main__':
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--gametype', choices=["explode","autoturn"], default="explode")
    args = parser.parse_args()

    game = SSF_Game(args)

    pyglet.app.run()
