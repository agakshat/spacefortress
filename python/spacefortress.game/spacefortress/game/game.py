from __future__ import division
import pyglet
from pyglet.window import key
import numpy as np

import spacefortress.core as sf
from spacefortress.util import ImageEncoder

class SSF_Game(pyglet.app.EventLoop):

    FPS = 30

    def __init__(self, args):
        super(SSF_Game, self).__init__()
        self.g = sf.Game(args.game, viewport=(130,80,450,460), lw=2, grayscale=False)
        self.w = self.g.pb_width
        self.h = self.g.pb_height

        if args.video:
            self.encoder = ImageEncoder(args.video, (self.h, self.w, 4), self.FPS)
        else:
            self.encoder = None

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

        self.raw_pixels = self.g.pb_pixels
        self.draw()

        pyglet.clock.schedule_interval(self.update, 1/self.FPS)

    def draw(self):
        self.g.draw()
        self.game_state = np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4)
        if self.encoder:
            self.encoder.capture_frame(self.game_state)
        image = pyglet.image.ImageData(self.w, self.h, 'BGRA', self.game_state.tobytes(), pitch=self.w * -4)
        self.game_window.clear()
        self.game_window.switch_to()
        self.game_window.dispatch_events()
        image.blit(0,0)
        self.game_window.flip()

    def on_key_press(self, symbol, modifiers):
        if symbol == key.SPACE:
            self.g.press_key(sf.FIRE_KEY)
        elif symbol == key.W:
            self.g.press_key(sf.THRUST_KEY)
        elif symbol == key.A:
            self.g.press_key(sf.LEFT_KEY)
        elif symbol == key.D:
            self.g.press_key(sf.RIGHT_KEY)
        elif symbol == key.ESCAPE:
            self.cleanup()

    def on_key_release(self, symbol, modifiers):
        if symbol == key.SPACE:
            self.g.release_key(sf.FIRE_KEY)
        elif symbol == key.W:
            self.g.release_key(sf.THRUST_KEY)
        elif symbol == key.A:
            self.g.release_key(sf.LEFT_KEY)
        elif symbol == key.D:
            self.g.release_key(sf.RIGHT_KEY)

    def on_close(self):
        self.cleanup()

    def cleanup(self):
        if self.encoder:
            self.encoder.close()
        self.exit()

    def update(self, dt):
        self.g.step_one_tick(int(np.round(dt*1000)))
        self.draw()
        if self.g.is_game_over():
            self.cleanup()
