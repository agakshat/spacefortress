import numpy as np
import gym
from gym import spaces
from gym.utils import seeding

import logging
logger = logging.getLogger(__name__)

import cv2
import ssf

class SSF_Env(gym.Env):

    metadata = {
        'render.modes': ['rgb_array'],
        'video.frames_per_second' : 30
    }

    def __init__(self, gametype="explode", scale=.5):
        self._seed()
        self.gametype = gametype
        self.scale = scale

    def _seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]

    def _reset(self):
        self.tickdur = int(np.ceil(1./self.metadata['video.frames_per_second']*1000))
        if self.gametype == "explode":
            self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1], [0, 1], [0, 1])).T.reshape(-1,4)
            self.g = ssf.makeExplodeGame()
        elif self.gametype == "autoturn":
            self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1])).T.reshape(-1,2)
            self.g = ssf.makeAutoTurnGame()
        self.action_space = spaces.Discrete(len(self.action_combinations))
        self.w = int(np.ceil(self.g.contents.config.width * self.scale))
        self.h = int(np.ceil(self.g.contents.config.height * self.scale))
        self.observation_space = spaces.Box(low=0, high=255, shape=(self.h, self.w, 3))
        self.pb = ssf.newPixelBuffer(self.g, self.w, self.h)
        self.raw_pixels = ssf.get_pixel_buffer_data(self.pb)
        ssf.drawGameStateScaled(self.g, self.pb, self.scale)
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2RGB)
        return self.game_state

    def _render(self, mode='rgb_array', close=False):
        return True

    def _destroy(self):
        pass

    def _step(self, action):
        done = False
        reward = 0
        keystate = self.action_combinations[action]
        if keystate[0]:
            ssf.pressKey(self.g, ssf.FIRE_KEY)
        else:
            ssf.releaseKey(self.g, ssf.FIRE_KEY)
        if keystate[1]:
            ssf.pressKey(self.g, ssf.THRUST_KEY)
        else:
            ssf.releaseKey(self.g, ssf.THRUST_KEY)
        if self.gametype == "explode":
            if keystate[2]:
                ssf.pressKey(self.g, ssf.LEFT_KEY)
            else:
                ssf.releaseKey(self.g, ssf.LEFT_KEY)
            if keystate[3]:
                ssf.pressKey(self.g, ssf.RIGHT_KEY)
            else:
                ssf.releaseKey(self.g, ssf.RIGHT_KEY)
        ssf.stepOneTick(self.g, self.tickdur)
        reward = self.g.reward
        ssf.drawGameStateScaled(self.g, self.pb, self.scale)
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2RGB)
        done = ssf.isGameOver(self.g)
        return self.game_state, reward, done, {}
