import numpy as np
import gym
from gym import spaces
from gym.utils import seeding

import os, platform

import logging
logger = logging.getLogger(__name__)

import cv2
import ssf

class SSF_Env(gym.Env):

    metadata = {
        'render.modes': ['rgb_array'],
        'video.frames_per_second' : 30
    }

    def __init__(self, gametype="explode", scale=.2, ls=3):
        self._seed()
        self.gametype = gametype
        self.scale = scale
        self.ls = ls
        self.tickdur = int(np.ceil(1./self.metadata['video.frames_per_second']*1000))
        if self.gametype == "explode":
            self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1], [0, 1], [0, 1])).T.reshape(-1,4)
            self.g = ssf.makeExplodeGame(grayscale=True)
        elif self.gametype == "autoturn":
            self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1])).T.reshape(-1,2)
            self.g = ssf.makeAutoTurnGame(grayscale=True)
        self.action_space = spaces.Discrete(len(self.action_combinations))
        self.videofile = None
        self.video = None

    def _seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]

    def _reset(self):
        if self.gametype == "explode":
            self.g = ssf.makeExplodeGame(grayscale=True)
        elif self.gametype == "autoturn":
            self.g = ssf.makeAutoTurnGame(grayscale=True)
        self.w = int(np.ceil(self.g.contents.config.width * self.scale))
        self.h = int(np.ceil(self.g.contents.config.height * self.scale))
        self.observation_space = spaces.Box(low=0, high=255, shape=(self.h, self.w, 3))
        self.pb = ssf.newPixelBuffer(self.g, self.w, self.h)
        self.raw_pixels = ssf.get_pixel_buffer_data(self.pb)
        ssf.drawGameStateScaled(self.g, self.pb, self.scale, self.ls)
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
        if self.videofile and not self.video:
            vf = "%s.avi" % self.videofile
            lm = "latest-model.avi"
            self.video = cv2.VideoWriter(vf, cv2.VideoWriter_fourcc(*"H264"), self.metadata['video.frames_per_second'], (self.w,self.h))
            if platform.system() != "Windows":
                if os.path.exists(lm):
                    os.unlink(lm)
                os.symlink(vf, lm)
            self.video.write(cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB))
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
        reward = self.g.contents.reward
        ssf.drawGameStateScaled(self.g, self.pb, self.scale, self.ls)
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
        done = ssf.isGameOver(self.g)
        if self.videofile:
            img = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)
            self.video.write(img)
            if done:
                self.video.release()
                cv2.destroyAllWindows()
                self.video = None
        return self.game_state, reward, done, {}
