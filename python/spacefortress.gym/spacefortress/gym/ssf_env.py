from __future__ import division

import pyglet

import numpy as np
import gym
from gym import spaces
from gym.utils import seeding

import os, platform

import logging
logger = logging.getLogger(__name__)

import cv2
import spacefortress as sf

class SSF_Viewer(object):
    def __init__(self, display=None):
        self.window = None
        self.isopen = False
        self.display = display
    def imshow(self, arr):
        if self.window is None:
            height, width = arr.shape
            self.window = pyglet.window.Window(width=width, height=height, display=self.display, vsync=False)
            self.width = width
            self.height = height
            self.isopen = True
        image = pyglet.image.ImageData(self.width, self.height, 'I', arr.tobytes(), pitch=self.width * -1)
        self.window.clear()
        self.window.switch_to()
        self.window.dispatch_events()
        image.blit(0,0)
        self.window.flip()
    def close(self):
        if self.isopen:
            self.window.close()
            self.isopen = False
    def __del__(self):
        self.close()

class SSF_Env(gym.Env):

    metadata = {
        'render.modes': ['human','rgb_array'],
        'video.frames_per_second' : 30
    }

    def __init__(self, gametype="explode", scale=.2, ls=3, action_set=0, continuous=False, obs_type='image'):
        assert obs_type in ('image', 'features')
        self.obs_type = obs_type
        self._seed()
        self.continuous = continuous
        self.viewer = None
        self.last_action = None
        self.gametype = gametype
        self.scale = scale
        self.ls = ls
        self.tickdur = int(np.ceil(1./self.metadata['video.frames_per_second']*1000))

        # 0=FIRE, 1=THRUST, 2=LEFT, 3=RIGHT
        if self.gametype == "explode":
            # if action_set == 1:
            #     self.action_combinations = np.array([
            #         [0, 0, 0, 0], # NOOP
            #         [1, 0, 0, 0], # FIRE
            #         [0, 1, 0, 0], # THRUST
            #         [0, 0, 1, 0], # LEFT
            #         [0, 0, 0, 1], # RIGHT
            #         [1, 1, 0, 0], # FIRE & THRUST
            #         [1, 0, 1, 0], # FIRE & LEFT
            #         [1, 0, 0, 1], # FIRE & RIGHT
            #     ])
            # if action_set == 2:
            #     self.action_combinations = np.array([
            #         [0, 0, 0, 0], # NOOP
            #         [1, 0, 0, 0], # FIRE
            #         [0, 1, 0, 0], # THRUST
            #         [0, 0, 1, 0], # LEFT
            #         [0, 0, 0, 1], # RIGHT
            #     ])
            # else:
            self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1], [0, 1], [0, 1])).T.reshape(-1,4)
            self.g = sf.makeExplodeGame(grayscale=True)
        elif self.gametype == "autoturn":
            # if action_set == 1:
            #     self.action_combinations = np.array([
            #         [0, 0], # NOOP
            #         [1, 0], # FIRE
            #         [0, 1], # THRUST
            #         [1, 1], # FIRE & THRUST
            #     ])
            # if action_set == 2:
            #     self.action_combinations = np.array([
            #         [0, 0], # NOOP
            #         [1, 0], # FIRE
            #         [0, 1], # THRUST
            #     ])
            # else:
            self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1], [0, 1], [0, 1])).T.reshape(-1,4)
            self.g = sf.makeAutoTurnGame(grayscale=True)
        if self.continuous:
            if self.gametype == "explode":
                # Action is 3 floats [fire, thrust, turn].
                # fire: -1..0 off, 0..+1 on
                # thrust: -1..0 off, 0..+1 on
                # turn:  -1.0..-0.5 left, +0.5..+1.0 right, -0.5..0.5 nothing
                self.action_space = spaces.Box(-1, +1, (3,))
            elif self.gametype == "autoturn":
                # Action is 2 floats [fire, thrust].
                # fire: -1..0 off, 0..+1 on
                # thrust: -1..0 off, 0..+1 on
                self.action_space = spaces.Box(-1, +1, (2,))
        else:
            self.action_space = spaces.Discrete(len(self.action_combinations))

        if self.gametype == "explode":
            self.g = sf.makeExplodeGame(grayscale=True)
        elif self.gametype == "autoturn":
            self.g = sf.makeAutoTurnGame(grayscale=True)
        self.w = int(np.ceil(self.g.contents.config.width * self.scale))
        self.h = int(np.ceil(self.g.contents.config.height * self.scale))
        self.pb = sf.newPixelBuffer(self.g, self.w, self.h)
        self.raw_pixels = sf.get_pixel_buffer_data(self.pb)

        self.videofile = None
        self.videofile2 = None
        self.video = None

        self.MAX_SCORE = int(((self.g.contents.config.gameTime/1000) / (.250 * 11 + 1)) * (100-2*12))

        if self.obs_type == 'image':
            self.observation_space = spaces.Box(low=0, high=255, shape=(self.h, self.w, 3))
        elif self.obs_type == 'features':
            self.observation_space = spaces.Box(low=-1, high=1, shape=self._get_features().shape)

    def _get_features(self):
        return np.clip(np.array([
            1 if self.g.contents.ship.o.alive else 0,
            self.g.contents.ship.o.position.x / self.g.contents.config.width,
            self.g.contents.ship.o.position.y / self.g.contents.config.height,
            self.g.contents.ship.o.velocity.x / 10,
            self.g.contents.ship.o.velocity.y / 10,
            self.g.contents.ship.o.angle / 360,
            self.g.contents.ship.vdir % 360 / 360,
            1 if self.g.contents.fortress.o.alive else 0,
            self.g.contents.fortress.o.angle / 360,
            len([m for m in self.g.contents.missiles if m.o.alive]) / sf.MAX_MISSILES,
            len([m for m in self.g.contents.shells if m.o.alive]) / sf.MAX_SHELLS,
            max(self.g.contents.score.vulnerability, 10) / 10,
            self.g.contents.score.points / self.MAX_SCORE
            ]), -1, 1)

    def _seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]

    def _reset(self):
        sf.initGame(self.g)
        sf.drawGameStateScaled(self.g, self.pb, self.scale, self.ls)
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
        self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)
        if self.videofile and not self.video:
            vf = "%s.avi" % self.videofile
            self.video = cv2.VideoWriter(vf, cv2.VideoWriter_fourcc(*"H264"), self.metadata['video.frames_per_second'], (self.w,self.h))
            if self.videofile2!=None and platform.system() != "Windows":
                if os.path.exists(self.videofile2):
                    os.unlink(self.videofile2)
                os.symlink(vf, self.videofile2)
            self.video.write(self.game_gray_rgb)
        return self.game_gray_rgb

    def _render(self, mode='human', close=False):
        if close:
            if self.viewer is not None:
                self.viewer.close()
                self.viewer = None
            return

        if self.viewer is None:
            self.viewer = SSF_Viewer()

        self.viewer.imshow(self.game_state)

        if mode == 'rgb_array':
            return self.game_gray_rgb

    def _destroy(self):
        pass

    def _step(self, action):
        done = False
        reward = 0
        if self.continuous:
            if action[0] > 0:
                sf.pressKey(self.g, sf.FIRE_KEY)
            else:
                sf.releaseKey(self.g, sf.FIRE_KEY)
            if action[1] > 0:
                sf.pressKey(self.g, sf.THRUST_KEY)
            else:
                sf.releaseKey(self.g, sf.THRUST_KEY)
            if self.gametype == "explode":
                if action[2] < -0.5:
                    if self.last_action != None and self.last_action > 0.5:
                        sf.releaseKey(self.g, sf.RIGHT_KEY)
                    sf.pressKey(self.g, sf.LEFT_KEY)
                elif action[2] > 0.5:
                    if self.last_action != None and self.last_action < -0.5:
                        sf.releaseKey(self.g, sf.LEFT_KEY)
                    sf.pressKey(self.g, sf.RIGHT_KEY)
                else:
                    if self.last_action != None and self.last_action < -0.5:
                        sf.releaseKey(self.g, sf.LEFT_KEY)
                    elif self.last_action != None and self.last_action > 0.5:
                        sf.releaseKey(self.g, sf.RIGHT_KEY)
        else:
            keystate = self.action_combinations[action]
            if keystate[0]:
                sf.pressKey(self.g, sf.FIRE_KEY)
            else:
                sf.releaseKey(self.g, sf.FIRE_KEY)
            if keystate[1]:
                sf.pressKey(self.g, sf.THRUST_KEY)
            else:
                sf.releaseKey(self.g, sf.THRUST_KEY)
            if self.gametype == "explode":
                if keystate[2]:
                    sf.pressKey(self.g, sf.LEFT_KEY)
                else:
                    sf.releaseKey(self.g, sf.LEFT_KEY)
                if keystate[3]:
                    sf.pressKey(self.g, sf.RIGHT_KEY)
                else:
                    sf.releaseKey(self.g, sf.RIGHT_KEY)
        sf.stepOneTick(self.g, self.tickdur)
        reward = self.g.contents.reward
        sf.drawGameStateScaled(self.g, self.pb, self.scale, self.ls)
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
        self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)
        done = sf.isGameOver(self.g)
        if self.videofile:
            self.video.write(self.game_gray_rgb)
            if done:
                self.video.release()
                cv2.destroyAllWindows()
                self.video = None
        self.last_action = action
        if self.obs_type == 'image':
            return self.game_state, reward, done, {}
        else:
            return self._get_features(), reward, done, {}
