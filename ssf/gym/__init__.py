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
        'render.modes': ['human','rgb_array'],
        'video.frames_per_second' : 30
    }

    def __init__(self, gametype="explode", scale=.2, ls=3, action_set=2, continuous=False):
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
            if action_set == 1:
                self.action_combinations = np.array([
                    [0, 0, 0, 0], # NOOP
                    [1, 0, 0, 0], # FIRE
                    [0, 1, 0, 0], # THRUST
                    [0, 0, 1, 0], # LEFT
                    [0, 0, 0, 1], # RIGHT
                    [1, 1, 0, 0], # FIRE & THRUST
                    [1, 0, 1, 0], # FIRE & LEFT
                    [1, 0, 0, 1], # FIRE & RIGHT
                ])
            if action_set == 2:
                self.action_combinations = np.array([
                    [0, 0, 0, 0], # NOOP
                    [1, 0, 0, 0], # FIRE
                    [0, 1, 0, 0], # THRUST
                    [0, 0, 1, 0], # LEFT
                    [0, 0, 0, 1], # RIGHT
                ])
            else:
                self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1], [0, 1], [0, 1])).T.reshape(-1,4)
            self.g = ssf.makeExplodeGame(grayscale=True)
        elif self.gametype == "autoturn":
            if action_set == 1:
                self.action_combinations = np.array([
                    [0, 0], # NOOP
                    [1, 0], # FIRE
                    [0, 1], # THRUST
                    [1, 1], # FIRE & THRUST
                ])
            if action_set == 2:
                self.action_combinations = np.array([
                    [0, 0], # NOOP
                    [1, 0], # FIRE
                    [0, 1], # THRUST
                ])
            else:
                self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1])).T.reshape(-1,2)
            self.g = ssf.makeAutoTurnGame(grayscale=True)
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
        self.videofile = None
        self.videofile2 = None
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
            self.video = cv2.VideoWriter(vf, cv2.VideoWriter_fourcc(*"H264"), self.metadata['video.frames_per_second'], (self.w,self.h))
            if self.videofile2!=None and platform.system() != "Windows":
                if os.path.exists(self.videofile2):
                    os.unlink(self.videofile2)
                os.symlink(vf, self.videofile2)
            self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)
            self.video.write(self.game_gray_rgb)
        return self.game_state

    def _render(self, mode='human', close=False):
        if close:
            if self.viewer is not None:
                self.viewer.close()
                self.viewer = None
            return

        from gym.envs.classic_control import rendering
        if self.viewer is None:
            self.viewer = rendering.SimpleImageViewer()

        self.viewer.imshow(self.game_gray_rgb)

        if mode == 'rgb_array':
            return self.game_gray_rgb

    def _destroy(self):
        pass

    def _step(self, action):
        done = False
        reward = 0
        if self.continuous:
            if action[0] > 0:
                ssf.pressKey(self.g, ssf.FIRE_KEY)
            else:
                ssf.releaseKey(self.g, ssf.FIRE_KEY)
            if action[1] > 0:
                ssf.pressKey(self.g, ssf.THRUST_KEY)
            else:
                ssf.releaseKey(self.g, ssf.THRUST_KEY)
            if self.gametype == "explode":
                if action[2] < -0.5:
                    if self.last_action != None and self.last_action > 0.5:
                        ssf.releaseKey(self.g, ssf.RIGHT_KEY)
                    ssf.pressKey(self.g, ssf.LEFT_KEY)
                elif action[2] > 0.5:
                    if self.last_action != None and self.last_action < -0.5:
                        ssf.releaseKey(self.g, ssf.LEFT_KEY)
                    ssf.pressKey(self.g, ssf.RIGHT_KEY)
                else:
                    if self.last_action != None and self.last_action < -0.5:
                        ssf.releaseKey(self.g, ssf.LEFT_KEY)
                    elif self.last_action != None and self.last_action > 0.5:
                        ssf.releaseKey(self.g, ssf.RIGHT_KEY)
        else:
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
            self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)
            self.video.write(self.game_gray_rgb)
            if done:
                self.video.release()
                cv2.destroyAllWindows()
                self.video = None
        self.last_action = action
        return self.game_state, reward, done, {}
