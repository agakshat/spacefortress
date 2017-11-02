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
import spacefortress.core as sf

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

    def __init__(self, gametype="deep-explode", scale=.2, viewport=(130,80,450,460), ls=3, action_set=0, continuous=False, obs_type='image'):
        assert obs_type in ('image', 'features', 'normalized_features', 'monitors')
        self.obs_type = obs_type
        self._seed()
        self.continuous = continuous
        self.viewer = None
        self.last_action = None
        self.gametype = gametype
        self.w = int(viewport[2] * scale)
        self.h = int(viewport[3] * scale)
        self.viewport = viewport
        self.ls = ls
        self.tickdur = int(np.ceil(1./self.metadata['video.frames_per_second']*1000))

        # 0=FIRE, 1=THRUST, 2=LEFT, 3=RIGHT
        if self.gametype in ["explode","deep-explode"]:
            if action_set == 0:
                self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1], [0, 1], [0, 1])).T.reshape(-1,4)
            elif action_set == 1:
                self.action_combinations = np.array([
                    [0, 0, 0, 0], # NOOP
                    [1, 0, 0, 0], # FIRE
                    [0, 1, 0, 0], # THRUST
                    [0, 0, 1, 0], # LEFT
                    [0, 0, 0, 1], # RIGHT
                ])
        elif self.gametype in ["autoturn","deep-autoturn"]:
            if action_set == 0:
                self.action_combinations = np.array(np.meshgrid([0, 1], [0, 1])).T.reshape(-1,2)
            elif action_set == 1:
                self.action_combinations = np.array([
                    [0, 0], # NOOP
                    [1, 0], # FIRE
                    [0, 1], # THRUST
                ])
        if self.continuous:
            if self.gametype in ["explode","deep-explode"]:
                # Action is 3 floats [fire, thrust, turn].
                # fire: -1..0 off, 0..+1 on
                # thrust: -1..0 off, 0..+1 on
                # turn:  -1.0..-0.5 left, +0.5..+1.0 right, -0.5..0.5 nothing
                self.action_space = spaces.Box(-1, +1, (3,))
            elif self.gametype in ["autoturn","deep-autoturn"]:
                # Action is 2 floats [fire, thrust].
                # fire: -1..0 off, 0..+1 on
                # thrust: -1..0 off, 0..+1 on
                self.action_space = spaces.Box(-1, +1, (2,))
        else:
            self.action_space = spaces.Discrete(len(self.action_combinations))

        self._reset()

    def _get_features(self):
        if self.obs_type == 'monitors':
            return np.array([
                0.5 if len(self.g.missiles) > 0 else -0.5,
                0.5 if self.g.fortress_alive else -0.5,
                0.5 if self.g.vulnerability > 10 else -0.5,
                0.5 if self.g.vulnerability > 10 and self.g.vulnerability_timer < self.g.vulnerability_time else -0.5,
                0.5 if self.g.aim < 3 else -0.5,
                0.5 if self.g.aim > 3 else -0.5,
                0.5 if self.g.ndist > .75 else -0.5,
                0.5 if self.g.ndist > .25 else -0.5,
                0.5 if self.g.ndist < -.25 else -0.5,
                0.5 if self.g.ndist < -.75 else -0.5
            ])
        elif self.obs_type == 'normalized_features':
            return np.clip(np.array([
                self.g.key_fire / self.max_ticks,
                self.g.key_thrust / self.max_ticks,
                self.g.key_left / self.max_ticks,
                self.g.key_right / self.max_ticks,
                1 if self.g.ship_alive else 0,
                #self.g.ship_x / self.g.pb_width,
                #self.g.ship_y / self.g.pb_height,
                self.g.ship_vx / 10,
                self.g.ship_vy / 10,
                #self.g.ship_angle / 360,
                self.g.aim / 360,
                self.g.vdir % 360 / 360,
                self.g.ndist,
                1 if self.g.fortress_alive else 0,
                #self.g.fortress_angle / 360,
                max(self.g.vulnerability, 10) / 10,
                1 if self.g.vulnerability > 10 and self.g.vulnerability_timer < self.g.vulnerability_time else 0,
                len(self.g.missiles) / sf.MAX_MISSILES,
                len(self.g.shells) / sf.MAX_SHELLS,
                #self.g.points / self.g.max_points
                ]), -1, 1)
        else:
            return np.array([
                self.g.key_fire,
                self.g.key_thrust,
                self.g.key_left,
                self.g.key_right,
                self.g.ship_alive,
                #self.g.ship_x,
                #self.g.ship_y,
                self.g.ship_vx,
                self.g.ship_vy,
                #self.g.ship_angle,
                self.g.aim,
                self.g.vdir,
                self.g.ndist,
                self.g.fortress_alive,
                #self.g.fortress_angle,
                self.g.vulnerability,
                1 if self.g.vulnerability > 10 and self.g.vulnerability_timer < self.g.vulnerability_time else 0,
                len(self.g.missiles),
                len(self.g.shells),
                #self.g.points
                ])

    def _seed(self, seed=None):
        self.np_random, seed = seeding.np_random(seed)
        return [seed]

    def _reset(self):
        self.g = sf.Game(self.gametype, width=self.w, height=self.h, viewport=self.viewport, lw=self.ls, grayscale=True)
        self.max_ticks = np.floor(self.g.max_time / self.tickdur)
        self.raw_pixels = self.g.pb_pixels
        self.g.draw()
        if self.obs_type == 'image':
            self.observation_space = spaces.Box(low=0, high=255, shape=(self.g.pb_height, self.g.pb_width, 3))
            self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
            self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)
            return self.game_state
        else:
            self.observation_space = spaces.Box(low=-1, high=1, shape=self._get_features().shape)
            self.game_state = np.array([])
            return self._get_features()

    def _render(self, mode='human', close=False):
        if close:
            if self.viewer is not None:
                self.viewer.close()
                self.viewer = None
            return

        if self.viewer is None:
            self.viewer = SSF_Viewer()

        if self.obs_type != 'image' and self.game_state.shape == (0,):
            self.g.draw()
            self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
            self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)

        self.viewer.imshow(self.game_state)

        if mode == 'rgb_array':
            return self.game_gray_rgb

    def _destroy(self):
        pass

    def _draw(self):
        self.g.draw()
        self.game_state = cv2.cvtColor(np.fromstring(self.raw_pixels, np.uint8).reshape(self.h, self.w, 4), cv2.COLOR_RGBA2GRAY)
        self.game_gray_rgb = cv2.cvtColor(self.game_state,cv2.COLOR_GRAY2RGB)

    def _step(self, action):
        done = False
        reward = 0
        if self.continuous:
            if action[0] > 0:
                self.g.press_key(sf.FIRE_KEY)
            else:
                self.g.release_key(sf.FIRE_KEY)
            if action[1] > 0:
                self.g.press_key(sf.THRUST_KEY)
            else:
                self.g.release_key(sf.THRUST_KEY)
            if self.gametype in ["explode","deep-explode"]:
                if action[2] < -0.5:
                    if self.last_action != None and self.last_action > 0.5:
                        self.g.release_key(sf.RIGHT_KEY)
                    self.g.press_key(sf.LEFT_KEY)
                elif action[2] > 0.5:
                    if self.last_action != None and self.last_action < -0.5:
                        self.g.release_key(sf.LEFT_KEY)
                    self.g.press_key(sf.RIGHT_KEY)
                else:
                    if self.last_action != None and self.last_action < -0.5:
                        self.g.release_key(sf.LEFT_KEY)
                    elif self.last_action != None and self.last_action > 0.5:
                        self.g.release_key(sf.RIGHT_KEY)
        else:
            keystate = self.action_combinations[action]
            if keystate[0]:
                self.g.press_key(sf.FIRE_KEY)
            else:
                self.g.release_key(sf.FIRE_KEY)
            if keystate[1]:
                self.g.press_key(sf.THRUST_KEY)
            else:
                self.g.release_key(sf.THRUST_KEY)
            if self.gametype in ["explode","deep-explode"]:
                if keystate[2]:
                    self.g.press_key(sf.LEFT_KEY)
                else:
                    self.g.release_key(sf.LEFT_KEY)
                if keystate[3]:
                    self.g.press_key(sf.RIGHT_KEY)
                else:
                    self.g.release_key(sf.RIGHT_KEY)

        reward = self.g.step_one_tick(self.tickdur)
        done = self.g.is_game_over()
        self.last_action = action
        if self.obs_type == 'image':
            self._draw()
            return self.game_state, reward, done, {}
        else:
            self.game_state = np.array([])
            return self._get_features(), reward, done, {}
