from collections import deque
import random
import atari_py
import torch
import cv2  # Note that importing cv2 before torch may cause segfaults?

"""
From https://github.com/ikostrikov/pytorch-a2c-ppo-acktr/blob/master/envs.py
"""

import gym
from gym.spaces.box import Box
import cv2, pdb
import numpy as np

class WrapPyTorch(gym.ObservationWrapper):
    def __init__(self, env=None, stack_size=4):
        super(WrapPyTorch, self).__init__(env)
        self.observation_space = Box(
            0,
            255,
            [1,84,84],dtype=np.uint8
        )
        self.stack_size = stack_size
        self.current_obs = np.zeros((self.stack_size,84,84))

    def observation(self, observation):
        x = cv2.resize(observation,(84,84),interpolation=cv2.INTER_AREA)
        y = np.expand_dims(x,0)
        if self.stack_size > 1:
          self.current_obs[:-1,:] = self.current_obs[1:,:]
        self.current_obs[-1,:] = y
        return self.current_obs