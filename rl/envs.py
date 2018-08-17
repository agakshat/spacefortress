"""
From https://github.com/ikostrikov/pytorch-a2c-ppo-acktr/blob/master/envs.py
"""

import gym
from gym.spaces.box import Box
import cv2
import numpy as np

def make_env(env_id, seed, rank):
    def _thunk():
        env = gym.make(env_id)
        env.seed(seed + rank)
        env = WrapPyTorch(env)
        return env
    return _thunk


class WrapPyTorch(gym.ObservationWrapper):
    def __init__(self, env=None):
        super(WrapPyTorch, self).__init__(env)
        self.observation_space = Box(
            0,
            255,
            [1,84,84],dtype=np.uint8
        )

    def observation(self, observation):
        x = cv2.resize(observation,(84,84),interpolation=cv2.INTER_AREA)
        return np.expand_dims(x,0)
