import argparse
import timeit
import tempfile
import gym
from gym import wrappers
import numpy as np

import spacefortress.gym

if __name__ == '__main__':

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--gametype', choices=["explode","autoturn"], default="explode")
    parser.add_argument('--obstype', choices=["image","features"], default="image")
    parser.add_argument('--visualize', action='store_true')
    args = parser.parse_args()

    env_name = 'SpaceFortress-{}-{}-v0'.format(args.gametype, args.obstype)
    env = gym.make(env_name)
    tmp_dir = tempfile.mkdtemp(prefix='{}-'.format(env_name))
    print(tmp_dir)
    env = wrappers.Monitor(env, tmp_dir)
    state = env.reset()
    done = False
    start = timeit.default_timer()
    while not done:
        action = env.action_space.sample()
        state, r, done, _ = env.step(action)
        if args.visualize:
            env.render()
    stop = timeit.default_timer()
    print stop - start
