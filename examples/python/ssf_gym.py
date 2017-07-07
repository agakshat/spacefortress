import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)),"../"))

import argparse

import ssf
import ssf.gym

import timeit

if __name__ == '__main__':

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--scale', type=float, default=1)
    parser.add_argument('--visualize', action='store_true')
    args = parser.parse_args()

    env = ssf.gym.SSF_Env(gametype="explode", scale=args.scale, ls=2)
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
