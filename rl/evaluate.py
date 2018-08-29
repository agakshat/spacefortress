import argparse
import os
import types

import numpy as np
import torch
from networks import ACNet
import gym_vecenv
import spacefortress.gym
import gym
from envs import make_env
import pdb
import time

parser = argparse.ArgumentParser(description='RL')
parser.add_argument('--seed', type=int, default=1,
                    help='random seed (default: 1)')
parser.add_argument('--num-stack', type=int, default=4,
                    help='number of frames to stack (default: 4)')
parser.add_argument('--env-name', default='youturn',
                    help='environment to train on (default: youturn)')
parser.add_argument('--load-dir', default='./trained_models/',
                    help='directory to save agent logs (default: ./trained_models/)')
parser.add_argument('--steps', type=int, default=100000,
                    help='number of steps to run testing script (default: 100000)')
parser.add_argument('--render', type=int, default=0,
                    help='pass flag equal to 1 for rendering (default: 0)')
parser.add_argument('--deterministic', action='store_true', default=False,
                    help='deterministic flag')
parser.add_argument('--feedforward', action='store_true', default=False,
                    help='use feedforward instead of recurrent architecture')
args = parser.parse_args()

torch.set_num_threads(1)
if args.env_name == 'autoturn':
    args.env_name = 'SpaceFortress-testautoturn-image-v0'
if args.env_name == 'youturn':
    args.env_name = 'SpaceFortress-testyouturn-image-v0'
env = make_env(args.env_name, args.seed, 0)
env = gym_vecenv.DummyVecEnv([env])
action_dim = env.action_space.n

actorNet = ACNet(action_dim,args.feedforward)
actorState = torch.load(args.load_dir,map_location = lambda storage, loc: storage)
actorNet.load_state_dict(actorState)

render_func = env.envs[0].render
obs_shape = env.observation_space.shape
obs_shape = (obs_shape[0] * args.num_stack, *obs_shape[1:])
current_obs = torch.zeros(1, *obs_shape)
masks = torch.zeros(1, 1)
state_size = 1 if args.feedforward else 256
states = torch.zeros(1,state_size)

def update_current_obs(obs):
    shape_dim0 = env.observation_space.shape[0]
    obs = torch.from_numpy(obs).float()
    if args.num_stack > 1:
        current_obs[:, :-shape_dim0] = current_obs[:, shape_dim0:]
    current_obs[:, -shape_dim0:] = obs

if args.render:
    render_func('human')
    input("Press Enter to Continue")

obs = env.reset()
update_current_obs(obs)
ep_reward = 0
total_reward = 0
num_episodes = 0
fortress_destroyed = 0
total_fortress_destroyed = 0
shots_fired = 0
total_shots_fired = 0
max_reward = 0
for i in range(args.steps):
    try:
        with torch.no_grad():
            value, action, _, states = actorNet.act(current_obs,states,masks,
                                            deterministic=args.deterministic)
        cpu_actions = action.squeeze(1).cpu().numpy()
        obs, reward, done, info = env.step(cpu_actions)
        fortress_destroyed += sum(info)
        shots_fired += cpu_actions==1
        ep_reward += reward
        if done:
            num_episodes+=1
            total_reward+=ep_reward
            max_reward = max(max_reward,ep_reward)
            total_fortress_destroyed += fortress_destroyed
            total_shots_fired += shots_fired
            print("Episode Reward: |Last {} | Average {} || Fortress: |Last {} | Average {:.3f} || Shots: |Last {} | Average {:.3f} ".
                    format(ep_reward,total_reward/num_episodes,fortress_destroyed,
                            total_fortress_destroyed/num_episodes,shots_fired,
                            total_shots_fired[0]/num_episodes))
            print("Max Reward: ",max_reward)
            ep_reward = 0
            fortress_destroyed = 0
            shots_fired = 0
        masks.fill_(0.0 if done else 1.0)
        if current_obs.dim() == 4:
            current_obs *= masks.unsqueeze(2).unsqueeze(2)
        else:
            current_obs *= masks
        update_current_obs(obs)
        if args.render:
            render_func('human')
            time.sleep(0.005)
    except KeyboardInterrupt:
        break
