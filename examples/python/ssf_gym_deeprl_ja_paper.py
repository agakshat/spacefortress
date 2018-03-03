from __future__ import division

import sys
import os
import warnings

import argparse

import numpy as np
import gym

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten, Dropout
from keras.optimizers import Adadelta, Adam, Nadam, RMSprop
from keras import backend as K

from rl.agents.dqn import DQNAgent
from rl.policy import MaxBoltzmannQPolicy, BoltzmannQPolicy, EpsGreedyQPolicy, LinearAnnealedPolicy
from rl.memory import SequentialMemory
from rl.core import Processor
from rl.callbacks import Callback

from spacefortress.gym.envs import SSF_Env

class SSFProcessor(Processor):

    def __init__(self, image=True):
        self.image = image

    def process_state_batch(self, batch):
        if self.image:
            processed_batch = batch.astype('float32') / 255.
        else:
            processed_batch = batch.astype('float32')
        return processed_batch

    def process_reward(self, reward):
        return np.clip(reward, -1., 1.)

def gen_mlp(input_shape, actions, nodes=64, layers=3, hidden_activation="relu", output_activation="linear"):
    model = Sequential()
    model.add(Flatten(input_shape=(WINDOWLENGTH,) + env.observation_space.shape))
    for _ in range(layers):
        model.add(Dense(nodes))
        model.add(Activation(hidden_activation))
    model.add(Dense(actions))
    model.add(Activation(output_activation))
    return model

class SSFLogger(Callback):

    def __init__(self, agent, filename):
        self.agent = agent
        self.log = open(filename, "w")
        self.metrics = {}
        self.metrics_names = None
        self.epoch = 0
        self.episode = 0

    def get_action_p(self):
        s = sum([self.env.actions_taken[i] for i in xrange(len(self.env.actions_taken))])
        return [self.env.actions_taken[i]/s for i in xrange(len(self.env.actions_taken))]

    def get_durations(self):
        return [
            np.nanmean(self.env.g.thrust_durations)*self.env.tickdur/1000,
            np.nanmean(self.env.g.shot_durations)*self.env.tickdur/1000,
            np.nanmean(self.env.g.shot_intervals_invul)*self.env.tickdur/1000,
            np.nanmean(self.env.g.shot_intervals_vul)*self.env.tickdur/1000
            ]

    def on_train_begin(self, logs):
        print("=====================================")
        print("Training Epoch %d" % self.epoch)
        print("=====================================")
        if self.metrics_names == None:
            header = [
                "training","epoch","episode","bigHexDeaths","smallHexDeaths",
                "shellDeaths","shipDeaths","resets","destroyedFortresses",
                "missedShots","totalShots","totalThrusts","totalLefts","totalRights",
                "vlnerIncs","maxVlner","points","rawPoints",
                "mean_thrust_duration","mean_shot_duration","mean_shot_interval_invul","mean_shot_interval_vul"
            ] + ["action%d_p" % (i) for i in xrange(len(self.env.actions_taken))] + self.model.metrics_names
            self.log.write("%s\n" % "\t".join(header))
            self.log.flush()
        self.metrics_names = self.model.metrics_names

    def on_episode_begin(self, episode, logs):
        self.metrics[episode] = []
        self.episode += 1

    def on_episode_end(self, episode, logs={}):
        print(self.env.g.stats)
        if self.agent.training:
            metrics = np.array(self.metrics[episode])
            metrics = [np.nanmean(metrics[:, idx]) if len(metrics[:, idx])>0 else float("nan") for idx in xrange(len(self.metrics_names))]
        else:
            metrics = [float("nan") for _ in xrange(len(self.metrics_names))]
        self.log.write("%s\n" % "\t".join(map(str,[self.agent.training, self.epoch, self.episode]+list(self.env.g.stats) + self.get_durations() + self.get_action_p() + metrics)))
        self.log.flush()
        del self.metrics[episode]

    def on_step_end(self, step, logs):
        if self.agent.training:
            episode = logs['episode']
            self.metrics[episode].append(logs['metrics'])

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('--game', choices=["deep-explode","deep-autoturn","explode","autoturn"], default="explode")
parser.add_argument('--seed', type=int, default=1234)
parser.add_argument('--mlp-hidden-units', type=int, default=64)
parser.add_argument('--mlp-layers', type=int, default=3)
parser.add_argument('--frameskip', type=int, default=2)
parser.add_argument('--memskip', type=int, default=2)
parser.add_argument('--visualize', action='store_true')
args = parser.parse_args()

WINDOWLENGTH = 4
EPISODES_IN_GAME = 5294
EPISODE_LENGTH = int(EPISODES_IN_GAME/args.frameskip)
MEMSIZE = 1000000
NSTEPS = 100000

# Get the environment and extract the number of actions.
env = SSF_Env(gametype=args.game, scale=.2, ls=2, obs_type="features", action_set=1)
np.random.seed(args.seed)
env.seed(args.seed)
nb_actions = env.action_space.n
nb_features = env.observation_space.shape[0]
input_shape = (WINDOWLENGTH,) + env.observation_space.shape

# Next, we build a very simple model.
model = gen_mlp(input_shape, nb_actions)
print(model.summary())

memory = SequentialMemory(limit=MEMSIZE, window_length=WINDOWLENGTH)

eps_start = 1

dqn = DQNAgent(model=model, nb_actions=nb_actions, memory=memory,
               nb_steps_warmup=EPISODE_LENGTH,
               target_model_update=int(env.metadata['video.frames_per_second']/args.frameskip*10),
               train_interval=1,
               enable_double_dqn=False, enable_dueling_network=False,
               processor = SSFProcessor(image=False),
               batch_size=32,  gamma=.999999, delta_clip=1.,
               memory_interval=args.memskip)

dqn.compile(RMSprop(), metrics=['mae'])

# Set some variables
MODELNAME = "dqn_game:%s" % (args.game)
weightsfile = '%s.h5f' % (MODELNAME)
logfile = '%s.log' % (MODELNAME)

if os.path.isfile(weightsfile):
    dqn.load_weights(weightsfile)

logger_cb = SSFLogger(dqn, logfile)

test_policy = LinearAnnealedPolicy(EpsGreedyQPolicy(), attr='eps', value_max=.05, value_min=.05, value_test=.05, nb_steps=NSTEPS)

for i in range(100):
    logger_cb.epoch += 1
    if i < 9:
        eps_start_new = eps_start - .1 * i
        eps_end_new = eps_start - .1 * (i+1)
        dqn.policy = LinearAnnealedPolicy(EpsGreedyQPolicy(), attr='eps', value_max=eps_start_new, value_min=eps_end_new, value_test=.05, nb_steps=NSTEPS)
    else:
        dqn.policy = LinearAnnealedPolicy(EpsGreedyQPolicy(), attr='eps', value_max=.1, value_min=.1, value_test=.1, nb_steps=NSTEPS)
    dqn.training = True
    dqn.fit(env, action_repetition=args.frameskip, nb_steps=NSTEPS,
            visualize=args.visualize, verbose=2, callbacks=[logger_cb],
            nb_max_start_steps=env.metadata['video.frames_per_second']*4)
    dqn.policy = test_policy
    dqn.test_policy = test_policy
    dqn.test(env, nb_episodes=10, visualize=True, callbacks=[logger_cb])
    dqn.nb_steps_warmup = 0
    dqn.save_weights(weightsfile, overwrite=True)
