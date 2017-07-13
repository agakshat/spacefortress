from __future__ import division
import argparse

from PIL import Image
import numpy as np
import gym
from gym import wrappers

import tempfile

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten, Conv2D, Permute
from keras.optimizers import Adadelta
import keras.backend as K

from rl.agents.dqn import DQNAgent
from rl.agents import SARSAAgent
from rl.policy import Policy, LinearAnnealedPolicy, BoltzmannQPolicy, EpsGreedyQPolicy
from rl.memory import SequentialMemory
from rl.core import Processor
from rl.callbacks import FileLogger, ModelIntervalCheckpoint

import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)),"../"))

import spacefortress as sf
import spacefortress.gym

import cv2
import numpy as np

INPUT_SHAPE = (84, 84)
WINDOW_LENGTH = 4

FRAMESKIP = 2
EPISODES_IN_GAME = 5294
EPISODE_LENGTH = int(EPISODES_IN_GAME/FRAMESKIP)
MEMSIZE = 200 * EPISODES_IN_GAME

class EpochLinearAnnealedPolicy(Policy):
    def __init__(self, inner_policy, attr, value_max, value_min, value_test, nb_epochs):
        if not hasattr(inner_policy, attr):
            raise ValueError('Policy "{}" does not have attribute "{}".'.format(attr))

        super(EpochLinearAnnealedPolicy, self).__init__()

        self.inner_policy = inner_policy
        self.attr = attr
        self.value_max = value_max
        self.value_min = value_min
        self.value_test = value_test
        self.nb_epochs = nb_epochs
        self.epoch = 0

    def reset(self):
        self.epoch = self.agent.epoch

    def get_current_value(self):
        if self.agent.training:
            # Linear annealed: f(x) = ax + b.
            a = -float(self.value_max - self.value_min) / float(self.nb_epochs)
            b = float(self.value_max)
            value = max(self.value_min, a * float(self.agent.epoch-1-self.epoch) + b)
        else:
            value = self.value_test
        # print(self.value_min, value, self.value_max)
        return value

    def select_action(self, **kwargs):
        setattr(self.inner_policy, self.attr, self.get_current_value())
        return self.inner_policy.select_action(**kwargs)

    @property
    def metrics_names(self):
        return ['mean_{}'.format(self.attr)]

    @property
    def metrics(self):
        return [getattr(self.inner_policy, self.attr)]

    def get_config(self):
        config = super(LinearAnnealedPolicy, self).get_config()
        config['attr'] = self.attr
        config['value_max'] = self.value_max
        config['value_min'] = self.value_min
        config['value_test'] = self.value_test
        config['nb_steps'] = self.nb_steps
        config['inner_policy'] = get_object_config(self.inner_policy)
        return config

class SSFProcessor(Processor):

    def __init__(self, image=True):
        self.image = image

    def process_state_batch(self, batch):
        if self.image:
            processed_batch = batch.astype('float32') / 255.
            return processed_batch
        else:
            return batch

    def process_reward(self, reward):
        return reward/1000.

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('--game', choices=["explode","autoturn"], default=["explode"], help="Game type.", nargs=1)
parser.add_argument('--obstype', choices=["image","features"], default="image")
parser.add_argument('--weights', type=str, default=None)
parser.add_argument('--episodes', type=int, default=10)
parser.add_argument('--epochs', type=int, default=100)
parser.add_argument('--policy', choices=["eps","tau"], default="eps")
parser.add_argument('--algo', choices=["dqn","sarsa"], default="dqn")
parser.add_argument('--actionset', choices=[0,1,2], default=0, type=int)
parser.add_argument('--visualize', action='store_true')
args = parser.parse_args()
args.game = args.game[0]

# Get the environment and extract the number of actions.
env_name = 'SpaceFortress-{}-{}-v0'.format(args.gametype, args.obstype)
env = gym.make(env_name)
np.random.seed(123)
env.seed(123)
nb_actions = env.action_space.n

TRAIN_INTERVAL = int(np.ceil(5 / env.tickdur))

if args.algo == "sarsa":
    WINDOW_LENGTH = 1
input_shape = (WINDOW_LENGTH,) + INPUT_SHAPE

model = Sequential()
if args.obstype == "image":
    processor = SSFProcessor(image=True)
    # Next, we build our model. We use the same model that was described by Mnih et al. (2015).
    if K.image_dim_ordering() == 'tf':
        # (width, height, channels)
        model.add(Permute((2, 3, 1), input_shape=input_shape))
    elif K.image_dim_ordering() == 'th':
        # (channels, width, height)
        model.add(Permute((1, 2, 3), input_shape=input_shape))
    else:
        raise RuntimeError('Unknown image_dim_ordering.')
    model.add(Conv2D(32, (8, 8), strides=(4, 4)))
    model.add(Activation('relu'))
    model.add(Conv2D(64, (4, 4), strides=(2, 2)))
    model.add(Activation('relu'))
    model.add(Conv2D(64, (3, 3), strides=(1, 1)))
    model.add(Activation('relu'))
    model.add(Flatten())
    model.add(Dense(512))
    model.add(Activation('relu'))
    model.add(Dense(nb_actions))
    model.add(Activation('linear'))
else:
    processor = SSFProcessor(image=False)
    model.add(Flatten(input_shape=(WINDOW_LENGTH, ) + env.observation_space.shape))
    model.add(Dense(128))
    model.add(Activation('relu'))
    model.add(Dense(128))
    model.add(Activation('relu'))
    model.add(Dense(128))
    model.add(Activation('relu'))
    model.add(Dense(128))
    model.add(Activation('relu'))
    model.add(Dense(nb_actions))
    model.add(Activation('linear'))
print(model.summary())

memory = SequentialMemory(limit=1000000, window_length=WINDOW_LENGTH)

if args.policy == "eps":
    policy = LinearAnnealedPolicy(EpsGreedyQPolicy(), attr='eps', value_max=1, value_min=.01, value_test=.01, nb_steps=EPISODE_LENGTH*args.episodes)
elif args.policy == "tau":
    policy = BoltzmannQPolicy()#EpochLinearAnnealedPolicy(BoltzmannQPolicy(), attr='tau', value_max=1, value_min=.1, value_test=.01, nb_epochs=10)

if args.algo == "dqn":
    agent = DQNAgent(model=model, nb_actions=nb_actions, policy=policy, test_policy=policy, memory=memory,
                   processor=processor, nb_steps_warmup=50000, gamma=.99, target_model_update=10000,
                   train_interval=4)
elif args.algo == "sarsa":
    agent = SARSAAgent(model=model, nb_actions=nb_actions, nb_steps_warmup=10, policy=policy, test_policy=policy, train_interval=4)

agent.compile(Adadelta(lr=1), metrics=['mae'])

weights_filename = 'ssf_%s_%s_%s_weights.h5f' % (args.gametype, args.algo, args.policy)
log_filename = 'ssf_%s_%s_%s_log.json' % (args.gametype, args.algo, args.policy)
callbacks = []

tmp_dir = tempfile.mkdtemp(prefix='{}-'.format(env_name))
env_mon = wrappers.Monitor(env, tmp_dir)

for e in xrange(args.epochs):
    # if args.policy == "eps" and e % 10 == 0:
    #     policy.reset()
    # env.videofile = None
    agent.fit(env, callbacks=callbacks, nb_max_start_steps=30, nb_steps=EPISODE_LENGTH*args.episodes, log_interval=10000, verbose=2, action_repetition=2, nb_max_episode_steps=EPISODE_LENGTH, visualize=args.visualize)
    agent.nb_steps_warmup = 0
    agent.save_weights(weights_filename, overwrite=True)
    agent.test(env_mon, nb_episodes=1, visualize=True)
    policy.value_max = policy.value_max * .99
