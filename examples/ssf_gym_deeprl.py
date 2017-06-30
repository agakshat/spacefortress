from __future__ import division
import argparse

from PIL import Image
import numpy as np
import gym

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten, Conv2D, Permute
from keras.optimizers import Adadelta
import keras.backend as K

from rl.agents.dqn import DQNAgent, NAFAgent
from rl.agents import SARSAAgent
from rl.policy import Policy, LinearAnnealedPolicy, BoltzmannQPolicy, EpsGreedyQPolicy
from rl.memory import SequentialMemory
from rl.core import Processor
from rl.callbacks import FileLogger, ModelIntervalCheckpoint

import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)),"../"))

import ssf
import ssf.gym

import cv2
import numpy as np

INPUT_SHAPE = (84, 84)
WINDOW_LENGTH = 8

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

    def get_current_value(self):
        if self.agent.training:
            # Linear annealed: f(x) = ax + b.
            a = -float(self.value_max - self.value_min) / float(self.nb_epochs)
            b = float(self.value_max)
            value = max(self.value_min, a * float(self.agent.epoch-1) + b)
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
    def process_state_batch(self, batch):
        processed_batch = batch.astype('float32') / 255.
        return processed_batch

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('--mode', choices=["train","test"], default="train")
parser.add_argument('--weights', type=str, default=None)
parser.add_argument('--policy', choices=["eps","tau"], default="eps")
parser.add_argument('--algo', choices=["dqn","sarsa","naf"], default="dqn")
parser.add_argument('--actionset', choices=[0,1,2], default=2, type=int)
args = parser.parse_args()

# Get the environment and extract the number of actions.
env = ssf.gym.SSF_Env(gametype="explode", scale=.2, ls=3, action_set=args.actionset)
np.random.seed(123)
env.seed(123)
nb_actions = env.action_space.n

TRAIN_INTERVAL = int(np.ceil(5 / env.tickdur))

# Next, we build our model. We use the same model that was described by Mnih et al. (2015).
if args.algo == "sarsa":
    WINDOW_LENGTH = 1
input_shape = (WINDOW_LENGTH,) + INPUT_SHAPE
model = Sequential()
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
print(model.summary())

# Finally, we configure and compile our agent. You can use every built-in Keras optimizer and
# even the metrics!
memory = SequentialMemory(limit=1000000, window_length=WINDOW_LENGTH)
processor = SSFProcessor()

# Select a policy. We use eps-greedy action selection, which means that a random action is selected
# with probability eps. We anneal eps from 1.0 to 0.1 over the course of 1M steps. This is done so that
# the agent initially explores the environment (high eps) and then gradually sticks to what it knows
# (low eps). We also set a dedicated eps value that is used during testing. Note that we set it to 0.05
# so that the agent still performs some random actions. This ensures that the agent cannot get stuck.
if args.policy == "eps":
    policy = EpochLinearAnnealedPolicy(EpsGreedyQPolicy(), attr='eps', value_max=1, value_min=.1, value_test=.01, nb_epochs=100)

# The trade-off between exploration and exploitation is difficult and an on-going research topic.
# If you want, you can experiment with the parameters or use a different policy. Another popular one
# is Boltzmann-style exploration:
elif args.policy == "tau":
    policy = EpochLinearAnnealedPolicy(BoltzmannQPolicy(), attr='tau', value_max=10, value_min=1, value_test=.1, nb_epochs=100)
# Feel free to give it a try!

if args.algo == "dqn":
    agent = DQNAgent(model=model, nb_actions=nb_actions, policy=policy, test_policy=policy, memory=memory,
                   processor=processor, nb_steps_warmup=50000, gamma=.99, target_model_update=10000,
                   train_interval=4)
elif args.algo == "sarsa":
    agent = SARSAAgent(model=model, nb_actions=nb_actions, nb_steps_warmup=10, policy=policy, test_policy=policy, train_interval=4)

agent.compile(Adadelta(lr=1), metrics=['mae'])

# Okay, now it's time to learn something! We capture the interrupt exception so that training
# can be prematurely aborted. Notice that you can the built-in Keras callbacks!
weights_filename = 'ssf_%s_%s_weights.h5f' % (args.algo, args.policy)
log_filename = 'ssf_%s_%s_log.json' % (args.algo, args.policy)
callbacks = []

if args.mode == 'train':
    while True:
        env.videofile = None
        agent.fit(env, callbacks=callbacks, nb_max_start_steps=30, nb_steps=EPISODE_LENGTH*10, log_interval=10000, verbose=2, action_repetition=2, nb_max_episode_steps=EPISODE_LENGTH)
        agent.nb_steps_warmup = 0
        # After training is done, we save the final weights one more time.
        agent.save_weights(weights_filename, overwrite=True)
        # Finally, evaluate our algorithm for 10 episodes.
        env.videofile = 'ssf_%s_%s_epoch-%d' % (args.algo, args.policy, agent.epoch)
        env.videofile2 = 'ssf_%s_%s_latest.avi' % (args.algo, args.policy)
        agent.test(env, nb_episodes=1, visualize=False)
elif args.mode == 'test':
    if args.weights:
        weights_filename = args.weights
        agent.load_weights(weights_filename)
    env.videofile = 'ssf_%s_%s_test' % (args.algo, args.policy)
    env.videofile2 = None
    agent.test(env, nb_episodes=1, visualize=True)
