from __future__ import division
import argparse

from PIL import Image
import numpy as np
import gym

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten, Conv2D, Permute
from keras.optimizers import Adam
import keras.backend as K

from rl.agents.dqn import DQNAgent
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

EPISODE_LENGTH = int(5295/2)

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
args = parser.parse_args()

# Get the environment and extract the number of actions.
env = ssf.gym.SSF_Env(gametype="explode", scale=.2, ls=3)
np.random.seed(123)
env.seed(123)
nb_actions = env.action_space.n

# Next, we build our model. We use the same model that was described by Mnih et al. (2015).
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
policy = EpochLinearAnnealedPolicy(EpsGreedyQPolicy(), attr='eps', value_max=1, value_min=.1, value_test=.01, nb_epochs=1000)

# The trade-off between exploration and exploitation is difficult and an on-going research topic.
# If you want, you can experiment with the parameters or use a different policy. Another popular one
# is Boltzmann-style exploration:
# policy = BoltzmannQPolicy(tau=1.)
# Feel free to give it a try!

dqn = DQNAgent(model=model, nb_actions=nb_actions, policy=policy, test_policy=policy, memory=memory,
               processor=processor, nb_steps_warmup=50000, gamma=.99, target_model_update=10000,
               train_interval=4, delta_clip=1.)
dqn.compile(Adam(lr=.00015), metrics=['mae'])

# Okay, now it's time to learn something! We capture the interrupt exception so that training
# can be prematurely aborted. Notice that you can the built-in Keras callbacks!
weights_filename = 'ssf_dqn_weights.h5f'
log_filename = 'ssf_dqn_log.json'
callbacks = []

if args.mode == 'train':
    while True:
        env.videofile = None
        dqn.fit(env, callbacks=callbacks, nb_steps=EPISODE_LENGTH*10, log_interval=10000, verbose=2, action_repetition=2, nb_max_episode_steps=EPISODE_LENGTH)
        dqn.nb_steps_warmup = 0
        # After training is done, we save the final weights one more time.
        dqn.save_weights(weights_filename, overwrite=True)
        # Finally, evaluate our algorithm for 10 episodes.
        env.videofile = 'ssf_dqn_epoch-%d' % dqn.epoch
        dqn.test(env, nb_episodes=1, visualize=False)
elif args.mode == 'test':
    if args.weights:
        weights_filename = args.weights
    dqn.load_weights(weights_filename)
    env.videofile = 'ssf_dqn_test'
    dqn.test(env, nb_episodes=1, visualize=False)
