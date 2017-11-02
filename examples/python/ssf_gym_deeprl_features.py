import os
import warnings

import numpy as np
import gym

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten, ELU
from keras.optimizers import Adadelta

from rl.agents.dqn import DQNAgent
from rl.policy import MaxBoltzmannQPolicy
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
            return processed_batch
        else:
            return batch

    def process_reward(self, reward):
        return reward/1000.

class SSFLogger(Callback):

    def __init__(self, agent, filename):
        self.agent = agent
        self.log = open(filename, "w")
        self.metrics = {}
        self.metrics_names = None

    def on_train_begin(self, logs):
        if self.metrics_names == None:
            header = [
                "training","epoch","episode","bigHexDeaths","smallHexDeaths",
                "shellDeaths","shipDeaths","resets","destroyedFortresses",
                "missedShots","totalShots","vlnerIncs","maxVlner","points","rawPoints"
            ] + self.model.metrics_names
            self.log.write("%s\n" % "\t".join(header))
            self.log.flush()
        self.metrics_names = self.model.metrics_names

    def on_episode_begin(self, episode, logs):
        self.metrics[episode] = []

    def on_episode_end(self, episode, logs={}):
        if self.agent.training:
            metrics = np.array(self.metrics[episode])
            metrics = [np.nanmean(metrics[:, idx]) if len(metrics[:, idx])>0 else float("nan") for idx in xrange(len(self.metrics_names))]
        else:
            metrics = [float("nan") for _ in xrange(len(self.metrics_names))]
        self.log.write("%s\n" % "\t".join(map(str,[self.agent.training,self.agent.epoch,self.agent.episode+1]+list(self.env.g.stats) + metrics)))
        self.log.flush()
        del self.metrics[episode]

    def on_step_end(self, step, logs):
        if self.agent.training:
            episode = logs['episode']
            self.metrics[episode].append(logs['metrics'])

# Get the environment and extract the number of actions.
env = SSF_Env(gametype="deep-explode", scale=.2, ls=2, obs_type="normalized_features", action_set=1)
np.random.seed(123)
env.seed(123)
nb_actions = env.action_space.n
nb_features = env.observation_space.shape[0]

# Set some variables
MODELNAME = "deepSF_%s" %(env.obs_type)
weightsfile = '%s.h5f' % (MODELNAME)
logfile = '%s.log' % (MODELNAME)

WINDOW_LENGTH = 4
FRAMESKIP = 2
EPISODES_IN_GAME = 5294
EPISODE_LENGTH = int(EPISODES_IN_GAME/FRAMESKIP)
MEMSIZE = 200 * EPISODES_IN_GAME

# Next, we build a very simple model.
model = Sequential()
model.add(Flatten(input_shape=(WINDOW_LENGTH,) + env.observation_space.shape))
model.add(Dense(32))
model.add(ELU())
model.add(Dense(32))
model.add(ELU())
model.add(Dense(32))
model.add(ELU())
model.add(Dense(nb_actions))
model.add(Activation('linear'))
print(model.summary())

memory = SequentialMemory(limit=MEMSIZE, window_length=WINDOW_LENGTH)
policy = MaxBoltzmannQPolicy(eps=.01, tau=1)
dqn = DQNAgent(model=model, nb_actions=nb_actions, memory=memory,
               nb_steps_warmup=1000, target_model_update=1e-2, policy=policy,
               enable_double_dqn=True, enable_dueling_network=False,
               processor = SSFProcessor(image=False))
dqn.compile(Adadelta(), metrics=['mae'])


if os.path.isfile(weightsfile):
    dqn.load_weights(weightsfile)

logger_cb = SSFLogger(dqn, logfile)

while True:
    dqn.fit(env, action_repetition=FRAMESKIP, nb_steps=10*EPISODE_LENGTH,
            visualize=True, verbose=2, callbacks=[logger_cb],
            nb_max_start_steps=16)
    dqn.nb_steps_warmup = 0
    dqn.test(env, nb_episodes=1, visualize=True, callbacks=[logger_cb])
    dqn.save_weights(weightsfile, overwrite=True)
