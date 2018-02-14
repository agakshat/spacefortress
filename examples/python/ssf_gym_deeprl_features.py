from __future__ import division

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
        return reward/10.

class SSFLogger(Callback):

    def __init__(self, agent, filename):
        self.agent = agent
        self.log = open(filename, "w")
        self.metrics = {}
        self.metrics_names = None

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

    def on_episode_end(self, episode, logs={}):
        if self.agent.training:
            metrics = np.array(self.metrics[episode])
            metrics = [np.nanmean(metrics[:, idx]) if len(metrics[:, idx])>0 else float("nan") for idx in xrange(len(self.metrics_names))]
        else:
            metrics = [float("nan") for _ in xrange(len(self.metrics_names))]
        self.log.write("%s\n" % "\t".join(map(str,[self.agent.training,self.agent.epoch,self.agent.episode+1]+list(self.env.g.stats) + self.get_durations() + self.get_action_p() + metrics)))
        self.log.flush()
        del self.metrics[episode]

    def on_step_end(self, step, logs):
        if self.agent.training:
            episode = logs['episode']
            self.metrics[episode].append(logs['metrics'])

parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
parser.add_argument('--game', choices=["deep-explode","deep-autoturn","explode","autoturn"], default="deep-explode")
parser.add_argument('--action_set', type=int, default=1)
parser.add_argument('--features', choices=["features","normalized-features","monitors"], default="normalized-features")
parser.add_argument('--optimizer', choices=["adam","adadelta","nadam","rmsprop"], default="adam")
parser.add_argument('--policy', choices=["epsgreedy","boltzmann","maxboltzmann"], default="epsgreedy")
parser.add_argument('--learningrate', type=float, default=-1)
parser.add_argument('--windowlength', type=int, default=4)
parser.add_argument('--traininterval', type=int, default=4)
parser.add_argument('--memoryinterval', type=int, default=2)
parser.add_argument('--frameskip', type=int, default=2)
parser.add_argument('--memorysize', type=int, default=500)
parser.add_argument('--eps_max', type=float, default=.2)
parser.add_argument('--eps_min', type=float, default=.01)
parser.add_argument('--eps_test', type=float, default=.0001)
parser.add_argument('--tau_max', type=float, default=1)
parser.add_argument('--tau_min', type=float, default=.01)
parser.add_argument('--tau_test', type=float, default=.001)
parser.add_argument('--annealgames', type=int, default=500)
parser.add_argument('--ngames', type=int, default=1000)
parser.add_argument('--nepochs', type=int, default=20)
parser.add_argument('--seed', type=int, default=1234)
parser.add_argument('--visualize', action='store_true')
args = parser.parse_args()
print(args.game)

# Get the environment and extract the number of actions.
env = SSF_Env(gametype=args.game, scale=.2, ls=2, obs_type=args.features, action_set=args.action_set)
np.random.seed(args.seed)
env.seed(args.seed)
nb_actions = env.action_space.n
nb_features = env.observation_space.shape[0]

EPISODES_IN_GAME = 5294
EPISODE_LENGTH = int(EPISODES_IN_GAME/args.frameskip)
MEMSIZE = args.memorysize

# Next, we build a very simple model.
model = Sequential()
model.add(Flatten(input_shape=(args.windowlength,) + env.observation_space.shape))
model.add(Dense(64))
model.add(Activation('elu'))
model.add(Dense(64))
model.add(Activation('elu'))
model.add(Dense(64))
model.add(Activation('elu'))
model.add(Dense(nb_actions))
model.add(Activation('linear'))
print(model.summary())

memory = SequentialMemory(limit=MEMSIZE * EPISODES_IN_GAME, window_length=args.windowlength)

if args.policy == "epsgreedy":
    policy = EpsGreedyQPolicy(eps=args.eps_max)
elif args.policy == "boltzmann":
    policy = BoltzmannQPolicy(tau=args.tau_max)
elif args.policy == "maxboltzmann":
    policy = MaxBoltzmannQPolicy(eps=args.eps_max, tau=args.tau_max)

policy = LinearAnnealedPolicy(policy, attr='eps', value_max=args.eps_max, value_min=args.eps_min, value_test=args.eps_test, nb_steps=EPISODE_LENGTH*args.annealgames)

dqn = DQNAgent(model=model, nb_actions=nb_actions, memory=memory,
               nb_steps_warmup=EPISODE_LENGTH, target_model_update=EPISODE_LENGTH, policy=policy,
               enable_double_dqn=True, enable_dueling_network=False,
               #gamma=.8,
               #delta_clip=10,
               processor = SSFProcessor(image=False),
               train_interval=args.traininterval, memory_interval=args.memoryinterval)

lr = args.learningrate

if args.learningrate <= 0:
    if args.optimizer == "adam":
        lr = 1e-3
    elif args.optimizer == "nadam":
        lr = 2e-3
    elif args.optimizer == "adadelta":
        lr = 1e0
    elif args.optimizer == "rmsprop":
        lr = 1e-3

if args.optimizer == "adam":
    o = Adam(lr=lr)
elif args.optimizer == "nadam":
    o = Nadam(lr=lr)
elif args.optimizer == "adadelta":
    o = Adadelta(lr=lr)
elif args.optimizer == "rmsprop":
    o = RMSprop(lr=lr)

dqn.compile(o, metrics=['mae'])

# Set some variables
MODELNAME = "dqn_game:%s_obs:%s_pol:%s_opt:%s_lr:%.6f_wl:%d_ms:%d_fs:%d_ti:%d_mi:%d_as:%d_seed:%d" % (args.game, env.obs_type, args.policy, args.optimizer, float(K.get_value(o.lr)), args.windowlength, args.memorysize, args.frameskip, args.traininterval, args.memoryinterval, args.action_set, args.seed)
weightsfile = '%s.h5f' % (MODELNAME)
logfile = '%s.log' % (MODELNAME)

if os.path.isfile(weightsfile):
    dqn.load_weights(weightsfile)

logger_cb = SSFLogger(dqn, logfile)

for epoch in xrange(args.nepochs):
    start_eps = dqn.policy.value_max
    for _ in xrange(int(np.ceil(args.ngames/10))):
        epoch_eps = dqn.policy.value_max
        dqn.training = True
        dqn.fit(env, action_repetition=args.frameskip, nb_steps=10*EPISODE_LENGTH,
                visualize=args.visualize, verbose=2, callbacks=[logger_cb],
                nb_max_start_steps=env.metadata['video.frames_per_second']*4)
        dqn.nb_steps_warmup = 0
        dqn.test(env, nb_episodes=1, visualize=args.visualize, callbacks=[logger_cb])
        dqn.policy.value_max = epoch_eps * .99
    dqn.policy.value_max = start_eps * .75

dqn.save_weights(weightsfile, overwrite=True)
