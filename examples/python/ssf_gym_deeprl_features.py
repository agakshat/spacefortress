import numpy as np
import gym

from keras.models import Sequential
from keras.layers import Dense, Activation, Flatten
from keras.optimizers import Adam

from rl.agents.dqn import DQNAgent
from rl.policy import BoltzmannQPolicy
from rl.memory import SequentialMemory

from spacefortress.gym.envs import SSF_Env

WINDOW_LENGTH = 16

# Get the environment and extract the number of actions.
env = SSF_Env(gametype="explode", scale=.2, ls=2, obs_type="normalized_features")
np.random.seed(123)
env.seed(123)
nb_actions = env.action_space.n
nb_features = env.observation_space.shape[0]

# Next, we build a very simple model.
model = Sequential()
model.add(Flatten(input_shape=(WINDOW_LENGTH,) + env.observation_space.shape))
model.add(Dense(nb_features))
model.add(Activation('relu'))
model.add(Dense(2*nb_features))
model.add(Activation('relu'))
model.add(Dense(2*nb_features))
model.add(Activation('relu'))
model.add(Dense(nb_actions))
model.add(Activation('linear'))
print(model.summary())

memory = SequentialMemory(limit=50000, window_length=WINDOW_LENGTH)
policy = BoltzmannQPolicy()
dqn = DQNAgent(model=model, nb_actions=nb_actions, memory=memory, nb_steps_warmup=10000,
               target_model_update=1e-2, policy=policy)
dqn.compile(Adam(lr=1e-3), metrics=['mae'])

dqn.fit(env, nb_steps=10000000, visualize=True, verbose=2)
