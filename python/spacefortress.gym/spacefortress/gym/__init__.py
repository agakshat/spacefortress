from gym.envs.registration import register

register(
	id='SpaceFortress-youturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'youturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-autoturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'autoturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-testyouturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'test-youturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-testautoturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'test-autoturn', 'obs_type': 'image' },
	nondeterministic=False
)