from gym.envs.registration import register

register(
	id='SpaceFortress-explode-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'explode', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-autoturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'autoturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-explode-features-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'nopenalty-explode', 'obs_type': 'normalized-features' },
	nondeterministic=False
)

register(
	id='SpaceFortress-autoturn-features-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'nopenalty-autoturn', 'obs_type': 'normalized-features' },
	nondeterministic=False
)

register(
	id='SpaceFortress-slowautoturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'slow-autoturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-slowexplode-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'slow-explode', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-nopenaltyexplode-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'nopenalty-explode', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-nopenaltyautoturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env',
	kwargs={ 'gametype': 'nopenalty-autoturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-testexplode-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env_Test',
	kwargs={ 'gametype': 'explode', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-testautoturn-image-v0',
	entry_point='spacefortress.gym.envs:SSF_Env_Test',
	kwargs={ 'gametype': 'autoturn', 'obs_type': 'image' },
	nondeterministic=False
)