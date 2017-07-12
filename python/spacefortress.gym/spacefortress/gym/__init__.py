from gym.scoreboard.registration import add_task, add_group
from gym.envs.registration import register
from .ssf_env import SSF_Env

register(
	id='SpaceFortress-explode-image-v0',
	entry_point='spacefortress.gym:SSF_Env',
	kwargs={ 'gametype': 'explode', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-autoturn-image-v0',
	entry_point='spacefortress.gym:SSF_Env',
	kwargs={ 'gametype': 'autoturn', 'obs_type': 'image' },
	nondeterministic=False
)

register(
	id='SpaceFortress-explode-features-v0',
	entry_point='spacefortress.gym:SSF_Env',
	kwargs={ 'gametype': 'explode', 'obs_type': 'features' },
	nondeterministic=False
)

register(
	id='SpaceFortress-autoturn-features-v0',
	entry_point='spacefortress.gym:SSF_Env',
	kwargs={ 'gametype': 'autoturn', 'obs_type': 'features' },
	nondeterministic=False
)

add_group(
	id= 'SpaceFortress',
	name= 'SpaceFortress',
	description= 'SpaceFortress games'
)

add_task(
	id='SpaceFortress-explode-image-v0',
	summary="2D frictionless space shooter (explode, image)",
	group='SpaceFortress',
	contributor='Ryan M. Hope'
)

add_task(
	id='SpaceFortress-autoturn-image-v0',
	summary="2D frictionless space shooter (autoturn, image)",
	group='SpaceFortress',
	contributor='Ryan M. Hope'
)

add_task(
	id='SpaceFortress-explode-features-v0',
	summary="2D frictionless space shooter (explode, features)",
	group='SpaceFortress',
	contributor='Ryan M. Hope'
)

add_task(
	id='SpaceFortress-autoturn-features-v0',
	summary="2D frictionless space shooter (autoturn, features)",
	group='SpaceFortress',
	contributor='Ryan M. Hope'
)
