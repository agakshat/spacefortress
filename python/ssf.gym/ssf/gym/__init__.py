from gym.scoreboard.registration import add_task, add_group
from gym.envs.registration import register
from .ssf_env import SSF_Env

register(
   	id='SpaceFortress-explode-v0',
   	entry_point='gym.envs.spacefortress:SSF_Env',
    kwargs={ 'gametype': 'explode' },
    nondeterministic=False,
)

register(
   	id='SpaceFortress-autoturn-v0',
   	entry_point='gym.envs.spacefortress:SSF_Env',
    kwargs={ 'gametype': 'autoturn' },
    nondeterministic=False,
)

add_group(
    id= 'SpaceFortress',
    name= 'SpaceFortress',
    description= '32 levels of the original Super Mario Bros game.'
)

add_task(
   	id='SpaceFortress-explode-v0',
   	summary="2D frictionless space shooter (explode variant)",
   	group='SpaceFortress',
   	contributor='Ryan M. Hope',
)

add_task(
   	id='SpaceFortress-autoturn-v0',
   	summary="2D frictionless space shooter (autoturn variant)",
   	group='SpaceFortress',
   	contributor='Ryan M. Hope',
)
