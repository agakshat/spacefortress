from gym.scoreboard.registration import add_task, add_group

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
