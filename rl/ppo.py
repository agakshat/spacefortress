from __future__ import absolute_import, division, print_function
import numpy as np
import torch
import torch.optim as optim
from networks import ACNet
from copy import deepcopy
import gym, pdb, os, gym_vecenv, time, spacefortress.gym
from arguments import get_args
from envs import make_env
from storage import RolloutStorage
from tensorboardX import SummaryWriter

class PPOAgent(object):
	def __init__(self,args):
		self.args = args
		self.device = torch.device('cuda') if args.cuda else torch.device('cpu')
		dummy_env = gym.make(self.args.env_name)
		self.actor = ACNet(dummy_env.action_space.n,args.feedforward)
		del dummy_env
		if args.load_dir is not None:
			actorState = torch.load(args.load_dir,map_location=lambda storage, loc: storage)
		if args.continue_training:
			self.actor.load_state_dict(actorState)
			print("Loaded pretrained model successfully")
		if args.transfer:
			self.actor.load_autoturn_model(actorState)
		if args.cuda:
			self.actor.cuda()
		self.actor_optimizer = optim.Adam(self.actor.parameters(),lr=self.args.lr)
		self.env_list = [make_env(self.args.env_name,self.args.seed,i) for i in range(self.args.num_processes)]
		if self.args.num_processes > 1:
			self.envs = gym_vecenv.SubprocVecEnv(self.env_list)
		else:
			self.envs = gym_vecenv.DummyVecEnv(self.env_list)
		if len(self.envs.observation_space.shape) == 1:
			self.envs = gym_vecenv.VecNormalize(self.envs)
		
		self.obs_shape = self.envs.observation_space.shape
		self.obs_shape = (self.obs_shape[0] * args.num_stack, *self.obs_shape[1:])
		self.state_shape = 1 if args.feedforward else 256
		self.rollouts = RolloutStorage(self.args.num_fwd_steps, self.args.num_processes, self.obs_shape, self.envs.action_space, self.state_shape)
		self.num_updates = int(args.num_frames)//args.num_fwd_steps//args.num_processes
		self.current_obs = torch.zeros(self.args.num_processes,*self.obs_shape)
		self.writer = SummaryWriter(log_dir=self.args.save_dir)
		self.fortress_threshold = 650
		self.scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(self.actor_optimizer,
											mode='max',factor=0.2,patience=15,verbose=True,threshold=1e-3,
											threshold_mode='rel')
		#self.scheduler2 = torch.optim.lr_scheduler.MultiStepLR(self.actor_optimizer,milestones=[40,80],gamma=0.3)
	
	def update_current_obs(self,obs):
		shape_dim0 = self.envs.observation_space.shape[0]
		obs = torch.from_numpy(obs).float()
		if self.args.num_stack > 1:
				self.current_obs[:, :-shape_dim0] = self.current_obs[:, shape_dim0:]
		self.current_obs[:, -shape_dim0:] = obs

	def train(self):

		obs = self.envs.reset()
		self.update_current_obs(obs)
		self.rollouts.observations[0].copy_(self.current_obs)

		episode_rewards = torch.zeros([self.args.num_processes,1])
		final_rewards = torch.zeros([self.args.num_processes,1])
		num_destruction = 0
		if self.args.cuda:
			self.current_obs = self.current_obs.cuda()
			self.rollouts.cuda()

		start = time.time()
		num_episodes = 0
		for iteration in range(self.num_updates):
			for step in range(self.args.num_fwd_steps):
				with torch.no_grad():
					value,action,action_log_prob,states = self.actor.act(self.rollouts.observations[step],
																															self.rollouts.states[step],
																															self.rollouts.masks[step])
				cpu_actions = action.squeeze(1).cpu().numpy()
				obs,reward,done,info = self.envs.step(cpu_actions)
				num_destruction += sum(info)
				reward = torch.from_numpy(np.expand_dims(np.stack(reward),1)).float()

				episode_rewards += reward
				masks = torch.FloatTensor([[0.0] if i else [1.0] for i in done])
				final_rewards*=masks
				final_rewards += (1-masks)*episode_rewards
				episode_rewards *= masks

				if self.args.cuda:
					masks = masks.cuda()
				if self.current_obs.dim() == 4:
					self.current_obs *= masks.unsqueeze(2).unsqueeze(2)
				else:
					self.current_obs *= masks

				self.update_current_obs(obs)
				self.rollouts.insert(step,self.current_obs,states,action,action_log_prob,value,reward,masks)

			with torch.no_grad():
				next_value = self.actor.get_value(self.rollouts.observations[-1],
																					self.rollouts.states[-1],
																					self.rollouts.masks[-1]).detach()
			
			self.rollouts.compute_returns(next_value,True,self.args.gamma,self.args.tau)
			advantages = self.rollouts.returns[:-1] - self.rollouts.value_preds[:-1]
			advantages = (advantages - advantages.mean())/(advantages.std()+1e-5)

			for i in range(self.args.ppo_epoch):
				if self.args.feedforward:
					data_generator = self.rollouts.feed_forward_generator(advantages,self.args.num_mini_batch)
				else:
					data_generator = self.rollouts.recurrent_generator(advantages,self.args.num_mini_batch)
				for sample in data_generator:
					observations_batch, states_batch, actions_batch, \
						return_batch, masks_batch, old_action_log_probs_batch, \
							adv_targ = sample
					values,action_log_probs,dist_entropy,states = self.actor.evaluate_actions(observations_batch,
																																										states_batch,
																																										masks_batch,
																																										actions_batch)
					ratio = torch.exp(action_log_probs - old_action_log_probs_batch)
					surr1 = ratio*adv_targ
					surr2 = torch.clamp(ratio,1.0-self.args.clip_param,1.0+self.args.clip_param)*adv_targ
					action_loss = -torch.min(surr1,surr2).mean()
					value_loss = (values-return_batch).pow(2).mean()
					self.actor_optimizer.zero_grad()
					actorLoss = action_loss + self.args.value_loss_coeff*value_loss - self.args.entropy_coeff*dist_entropy
					actorLoss.backward()
					torch.nn.utils.clip_grad_norm_(self.actor.parameters(),self.args.max_grad_norm)
					self.actor_optimizer.step()
			self.rollouts.after_update()

			if num_destruction>self.fortress_threshold:
				torch.save(self.actor.state_dict(),self.args.save_dir+'/'+self.env_name+'_'+str(iteration)+'_ppo_actor.pth.tar')
				self.fortress_threshold = num_destruction

			if iteration%self.args.log_interval == 0:
				end = time.time()
				total_num_steps = (iteration+1)*self.args.num_processes*self.args.num_fwd_steps
				num_destruction /= self.args.num_processes
				print("Updates {}, num timesteps {}, FPS {}, mean/median reward {:.1f}/{:.1f}, min/max reward {:.1f}/{:.1f}, entropy {:.5f}, value loss {:.5f}, policy loss {:.5f}, num fortress destroyed {:.2f}".
								format(iteration, total_num_steps,
											 int(total_num_steps / (end - start)),
											 final_rewards.mean(),
											 final_rewards.median(),
											 final_rewards.min(),
											 final_rewards.max(), dist_entropy.item(),
											 value_loss.item(), action_loss.item(),num_destruction))
				self.writer.add_scalar('data/rewardmean',final_rewards.mean(),total_num_steps)
				self.writer.add_scalar('data/distentropy',dist_entropy.item(),total_num_steps)
				self.writer.add_scalar('data/valueloss',value_loss.item(),total_num_steps)
				self.writer.add_scalar('data/actionloss',action_loss.item(),total_num_steps)
				self.writer.add_scalar('data/numdestruction',num_destruction,total_num_steps)
				self.scheduler.step(final_rewards.mean())
				#self.scheduler2.step()
				num_destruction = 0

			if iteration%self.args.save_interval==0:
				torch.save(self.actor.state_dict(),self.args.save_dir+'/'+self.args.env_name+'_'+str(iteration)+'_ppo_actor.pth.tar')
				self.writer.export_scalars_to_json(self.args.save_dir+'/'+self.args.env_name+"_all_scalars.json")

		self.envs.close()
		self.writer.close()


def main():
	os.environ['OMP_NUM_THREADS'] = '1'
	args = get_args()
	np.random.seed(args.seed)
	torch.manual_seed(args.seed)
	if args.cuda:
		torch.cuda.manual_seed_all(args.seed)
	agent = PPOAgent(args)
	try:
		agent.train()
	except KeyboardInterrupt:
		pass
	
if __name__=='__main__':
	main()