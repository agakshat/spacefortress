from __future__ import absolute_import, division, print_function
import numpy as np
import glob
import torch
from torch.autograd import Variable
import torch.optim as optim
from networks import ActorCriticSharedConvNetwork
from copy import deepcopy
import gym
import pdb
import os
from arguments import get_args
from envs import make_env
import gym_vecenv
from storage import RolloutStorage
import time
import spacefortress.gym
from tensorboardX import SummaryWriter

class PPOAgent(object):
  def __init__(self,args):
    self.env_name = args.env_name
    dummy_env = gym.make(self.env_name)
    is_discrete_action = type(dummy_env.action_space) is gym.spaces.discrete.Discrete
    if is_discrete_action:
      self.action_dim = dummy_env.action_space.n
      self.actor = ActorCriticSharedConvNetwork(self.action_dim)
    else:
      assert False, "This setup for Atari should not have continuous action space environments"
    del dummy_env
    self.actor_lr = args.lr
    self.use_cuda = args.cuda
    if args.continue_training:
      try:
        actorState = torch.load(args.load_dir,map_location = lambda storage, loc: storage)
        self.actor.load_state_dict(actorState)
      except:
        assert False, "Unable to find a model to load"
    if self.use_cuda:
      self.actor.cuda()
    self.actor_optimizer = optim.Adam(self.actor.parameters(),lr=self.actor_lr)
    self.time_horizon = args.time_horizon
    self.minibatch_size = args.minibatch_size
    self.gamma = args.gamma
    self.use_gae = args.use_gae
    self.gaelamb = 0
    if self.use_gae:
      self.gaelamb = args.tau
    self.clip = args.clip_param
    self.num_threads = args.num_processes
    self.thread_minibatch_size = self.minibatch_size//self.num_threads
    self.vfCoeff = args.value_loss_coeff
    self.entropyCoeff = args.entropy_coeff
    self.max_grad_norm = args.max_grad_norm
    self.optim_epochs = args.ppo_epoch
    self.seed = args.seed
    self.log_interval = args.log_interval
    self.save_interval = args.save_interval
    self.vis_interval = args.vis_interval
    self.vis = args.vis
    self.port = args.port
    self.save_dir = args.save_dir
    self.log_dir = args.log_dir
    self.env_list = [make_env(self.env_name,self.seed,i) for i in range(self.num_threads)]
    if self.num_threads > 1:
      self.envs = gym_vecenv.SubprocVecEnv(self.env_list)
    else:
      self.envs = gym_vecenv.DummyVecEnv(self.env_list)
    if len(self.envs.observation_space.shape) == 1:
      self.envs = gym_vecenv.VecNormalize(self.envs)
    self.obs_shape = self.envs.observation_space.shape
    self.obs_shape = (self.obs_shape[0] * args.num_stack, *self.obs_shape[1:])
    self.state_shape = 256
    self.rollouts = RolloutStorage(self.time_horizon, self.num_threads, self.obs_shape, self.envs.action_space, self.state_shape)
    self.num_updates = int(args.num_frames)//args.time_horizon//args.num_processes
    self.current_obs = torch.zeros(self.num_threads,*self.obs_shape)
    self.num_stack = args.num_stack
    self.writer = SummaryWriter(log_dir=self.save_dir)
    self.clip_rewards = args.clip_rewards
    self.boost_destroy_mul = args.boost_destroy_mul
    self.fortress_threshold = 650
    self.scheduler = torch.optim.lr_scheduler.ReduceLROnPlateau(self.actor_optimizer,
                      mode='max',factor=0.2,patience=15,verbose=True,threshold=1e-3,
                      threshold_mode='rel')
    #self.scheduler2 = torch.optim.lr_scheduler.MultiStepLR(self.actor_optimizer,milestones=[40,80],gamma=0.3)

  def update_current_obs(self,obs):
    shape_dim0 = self.envs.observation_space.shape[0]
    obs = torch.from_numpy(obs).float()
    if self.num_stack > 1:
        self.current_obs[:, :-shape_dim0] = self.current_obs[:, shape_dim0:]
    self.current_obs[:, -shape_dim0:] = obs

  def train(self):

    obs = self.envs.reset()
    self.update_current_obs(obs)
    self.rollouts.observations[0].copy_(self.current_obs)

    episode_rewards = torch.zeros([self.num_threads,1])
    final_rewards = torch.zeros([self.num_threads,1])
    num_destruction = 0
    if self.use_cuda:
      self.current_obs = self.current_obs.cuda()
      self.rollouts.cuda()

    start = time.time()
    num_episodes = 0
    for iteration in range(self.num_updates):
      for step in range(self.time_horizon):
        value,action,action_log_prob,states = self.actor.act(Variable(self.rollouts.observations[step], volatile=True),
                                                              Variable(self.rollouts.states[step], volatile=True),
                                                              Variable(self.rollouts.masks[step], volatile=True))
        cpu_actions = action.data.squeeze(1).cpu().numpy()
        obs,reward,done,info = self.envs.step(cpu_actions)
        num_destruction += sum(info)
        reward = torch.from_numpy(np.expand_dims(np.stack(reward),1)).float()
        if self.clip_rewards:
          reward = torch.sign(reward)
          infoarr = np.asarray(info).astype(int)
          infoarr = torch.from_numpy(np.expand_dims(np.stack(infoarr),1)).float()
          reward = reward + self.boost_destroy_mul*infoarr

        episode_rewards += reward
        masks = torch.FloatTensor([[0.0] if i else [1.0] for i in done])
        final_rewards*=masks
        final_rewards += (1-masks)*episode_rewards

        episode_rewards *= masks

        if self.use_cuda:
          masks = masks.cuda()
        if self.current_obs.dim() == 4:
          self.current_obs *= masks.unsqueeze(2).unsqueeze(2)
        else:
          self.current_obs *= masks

        self.update_current_obs(obs)
        self.rollouts.insert(step,self.current_obs,states.data,action.data,action_log_prob.data,value.data,reward,masks)

      next_value = self.actor.get_value(Variable(self.rollouts.observations[-1], volatile=True),
                                        Variable(self.rollouts.states[-1], volatile=True),
                                        Variable(self.rollouts.masks[-1], volatile=True)).data
      
      self.rollouts.compute_returns(next_value,self.use_gae,self.gamma,self.gaelamb)
      advantages = self.rollouts.returns[:-1] - self.rollouts.value_preds[:-1]
      advantages = (advantages - advantages.mean())/(advantages.std()+1e-5)

      for i in range(1):
        data_generator = self.rollouts.recurrent_generator(advantages,self.minibatch_size)
        for sample in data_generator:
          observations_batch, states_batch, actions_batch, \
            return_batch, masks_batch, old_action_log_probs_batch, \
              adv_targ = sample
          values,action_log_probs,dist_entropy,states = self.actor.evaluate_actions(Variable(observations_batch),
                                                                                    Variable(states_batch),
                                                                                    Variable(masks_batch),
                                                                                    Variable(actions_batch))
          adv_targ = Variable(adv_targ)
          #ratio = torch.exp(action_log_probs - Variable(old_action_log_probs_batch))
          #surr1 = ratio*adv_targ
          #surr2 = torch.clamp(ratio,1.0-self.clip,1.0+self.clip)*adv_targ
          #action_loss = -torch.min(surr1,surr2).mean()
          action_loss = -(adv_targ*action_log_probs).mean()
          value_loss = (values-Variable(return_batch)).pow(2).mean()
          self.actor_optimizer.zero_grad()
          actorLoss = action_loss + self.vfCoeff*value_loss - self.entropyCoeff*dist_entropy
          actorLoss.backward()
          torch.nn.utils.clip_grad_norm(self.actor.parameters(),self.max_grad_norm)
          self.actor_optimizer.step()
      self.rollouts.after_update()

      if num_destruction>self.fortress_threshold:
        torch.save(self.actor.state_dict(),self.save_dir+'/'+self.env_name+'_'+str(iteration)+'_ppo_actor.pth.tar')
        self.fortress_threshold = num_destruction

      if iteration%self.log_interval == 0:
        end = time.time()
        total_num_steps = (iteration+1)*self.num_threads*self.time_horizon
        print("Updates {}, num timesteps {}, FPS {}, mean/median reward {:.1f}/{:.1f}, min/max reward {:.1f}/{:.1f}, entropy {:.5f}, value loss {:.5f}, policy loss {:.5f}, num fortress destroyed {}".
                format(iteration, total_num_steps,
                       int(total_num_steps / (end - start)),
                       final_rewards.mean(),
                       final_rewards.median(),
                       final_rewards.min(),
                       final_rewards.max(), dist_entropy.data[0],
                       value_loss.data[0], action_loss.data[0],int(num_destruction)))
        self.writer.add_scalar('data/rewardmean',final_rewards.mean(),total_num_steps)
        self.writer.add_scalar('data/distentropy',dist_entropy.data[0],total_num_steps)
        self.writer.add_scalar('data/valueloss',value_loss.data[0],total_num_steps)
        self.writer.add_scalar('data/actionloss',action_loss.data[0],total_num_steps)
        self.writer.add_scalar('data/numdestruction',int(num_destruction),total_num_steps)
        #self.scheduler.step(final_rewards.mean())
        #self.scheduler2.step()
        num_destruction = 0

      if iteration%self.save_interval==0:
        torch.save(self.actor.state_dict(),self.save_dir+'/'+self.env_name+'_'+str(iteration)+'_a2c_actor.pth.tar')
        self.writer.export_scalars_to_json(self.save_dir+'/'+self.env_name+"_all_scalars.json")

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








