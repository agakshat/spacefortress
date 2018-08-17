import torch
import torch.nn as nn
import torch.nn.functional as F
import pdb

def weights_init(m):
	classname = m.__class__.__name__
	if classname.find('Conv') != -1 or classname.find('Linear') != -1:
		nn.init.orthogonal_(m.weight.data)
		if m.bias is not None:
			m.bias.data.fill_(0)
	elif classname.find('GRU') != -1:
		nn.init.orthogonal_(m.weight_ih.data)
		nn.init.orthogonal_(m.weight_hh.data)
		if m.bias_ih is not None:
			m.bias_ih.data.fill_(0)
		if m.bias_hh is not None:
			m.bias_hh.data.fill_(0)

class ACNet(nn.Module):
	def __init__(self,action_dim,is_feedforward=False):
		super().__init__()
		self.ff = is_feedforward
		self.conv1 = nn.Conv2d(4,16,kernel_size=8,stride=4)
		self.conv2 = nn.Conv2d(16,32,kernel_size=4, stride=2)
		self.fc1 = nn.Linear(2592,256)
		if not self.ff:
			self.gru = nn.GRUCell(256,256)
		else:
			self.fc2 = nn.Linear(256,256)
		self.action = nn.Linear(256,action_dim)
		self.value = nn.Linear(256,1)
		self.is_discrete_action = True

		self.train()
		self.apply(weights_init)

	def get_value(self,obs,state,mask):
		x,_ = self._fwd(obs,state,mask)
		return self.value(x)

	def _fwd(self,obs,state,mask):
		x = F.relu(self.conv1(obs/255.0))
		x = F.relu(self.conv2(x))
		x = x.view(-1,2592)
		x = F.relu(self.fc1(x))
		if not self.ff:
			if obs.size(0) == state.size(0):
				state = self.gru(x,state*mask)
			else:
				N = state.size(0)
				T = int(x.size(0)/N)
				x = x.view(T,N,x.size(1))
				mask = mask.view(T,N,1)
				outputs = []
				for i in range(T):
					hx = state = self.gru(x[i],state*mask[i])
					outputs.append(hx)
				x = torch.stack(outputs,dim=0)
				x = x.view(T*N,-1)
			x = F.tanh(state)
		else:
			x = F.relu(self.fc2(x))
		return x,state

	def act(self,obs,state,mask,deterministic=False):
		x,state = self._fwd(obs,state,mask)
		value = self.value(x)
		xact = self.action(x)
		probs = F.softmax(xact,dim=1)
		log_probs = F.log_softmax(xact,dim=1)
		if deterministic:
			action = probs.max(1,keepdim=True)[1]
		else:	
			action = probs.multinomial(1)
		action_log_probs = log_probs.gather(1,action)
		return value,action,action_log_probs,state

	def evaluate_actions(self,obs,state,mask,actions):
		x,state = self._fwd(obs,state,mask)
		xact = self.action(x)
		value = self.value(x)
		probs = F.softmax(xact,dim=1)
		log_probs = F.log_softmax(xact,dim=1)
		action_log_probs = log_probs.gather(1,actions)
		dist_entropy = -(log_probs*probs).sum(-1).mean()
		return value,action_log_probs,dist_entropy,state

	def load_autoturn_model(self,actorState):
		assert actorState.keys()==self.state_dict().keys(), "Keys not same!"
		self.conv1.weight.data.copy_(actorState["conv1.weight"])
		self.conv1.bias.data.copy_(actorState["conv1.bias"])
		self.conv2.weight.data.copy_(actorState["conv2.weight"])
		self.conv2.bias.data.copy_(actorState["conv2.bias"])
		self.fc1.weight.data.copy_(actorState["fc1.weight"])
		self.fc1.bias.data.copy_(actorState["fc1.bias"])
		if not self.ff:
			self.gru.weight_ih.data.copy_(actorState["gru.weight_ih"])
			self.gru.weight_hh.data.copy_(actorState["gru.weight_hh"])
			self.gru.bias_ih.data.copy_(actorState["gru.bias_ih"])
			self.gru.bias_hh.data.copy_(actorState["gru.bias_hh"])
		else:
			self.fc2.weight.data.copy_(actorState["fc2.weight"])
			self.fc2.bias.data.copy_(actorState["fc2.bias"])
		self.value.weight.data.copy_(actorState["value.weight"])
		self.value.bias.data.copy_(actorState["value.bias"])
		print("Loaded model successfully")
