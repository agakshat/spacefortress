import numpy as np
import pickle
import matplotlib.pyplot as plt
from itertools import cycle

plt.ion()
lines = ["--",":","-.","-"]
linecycler = cycle(lines)

# fnames = [145,137,140,134]
# human = 1989
# plt.title('Autoturn Game, PPO')
# plt.plot([0,2700],[human,human],marker='+')

fnames = [144,136,139,135]
human = 216
plt.title('Youturn Game, PPO')
plt.plot([0,2700],[human,human],marker='+')

# fnames = [161,162]
# plt.title('Temporal Transfer Learning, 125ms')

for f in fnames:
  f1 = pickle.load(open("tr2/"+str(f)+"/learning_curves.p","rb"))
  X = []
  Y = []
  for k in sorted(f1.keys()):
    X.append(k)
    Y.append(f1[k][1])
  plt.plot(X,Y,marker='',ls=next(linecycler))#,color=next(colorcycler))

plt.legend(['Humans','SF-GRU: Default Rewards','SF-GRU: Dense Rewards','SF-FF: AECI','SF-GRU: AECI'],ncol=2)
# plt.legend(['Without Transfer','With Transfer'])

plt.xlabel('Training Iteration')
plt.ylabel('Score')
