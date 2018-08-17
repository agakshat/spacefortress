import argparse
import torch
import os

def get_args():
    parser = argparse.ArgumentParser(description='RL')
    parser.add_argument('--lr', type=float, default=1e-3,
                        help='learning rate (default: 1e-3)')
    parser.add_argument('--gamma', type=float, default=0.99,
                        help='discount factor for rewards (default: 0.99)')
    parser.add_argument('--tau', type=float, default=0.95,
                        help='gae parameter (default: 0.95)')
    parser.add_argument('--entropy-coeff', type=float, default=0.05,
                        help='entropy term coefficient (default: 0.05)')
    parser.add_argument('--value-loss-coeff', type=float, default=0.5,
                        help='value loss coefficient (default: 0.5)')
    parser.add_argument('--max-grad-norm', type=float, default=0.5,
                        help='max norm of gradients (default: 0.5)')
    parser.add_argument('--seed', type=int, default=12345,
                        help='random seed (default: 12345)')
    parser.add_argument('--num-processes', type=int, default=16,
                        help='how many training CPU processes to use (default: 16)')
    parser.add_argument('--num-fwd-steps', type=int, default=1024,
                        help='number of forward timesteps in PPO (default: 1024)')
    parser.add_argument('--ppo-epoch', type=int, default=4,
                        help='number of ppo epochs (default: 4)')
    parser.add_argument('--num-mini-batch', type=int, default=4,
                        help='number of minibatches for ppo (default: 4)')
    parser.add_argument('--clip-param', type=float, default=0.1,
                        help='ppo clip parameter (default: 0.1)')
    parser.add_argument('--num-stack', type=int, default=4,
                        help='number of frames to stack (default: 4)')
    parser.add_argument('--log-interval', type=int, default=10,
                        help='log interval, one log per n updates (default: 10)')
    parser.add_argument('--save-interval', type=int, default=100,
                        help='save interval, one save per n updates (default: 100)')
    parser.add_argument('--vis-interval', type=int, default=100,
                        help='vis interval, one log per n updates (default: 100)')
    parser.add_argument('--num-frames', type=int, default=45e6,
                        help='number of frames to train (default: 45e6)')
    parser.add_argument('--env-name', default='youturn',
                        help='environment to train on (default: youturn)')
    parser.add_argument('--save-dir', default='./trained_models/',
                        help='directory to save agent logs (default: ./trained_models/)')
    parser.add_argument('--no-cuda', action='store_true', default=False,
                        help='disables CUDA training')
    parser.add_argument('--continue-training', action='store_true', default=False,
                        help='continue training from another model')
    parser.add_argument('--load-dir', default=None,
                        help='path to trained model file, if available')
    parser.add_argument('--transfer', action='store_true', default=False,
                        help='optionally transfer learning from autoturn')
    parser.add_argument('--feedforward', action='store_true', default=False,
                        help='use feedforward instead of recurrent architecture')
    args = parser.parse_args()

    args.cuda = not args.no_cuda and torch.cuda.is_available()

    if args.env_name == 'autoturn':
        args.env_name = 'SpaceFortress-autoturn-image-v0'
    if args.env_name == 'youturn':
        args.env_name = 'SpaceFortress-youturn-image-v0'

    if args.continue_training or args.transfer:
        assert os.path.exists(args.load_dir), "load_dir path does not exist"

    return args
