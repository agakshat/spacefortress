import sys
import argparse

from game import SSF_Game

def main(args=None):

    if args is None:
        args = sys.argv[1:]

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--game', choices=["explode","autoturn","deep-explode","deep-autoturn","slow-autoturn","slow-explode","nopenalty-explode","nopenalty-autoturn","test-autoturn","test-explode"], default="explode")
    parser.add_argument('--video', type=str, default=None)
    args = parser.parse_args(args)
    args.game = args.game

    game = SSF_Game(args)
    game.run()

if __name__ == '__main__':
    main()
