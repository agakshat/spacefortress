import sys
import argparse
import pyglet

from game import SSF_Game

def main(args=None):

    if args is None:
        args = sys.argv[1:]

    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--gametype', choices=["explode","autoturn"], default="explode")
    args = parser.parse_args(args)

    game = SSF_Game(args)

    pyglet.app.run()

if __name__ == '__main__':
    main()
