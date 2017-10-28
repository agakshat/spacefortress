import random
import argparse
import math
import numpy as np

import spacefortress.core as sf
from spacefortress.util import ImageEncoder

FPS = 30

def parse_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--game', choices=["explode","autoturn"], default=["explode"], nargs=1)
    parser.add_argument('--video', default="output.mp4", help="Specify the name of the video file.")
    parser.add_argument('--scale', default=1, help="Scale of output image", type=float)
    parser.add_argument('--linesize', default=2, help="Scale of output image", type=float)
    parser.add_argument('--grayscale', action='store_true')
    args = parser.parse_args()
    args.game = args.game[0]
    return args

def play_like_an_idiot(g, last_key, keys):
    if g.tick % FPS == 0:
        if last_key != None:
            g.release_key(last_key)
        last_key = keys[random.randint(0,len(keys)-1)]
        g.press_key(last_key)
    return last_key

if __name__ == "__main__":
    args = parse_args()
    print(args.game)
    g = sf.Game(args.game, args.scale, args.linesize, args.grayscale )
    if args.game == "explode":
        keys = [sf.FIRE_KEY, sf.THRUST_KEY, sf.LEFT_KEY, sf.RIGHT_KEY]
    elif args.game == "autoturn":
        keys = [sf.FIRE_KEY, sf.THRUST_KEY]
    scale = args.scale
    w = g.pb_width
    h = g.pb_height
    print(w,h)
    raw_pixels = g.pb_pixels
    g.draw()
    if args.video:
        encoder = ImageEncoder(args.video, (h, w, 4), FPS)
        encoder.capture_frame(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4))
    else:
        encoder = None

    last_key = None
    print "start playing"
    while not g.is_game_over():
        last_key = play_like_an_idiot(g, last_key, keys)
        g.step_one_tick(33)
        g.draw()
        if encoder:
            encoder.capture_frame(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4))

    if encoder:
        encoder.close()
