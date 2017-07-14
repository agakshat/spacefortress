import random
import argparse
import math
import numpy as np

import spacefortress as sf
from spacefortress.util import ImageEncoder

FPS = 30

def parse_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--game', choices=["explode","autoturn"], default=["explode"], nargs=1)
    parser.add_argument('--video', default="output.mp4", help="Specify the name of the video file.")
    parser.add_argument('--log',  default="output.log", help="Specify the name of the log file.")
    parser.add_argument('--scale', default=1, help="Scale of output image", type=float)
    parser.add_argument('--linesize', default=2, help="Scale of output image", type=float)
    parser.add_argument('--grayscale', action='store_true')
    args = parser.parse_args()
    args.game = args.game[0]
    return args

def play_like_an_idiot(g, last_key, keys):
    if g.contents.tick % FPS == 0:
        if last_key != None:
            sf.releaseKey(g, last_key)
        last_key = keys[random.randint(0,len(keys)-1)]
        sf.pressKey(g, last_key)
    return last_key

if __name__ == "__main__":
    args = parse_args()
    print(args.game)
    if args.game == "explode":
        g = sf.makeExplodeGame(args.grayscale)
        keys = [sf.FIRE_KEY, sf.THRUST_KEY, sf.LEFT_KEY, sf.RIGHT_KEY]
    elif args.game == "autoturn":
        g = sf.makeAutoTurnGame(args.grayscale)
        keys = [sf.FIRE_KEY, sf.THRUST_KEY]
    scale = args.scale
    w = int(math.ceil(g.contents.config.width * scale))
    h = int(math.ceil(g.contents.config.height * scale))
    print(w,h)
    pb = sf.newPixelBuffer(g, w, h)
    raw_pixels = sf.get_pixel_buffer_data(pb)
    sf.drawGameStateScaled(g, pb, scale, args.linesize)
    if args.video:
        encoder = ImageEncoder(args.video, (h, w, 4), FPS)
        encoder.capture_frame(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4))
    else:
        encoder = None

    sf.openLog(g, args.log)

    last_key = None
    print "start playing"
    while not sf.isGameOver(g):
        last_key = play_like_an_idiot(g, last_key, keys)
        sf.stepOneTick(g, 33)
        sf.drawGameStateScaled(g, pb, scale, args.linesize)
        if encoder:
            encoder.capture_frame(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4))

    if encoder:
        encoder.close()
    sf.closeLog(g)
