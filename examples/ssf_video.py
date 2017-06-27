import sys, os
sys.path.insert(0, os.path.join(os.path.dirname(os.path.realpath(__file__)),"../"))

import random
import argparse
import ssf
import math
import cv2
import numpy as np

# An example of using the pixel buffer from the C space fortress library

def parse_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument('--game', choices=["explode","autoturn"], default="explode", help="Game type.")
    parser.add_argument('--output', default="output.avi", help="Specify the name of the video file.")
    parser.add_argument('--log',  default="output.log", help="Specify the name of the log file.")
    parser.add_argument('--scale', default=1, help="Scale of output image", type=float)
    parser.add_argument('--linesize', default=2, help="Scale of output image", type=float)
    parser.add_argument('--grayscale', action='store_true')
    args = parser.parse_args()
    return args

def play_like_an_idiot(g, last_key):
    keys = [ssf.FIRE_KEY, ssf.THRUST_KEY, ssf.LEFT_KEY, ssf.RIGHT_KEY]
    if g.contents.tick % 30 == 0:
        if last_key != None:
            ssf.releaseKey(g, last_key)
        last_key = keys[random.randint(0,len(keys)-1)]
        ssf.pressKey(g, last_key)
    return last_key

if __name__ == "__main__":
    args = parse_args()
    print(args.game)
    if args.game == "explode":
        g = ssf.makeExplodeGame(args.grayscale)
    elif args.game == "autoturn":
        g = ssf.makeAutoTurnGame(args.grayscale)
    scale = args.scale
    w = int(math.ceil(g.contents.config.width * scale))
    h = int(math.ceil(g.contents.config.height * scale))
    print(w,h)
    pb = ssf.newPixelBuffer(g, w, h)
    raw_pixels = ssf.get_pixel_buffer_data(pb)
    ssf.drawGameStateScaled(g, pb, scale, args.linesize)
    cv2.imwrite("output.png", cv2.cvtColor(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4), cv2.COLOR_RGBA2RGB))
    out = cv2.VideoWriter(args.output ,cv2.VideoWriter_fourcc(*"H264"), 30, (w,h))

    ssf.openLog(g, args.log)

    last_key = None
    print "start playing"
    while not ssf.isGameOver(g):
        last_key = play_like_an_idiot(g, last_key)
        ssf.stepOneTick(g, 33)
        ssf.logGameState(g)
        ssf.drawGameStateScaled(g, pb, scale, args.linesize)
        src = cv2.cvtColor(cv2.cvtColor(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4), cv2.COLOR_RGBA2GRAY), cv2.COLOR_GRAY2RGB)
        out.write(src)

    out.release()
    cv2.destroyAllWindows()
    ssf.closeLog(g)
