import random
import argparse
import ssf
import math
import cv2
import numpy as np

# An example of using the pixel buffer from the C space fortress library

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output', metavar="FILE", default="output.mpg", help="Specify the name of the video file. (output.mpg)")
    parser.add_argument('--log', metavar="FILE", default="output.log", help="Specify the name of the log file. (output.log)")
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
    g = ssf.makeExplodeGame()
    scale = .5
    w = int(math.ceil(g.contents.config.width * scale))
    h = int(math.ceil(g.contents.config.height * scale))
    pb = ssf.newPixelBuffer(g, w, h)
    raw_pixels = ssf.get_pixel_buffer_data(pb)

    fourcc = cv2.cv.CV_FOURCC(*"XVID")
    out = cv2.VideoWriter('output.avi',fourcc, 30, (w,h))

    ssf.openLog(g, args.log)

    last_key = None
    print "start playing"
    while not ssf.isGameOver(g):
        last_key = play_like_an_idiot(g, last_key)
        ssf.stepOneTick(g, 33)
        ssf.logGameState(g)
        ssf.drawGameStateScaled(g, pb, scale)
        src = cv2.cvtColor(np.fromstring(raw_pixels, np.uint8).reshape(h, w, 4), cv2.COLOR_RGBA2RGB)
        out.write(src)

    out.release()
    cv2.destroyAllWindows()
    ssf.closeLog(g)
