import random
import argparse
import subprocess
import ssf
import math

# An example of using the pixel buffer from the C space fortress library

def parse_args():
    parser = argparse.ArgumentParser()
    parser.add_argument('--output', metavar="FILE", default="output.mpg", help="Specify the name of the video file. (output.mpg)")
    parser.add_argument('--log', metavar="FILE", default="output.log", help="Specify the name of the log file. (output.log)")
    args = parser.parse_args()
    return args

def start_ffmpeg(output_file, width, height):
    command = ['ffmpeg',
               # '-loglevel', 'quiet',
               '-y',
               '-f', 'rawvideo',
               '-vcodec', 'rawvideo',
               '-s', '%dx%d'%(width,height),
               '-pix_fmt', 'bgra',
               '-r', '30',
               '-i', '-',
               '-an',
               # '-c:v', 'libx264',
               # '-preset', 'slow',
               # '-c:v', 'libxvid',
               # '-b:v', '3000k',
               '-c:v', 'mpeg1video',
               '-b:v', '5000k',
               output_file]
    pipe = subprocess.Popen(command, stdin=subprocess.PIPE)
    return pipe

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
    scale = .25
    w = int(math.ceil(g.contents.config.width * scale))
    h = int(math.ceil(g.contents.config.height * scale))
    # w = 160
    # h = 192
    pb = ssf.newPixelBuffer(g, w, h)
    raw_pixels = ssf.get_pixel_buffer_data(pb)
    pipe = start_ffmpeg(args.output, w, h)

    ssf.openLog(g, args.log)

    last_key = None
    print "start playing"
    while not ssf.isGameOver(g):
        last_key = play_like_an_idiot(g, last_key)
        ssf.stepOneTick(g, 33)
        ssf.logGameState(g)
        # ssf.drawTinyGameState(g, pb)
        ssf.drawGameStateScaled(g, pb, scale)
        pipe.stdin.write(raw_pixels)

    pipe.stdin.close()
    ssf.closeLog(g)
