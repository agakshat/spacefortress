import os
import subprocess
import distutils.spawn, distutils.version
import numpy as np

class ImageEncoder(object):
    def __init__(self, output_path, frame_shape, frames_per_sec):
        self.proc = None
        self.output_path = output_path
        # Frame shape should be lines-first, so w and h are swapped
        h, w, pixfmt = frame_shape
        if pixfmt != 3 and pixfmt != 4:
            raise Exception("Your frame has shape {}, but we require (w,h,3) or (w,h,4), i.e. RGB values for a w-by-h image, with an optional alpha channl.".format(frame_shape))
        self.wh = (w,h)
        self.includes_alpha = (pixfmt == 4)
        self.frame_shape = frame_shape
        self.frames_per_sec = frames_per_sec

        if distutils.spawn.find_executable('avconv') is not None:
            self.backend = 'avconv'
        elif distutils.spawn.find_executable('ffmpeg') is not None:
            self.backend = 'ffmpeg'
        else:
            raise Exception("""Found neither the ffmpeg nor avconv executables. On OS X, you can install ffmpeg via `brew install ffmpeg`. On most Ubuntu variants, `sudo apt-get install ffmpeg` should do it. On Ubuntu 14.04, however, you'll need to install avconv with `sudo apt-get install libav-tools`.""")

        self.start()

    @property
    def version_info(self):
        return {
            'backend':self.backend,
            'version':str(subprocess.check_output([self.backend, '-version'],
                                                  stderr=subprocess.STDOUT)),
            'cmdline':self.cmdline
        }

    def start(self):
        self.cmdline = (self.backend,
                     '-nostats',
                     '-loglevel', 'error', # suppress warnings
                     '-y',
                     '-r', '%d' % self.frames_per_sec,

                     # input
                     '-f', 'rawvideo',
                     '-s:v', '{}x{}'.format(*self.wh),
                     '-pix_fmt',('rgb32' if self.includes_alpha else 'rgb24'),
                     '-i', '-', # this used to be /dev/stdin, which is not Windows-friendly

                     # output
                     '-vcodec', 'libx264',
                     '-pix_fmt', 'yuv420p',
                     self.output_path
                     )

        if hasattr(os,'setsid'): #setsid not present on Windows
            self.proc = subprocess.Popen(self.cmdline, stdin=subprocess.PIPE, preexec_fn=os.setsid)
        else:
            self.proc = subprocess.Popen(self.cmdline, stdin=subprocess.PIPE)

    def capture_frame(self, frame):
        if not isinstance(frame, (np.ndarray, np.generic)):
            raise Exception('Wrong type {} for {} (must be np.ndarray or np.generic)'.format(type(frame), frame))
        if frame.shape != self.frame_shape:
            raise Exception("Your frame has shape {}, but the VideoRecorder is configured for shape {}.".format(frame.shape, self.frame_shape))
        if frame.dtype != np.uint8:
            raise Exception("Your frame has data type {}, but we require uint8 (i.e. RGB values from 0-255).".format(frame.dtype))

        if distutils.version.LooseVersion(np.__version__) >= distutils.version.LooseVersion('1.9.0'):
            self.proc.stdin.write(frame.tobytes())
        else:
            self.proc.stdin.write(frame.tostring())

    def close(self):
        self.proc.stdin.close()
        ret = self.proc.wait()
