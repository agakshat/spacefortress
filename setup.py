from distutils.core import setup, Extension
from pkgconfig import pkgconfig
import subprocess

GGITSHA1 = subprocess.check_output(["git", "rev-parse", "-q", "HEAD"]).strip()
GVERSION = '1.%d' % len(subprocess.check_output(["git", "log", "--pretty=oneline"]).strip().split("\n"))

deps = pkgconfig("cairo")
deps.update({
    'define_macros': [
        ('GGITSHA1', '"%s"' % GGITSHA1),
        ('GVERSION', '"v%s"' % GVERSION)
    ]
})

extension_mod = Extension("ssf/libssfcairo", ["src/ssf.c", "src/ssf_cairo.c"], **deps)

setup(
    name = "ssf",
    version = '%s' % GVERSION,
    author='Shawn Betts',
    author_email='sabetts@andrew.cmu.edu',
    url='https://bitbucket.org/andersonlab/c-spacefortress',
    packages=['ssf'],
    ext_modules=[extension_mod]
)
