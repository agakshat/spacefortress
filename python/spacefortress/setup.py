from setuptools import setup, Extension
from pkgconfig import pkgconfig
from subprocess import PIPE,Popen
import os

GGITSHA1 = Popen(["git", "rev-parse", "-q", "HEAD"], stdout=PIPE).communicate()[0].decode('utf8').strip()
GVERSION = '1.%d' % len(Popen(["git", "log", "--pretty=oneline"], stdout=PIPE).communicate()[0].decode('utf8').split("\n"))

deps = pkgconfig("cairo")
deps.update({
    'define_macros': [
        ('GGITSHA1', '"%s"' % GGITSHA1),
        ('GVERSION', '"v%s"' % GVERSION)
    ]
})

sources = [os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)),"../../src/%s" % f)) for f in ["ssf.c","ssf_cairo.c"]]
headers = [os.path.abspath(os.path.join(os.path.dirname(os.path.realpath(__file__)),"../../src/%s" % f)) for f in ["ssf.h","ssf_cairo.h"]]

extension_mod = Extension("spacefortress/libssfcairo", sources, **deps)

descr_file = os.path.join(os.path.dirname(__file__), 'README.rst')

setup(
    name = "spacefortress",
    version = '%s' % GVERSION,
    description='A python wrapper to c-spacefortress',
    long_description=open(descr_file).read(),
    author='Shawn Betts',
    author_email='sabetts@andrew.cmu.edu',
    url='https://bitbucket.org/andersonlab/c-spacefortress',
    packages=['spacefortress'],
    ext_modules=[extension_mod],
    license='GPL-2',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
        'Programming Language :: Python :: 2.7',
    ],
)
