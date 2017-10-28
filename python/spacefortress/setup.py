from setuptools import setup, find_packages, Extension
from pkgconfig import pkgconfig
from subprocess import PIPE,Popen
import os

VERSION = 2.0

deps = pkgconfig("cairo")

sources = ["src/%s" % f for f in ["config.cpp","configs.cpp","vector.cpp","object.cpp","hexagon.cpp","game.cpp","pymodule.cpp","wireframe.cpp","draw.cpp"]]

if 'extra_compile_args' not in deps:
    deps['extra_compile_args'] = []
deps['extra_compile_args'].append("-std=c++11")

extension_mod = Extension("spacefortress/core/_spacefortress", sources, **deps)

descr_file = os.path.join(os.path.dirname(__file__), 'README.rst')

setup(
    name = "spacefortress",
    version = '%s' % VERSION,
    description='A python wrapper to c-spacefortress',
    long_description=open(descr_file).read(),
    author='Shawn Betts',
    author_email='sabetts@andrew.cmu.edu',
    url='https://bitbucket.org/andersonlab/c-spacefortress',
    packages=find_packages(),
    namespace_packages=['spacefortress'],
    ext_modules=[extension_mod],
    license='GPL-2',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
        'Programming Language :: Python :: 2.7',
    ],
)
