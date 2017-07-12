from setuptools import setup
from subprocess import PIPE,Popen
import os

GVERSION = '1.%d' % len(Popen(["git", "log", "--pretty=oneline"], stdout=PIPE).communicate()[0].decode('utf8').split("\n"))

descr_file = os.path.join(os.path.dirname(__file__), 'README.rst')

setup(
    name = "spacefortress.game",
    version = '%s' % GVERSION,
    description='Pyglet wrapper to c-spacefortress',
    long_description=open(descr_file).read(),
    author='Shawn Betts',
    author_email='sabetts@andrew.cmu.edu',
    url='https://bitbucket.org/andersonlab/c-spacefortress',
    packages=['spacefortress.game'],
    license='GPL-2',
    install_requires=['numpy','pyglet','gym','spacefortress'],
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
        'Programming Language :: Python :: 2.7',
    ],
    entry_points={
        'gui_scripts': [
            'spacefortress = spacefortress.game.__main__:main'
        ]
    },
)
