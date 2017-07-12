from setuptools import setup
from subprocess import PIPE,Popen
import os

GVERSION = '1.%d' % len(Popen(["git", "log", "--pretty=oneline"], stdout=PIPE).communicate()[0].decode('utf8').split("\n"))

descr_file = os.path.join(os.path.dirname(__file__), 'README.rst')

setup(
    name = "spacefortress.gym",
    version = '%s' % GVERSION,
    description='An OpenAI Gym wrapper for c-spacefortress',
    long_description=open(descr_file).read(),
    author='Shawn Betts',
    author_email='sabetts@andrew.cmu.edu',
    url='https://bitbucket.org/andersonlab/c-spacefortress',
    packages=['spacefortress.gym'],
    license='GPL-2',
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
        'Programming Language :: Python :: 2.7',
    ],
)
