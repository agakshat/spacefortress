from setuptools import setup, find_packages
import os

VERSION = 2.0

descr_file = os.path.join(os.path.dirname(__file__), 'README.rst')

setup(
    name = "spacefortress.gym",
    version = '%s' % VERSION,
    description='An OpenAI Gym wrapper for c-spacefortress',
    long_description=open(descr_file).read(),
    author='Shawn Betts',
    author_email='sabetts@andrew.cmu.edu',
    url='https://bitbucket.org/andersonlab/c-spacefortress',
    packages=find_packages(),
    namespace_packages=['spacefortress'],
    license='GPL-2',
    install_requires=['numpy','gym','spacefortress'],
    classifiers=[
        'Development Status :: 4 - Beta',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
        'Programming Language :: Python :: 2.7',
    ],
)
