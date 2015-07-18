#!/usr/bin/python
from os.path import exists, expanduser, join, islink
from subprocess import call, check_call
import sys

homedir = expanduser('~')

# Setup scripts.
call(['sudo', 'apt-get', 'install', 'git'])
if not exists(join(homedir, 'scripts')):
    call(['git', 'clone', 'https://github.com/chriswatrous/scripts'])
def setup_script(script_name):
    path = join(homedir, script_name)
    if exists(path) and islink(path):
        return
    if exists(path):
        call(['mv', path, path + '_original'])
    call(['ln', '-s', join(homedir, 'scripts', script_name), homedir])
setup_script('.bashrc')
setup_script('.ghci')
setup_script('.gvimrc')
setup_script('.octaverc')
setup_script('.pdbrc')
setup_script('.pythonrc.py')
setup_script('.vimrc')
setup_script('.vimrc.py')

# Install ubuntu packages.
apt_get_packages = [
    'geda',
    'ghci',
    'python-pip',
    'python3',
    'python3-pip',
    'svn',
    'terminator',
    'vim-gtk',
    ]
for package in apt_get_packages:
    call(['sudo', 'apt-get', 'install', '-y', package])
call(['sudo', 'apt-get', 'update', '-y'])
call(['sudo', 'apt-get', 'upgrade', '-y'])

# Install python packages.
call(['sudo', 'pip', 'install', '--upgrade', 'pip'])
pip_packages = [
    'requests',
    'flask',
    'behave'
    ]
for package in pip_packages

