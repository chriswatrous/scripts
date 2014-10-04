#! /usr/bin/python

import os
from subprocess import call

# change to the directory of this script
os.chdir(os.path.dirname(os.path.realpath(__file__)))


scripts = [
    (os.path.expanduser('~/.bashrc'), os.path.realpath('.bashrc')),
    (os.path.expanduser('~/.vimrc'), os.path.realpath('.vimrc')),
    (os.path.expanduser('~/.pythonrc.py'), os.path.realpath('.pythonrc.py')),
    (os.path.expanduser('~/.octaverc'), os.path.realpath('.octaverc'))]

if os.path.isfile(os.path.expanduser('~/.home-cygwin')):
    scripts.append(('/cygdrive/c/Users/Chris/Documents/AutoHotkey.ahk', os.path.realpath('AutoHotkey.ahk')))
    scripts.append(('/cygdrive/c/Users/Chris/Documents/AutoHotkey/Lib/PlaceActiveWindow.ahk', os.path.realpath('PlaceActiveWindow.ahk')))

if os.path.isfile(os.path.expanduser('~/.work-cygwin')):
    scripts.append(('/cygdrive/c/Users/cwatrous/Documents/AutoHotkey.ahk', os.path.realpath('AutoHotkey.ahk')))
    scripts.append(('/cygdrive/c/Users/cwatrous/Documents/AutoHotkey/Lib/PlaceActiveWindow.ahk', os.path.realpath('PlaceActiveWindow.ahk')))

for installed, checkout in scripts:
    installed_mtime = os.stat(installed).st_mtime
    checkout_mtime = os.stat(checkout).st_mtime

    if installed_mtime > checkout_mtime:
        print 'Copying %s -> %s' % (installed, checkout)
        call(['cp', '-p', installed, checkout])
    elif installed_mtime < checkout_mtime:
        print 'Copying %s -> %s' % (checkout, installed)
        call(['cp', '-p', checkout, installed])
