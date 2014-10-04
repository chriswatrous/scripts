#! /usr/bin/python

import os
from subprocess import call
from pdb import set_trace

# change to the directory of this script
os.chdir(os.path.dirname(os.path.realpath(__file__)))

def isfile(file):
    return os.path.isfile(os.path.expanduser(file))
files = ['~/.home_cygwin', '~/.work_cygwin', '~/.home_linux', '~/.work_linux']
home_cygwin, work_cygwin, home_linux, work_linux = map(isfile, files) 

scripts = [(os.path.expanduser('~/.bashrc'), os.path.realpath('.bashrc')),
           (os.path.expanduser('~/.pythonrc.py'), os.path.realpath('.pythonrc.py')),
           (os.path.expanduser('~/.octaverc'), os.path.realpath('.octaverc'))]

cygwin = os.path.exists('/cygdrive')
if cygwin:
    winhome = os.environ['USERPROFILE'] 
    scripts.append((os.path.join(winhome, 'Documents/AutoHotkey.ahk'), os.path.realpath('AutoHotkey.ahk')))
    scripts.append((os.path.join(winhome, 'Documents/AutoHotkey/Lib/PlaceActiveWindow.ahk'), os.path.realpath('PlaceActiveWindow.ahk')))

if not cygwin:
    scripts.append((os.path.expanduser('~/.vimrc'), os.path.realpath('.vimrc')))

for installed, checkout in scripts:
    installed_mtime = os.stat(installed).st_mtime
    checkout_mtime = os.stat(checkout).st_mtime

    if installed_mtime > checkout_mtime:
        print 'Copying %s -> %s' % (installed, checkout)
        call(['cp', '-p', installed, checkout])
    elif installed_mtime < checkout_mtime:
        print 'Copying %s -> %s' % (checkout, installed)
        call(['cp', '-p', checkout, installed])

if cygwin:
    lin = os.path.expanduser('~/.vimrc')
    win = os.path.join(winhome, '_vimrc')
    chk = '.vimrc'

    lin_mtime = os.stat(lin).st_mtime
    win_mtime = os.stat(win).st_mtime
    chk_mtime = os.stat(chk).st_mtime

    files = [lin, win, chk]
    mtimes = [os.stat(x).st_mtime for x in files]
    if len(set(mtimes)) > 1:
        i = mtimes.index(max(mtimes))
        source = files[i]
        files.remove(source)
        for f in files:
            print 'Copying %s -> %s' % (source, f)
            call(['cp', '-p', source, f])

