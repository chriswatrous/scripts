#! /usr/bin/python

from subprocess import call
from os import chdir, environ, stat
from os.path import realpath, expanduser, exists, dirname, join

# change to the directory of this script
chdir(dirname(realpath(__file__)))

scripts = [(expanduser('~/.bashrc'), realpath('.bashrc')),
           (expanduser('~/.pythonrc.py'), realpath('.pythonrc.py')),
           (expanduser('~/.octaverc'), realpath('.octaverc'))]

cygwin = exists('/cygdrive')
if cygwin:
    winhome = environ['USERPROFILE'] 
    scripts.append((join(winhome, 'Documents/AutoHotkey.ahk'), realpath('AutoHotkey.ahk')))
    scripts.append((join(winhome, 'Documents/AutoHotkey/Lib/PlaceActiveWindow.ahk'), realpath('PlaceActiveWindow.ahk')))

if not cygwin:
    scripts.append((expanduser('~/.vimrc'), realpath('.vimrc')))

for installed, checkout in scripts:
    installed_mtime = stat(installed).st_mtime
    checkout_mtime = stat(checkout).st_mtime

    if installed_mtime > checkout_mtime:
        print 'Copying %s -> %s' % (installed, checkout)
        call(['cp', '-p', installed, checkout])
    elif installed_mtime < checkout_mtime:
        print 'Copying %s -> %s' % (checkout, installed)
        call(['cp', '-p', checkout, installed])

if cygwin:
    lin = expanduser('~/.vimrc')
    win = join(winhome, '_vimrc')
    chk = '.vimrc'

    lin_mtime = stat(lin).st_mtime
    win_mtime = stat(win).st_mtime
    chk_mtime = stat(chk).st_mtime

    files = [lin, win, chk]
    mtimes = [stat(x).st_mtime for x in files]
    if len(set(mtimes)) > 1:
        i = mtimes.index(max(mtimes))
        source = files[i]
        files.remove(source)
        for f in files:
            print 'Copying %s -> %s' % (source, f)
            call(['cp', '-p', source, f])

