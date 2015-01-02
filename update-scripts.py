#! /usr/bin/python

from subprocess import call
from os import chdir, environ, stat
from os.path import realpath, expanduser, exists, dirname, join

chdir(dirname(realpath(__file__)))

scripts = [[expanduser('~/.bashrc'), realpath('.bashrc')],
           [expanduser('~/.pythonrc.py'), realpath('.pythonrc.py')],
           [expanduser('~/.octaverc'), realpath('.octaverc')]]

if exists('/cygdrive'):
    winhome = environ['USERPROFILE']
    winhome = join('/cygdrive/c/Users', environ['USER'])
    scripts.extend(
        [[join(winhome, 'Documents/AutoHotkey.ahk'), realpath('AutoHotkey.ahk')],
         [join(winhome, 'Documents/AutoHotkey/Lib/PlaceActiveWindow.ahk'), realpath('PlaceActiveWindow.ahk')],
         [expanduser('~/.vimrc'), join(winhome, '_vimrc'), '.vimrc']])
else:
    scripts.append([expanduser('~/.vimrc'), realpath('.vimrc')])

path = expanduser('~/.config/openbox/lxde-rc.xml')
if exists(path):
    scripts.append([path, realpath('lxde-rc.xml')])


for files in scripts:
    mtimes = [stat(x).st_mtime for x in files]
    if len(set(mtimes)) > 1:
        i = mtimes.index(max(mtimes))
        source = files[i]
        files.remove(source)
        for f in files:
            print 'Copying %s -> %s' % (source, f)
            call(['cp', '-p', source, f])

