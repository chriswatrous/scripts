#! /usr/bin/python
from subprocess import call
from os import chdir, environ, stat
from os.path import realpath, expanduser, exists, dirname, join, isdir

chdir(dirname(realpath(__file__)))

def winhome(path):
    return join('/cygdrive/c/Users', environ['USER'], path)

scripts = {'.bashrc':               ['~/.bashrc'],
           '.pythonrc.py':          ['~/.pythonrc.py'],
           '.octaverc':             ['~/.octaverc'],
           '.ghci':                 ['~/.ghci'],
           '.pdbrc':                ['~/.pdbrc'],
           'AutoHotkey.ahk':        [winhome('Documents/AutoHotkey.ahk')],
           'PlaceActiveWindow.ahk': [winhome('Documents/AutoHotkey/Lib/PlaceActiveWindow.ahk')],
           '.vimrc':                ['~/.vimrc', winhome('_vimrc')],
           'lxde-rc.xml':           ['~/.config/openbox/lxde-rc.xml']}

def main():
    for name in scripts:
        scripts[name] = map(expanduser, scripts[name])

    for name in scripts:
        for path in list(scripts[name]):
            if not exists(path):
                scripts[name].remove(path)
            if isdir(path):
                print "Path is a directory: '{}'".format(path)
                scripts[name].remove(path)

    for name in scripts.keys():
        if scripts[name] == []:
            print "Skipping '{}': no installed files found".format(name)
            del scripts[name]

    print
    updated = False

    for name in scripts:
        files = [name] + scripts[name]
        mtimes = [stat(x).st_mtime for x in files]
        if len(set(mtimes)) > 1:
            updated = True
            i = mtimes.index(max(mtimes))
            source = files[i]
            files.remove(source)
            for f in files:
                print 'Copying %s -> %s' % (source, f)
                call(['cp', '-p', source, f])

    if not updated:
        print 'Nothing to update.'


if __name__ == '__main__':
    main()
