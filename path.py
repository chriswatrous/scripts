#!/usr/bin/env python
import sys
import os
from os.path import exists


def uniq(L):
    out = []
    seen = set()
    for item in L:
        if item not in seen:
            out.append(item)
            seen.add(item)
    return out


paths = [
    '~/bin',
    '~/stuff/bin',
    '~/scripts/bin',
    '/usr/local/opt/coreutils/libexec/gnubin',
    '/home/chris/local-stuff/install/ghc/bin',
    '/Applications/Emacs.app/Contents/MacOS/libexec-x86_64-10_9',
    '/Applications/Emacs.app/Contents/MacOS/bin-x86_64-10_9',
    '/usr/local/bin',
] + os.getenv('PATH', '').split(':') + [
    '~/projects/cams/cams-test/tools',
    '~/.go/bin',
    '.',
]

if exists('/cygdrive/c/Users/Chris'):
    paths = paths + [
        '/cygdrive/c/Program Files (x86)/SMPlayer',
        '/cygdrive/c/Program Files (x86)/Audacity',
        '/cygdrive/c/Program Files (x86)/CSound6/bin',
    ]

paths = uniq(paths)

sys.stdout.write(':'.join(paths))
sys.stdout.flush()