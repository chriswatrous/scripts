#! /usr/bin/python

import os
from subprocess import call

# change to the directory of this script
os.chdir(os.path.dirname(os.path.realpath(__file__)))

installed = os.path.expanduser('~/.bashrc')
checkout = os.path.realpath('.bashrc')

installed_mtime = os.stat(installed).st_mtime
checkout_mtime = os.stat(checkout).st_mtime

if installed_mtime > checkout_mtime:
    print 'Copying %s to %s' % (installed, checkout)
    call(['cp', '-p', installed, checkout])
elif installed_mtime < checkout_mtime:
    print 'Copying %s to %s' % (checkout, installed)
    call(['cp', '-p', checkout, installed])
