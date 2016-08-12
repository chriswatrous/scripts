#!/usr/bin/env python
import subprocess
import sys
import os
from os.path import exists, expanduser, join, islink, dirname, realpath
from subprocess import CalledProcessError, check_output
from argparse import ArgumentParser

this_script_dir = dirname(realpath(__file__))
script_files_dir = dirname(this_script_dir)
homedir = expanduser('~')

failed_calls = []


def main():
    parser = ArgumentParser()
    parser.add_argument('install_type', choices=['workstation', 'server'])

    options = parser.parse_args()

    setup_script('.bashrc')
    setup_script('.ghci')
    setup_script('.gvimrc')
    setup_script('.octaverc')
    setup_script('.pdbrc')
    setup_script('.pythonrc.py')
    setup_script('.vimrc')
    setup_script('.vimrc.py')
    setup_script('setup-ubuntu.py')

    # Install ubuntu packages.
    for package in get_packages(options.install_type):
        call(['sudo', 'apt-get', 'install', '-y', package])
    call(['sudo', 'apt-get', 'update', '-y'])
    call(['sudo', 'apt-get', 'upgrade', '-y'])

    # Install python packages.
    call(['sudo', 'pip', 'install', '--upgrade', 'pip'])
    package_file = join(this_script_dir, 'pip-packages')
    call(['sudo', 'pip', 'install', '--requirement', package_file])
    call(['sudo', 'pip3', 'install', '--requirement', package_file])
    for line in check_output(['pip', 'list']).strip('\n').split('\n'):
        package = line.split('(')[0].strip()
        call(['sudo', 'pip', 'install', '--upgrade', package])
        call(['sudo', 'pip3', 'install', '--upgrade', package])

    # Check for failed calls.
    if failed_calls:
        print 'The following commands failed:'
        for cmd in failed_calls:
            print '    ' + str(cmd)


def setup_script(script_name):
    file_path = join(script_files_dir, script_name)
    link_path = join(homedir, script_name)
    if not islink(link_path):
        if exists(link_path):
            call(['mv', link_path, link_path + '_original'])
        call(['ln', '-s', file_path, link_path])


def call(cmd, shell=False):
    print cmd
    status_code = subprocess.call(cmd, shell=shell)
    if status_code != 0:
        failed_calls.append(cmd)
    return status_code


def get_packages(install_type):
    packages = {}
    for line in open(join(this_script_dir, 'apt-get-packages')):
        line = line.strip('\n')
        if line and line[0] not in [' ', '\t'] and line.endswith(':'):
            line = line[:-1]
            assert line in ['Both', 'Server', 'Workstation'], section
            section = line
            packages[section] = []
        else:
            line = line.strip()
            if line:
                packages[section].append(line)
    if install_type == 'server':
        return packages['Both'] + packages['Server']
    elif install_type == 'workstation':
        return packages['Both'] + packages['Workstation']
    else:
        explode


def check_call(cmd, shell=False):
    r = call(cmd, shell=shell)
    if r != 0:
        raise CalledProcessError(r, str(cmd))


if __name__ == '__main__':
    main()
