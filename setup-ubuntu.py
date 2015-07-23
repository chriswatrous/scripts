#!/usr/bin/python
from os.path import exists, expanduser, join, islink
import subprocess
from subprocess import CalledProcessError, check_output
import sys

homedir = expanduser('~')
failed_calls = []


def main():
    # Setup scripts.
    call(['sudo', 'apt-get', 'install', 'git'])
    if not exists(join(homedir, 'scripts')):
        call(['git', 'clone', 'https://github.com/chriswatrous/scripts'])

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
    apt_get_packages = [
        'ardour',
        'audacity',
        'curl',
        'flashplugin-installer',
        'gconf-editor',
        'geda',
        'ghc',
        'htop',
        'jack',
        'octave',
        'python-pip',
        'python3',
        'python3-pip',
        'sshfs',
        'terminator',
        'tree',
        'unity-tweak-tool',
        'vim-gtk',
        'mesa-utils',
        ]
    for package in apt_get_packages:
        call(['sudo', 'apt-get', 'install', '-y', package])
    call(['sudo', 'apt-get', 'update', '-y'])
    call(['sudo', 'apt-get', 'upgrade', '-y'])

    install_chrome()

    # Install python packages.
    call(['sudo', 'pip', 'install', '--upgrade', 'pip'])
    pip_packages = [
        'behave',
        'flask',
        'pep8',
        'requests',
        'ipython',
        'ipdb',
        ]
    for package in pip_packages:
        call(['sudo', 'pip', 'install', package])
        call(['sudo', 'pip3', 'install', package])

    # Check for failed calls.
    if failed_calls:
        print 'The following commands failed:'
        for cmd in failed_calls:
            print '    ' + str(cmd)


def install_chrome():
    try:
        output = check_output(['sudo', 'apt-key', 'list'])
        if not 'Google, Inc. Linux' in output:
            check_call(
                'curl https://dl-ssl.google.com/linux/linux_signing_key.pub | '
                'sudo apt-key add -', shell=True)

        path = '/etc/apt/sources.list.d/google.list'
        if exists(path):
            with open(path, 'r') as f:
                text = f.read()
        else:
            text = ''
        if not 'http://dl.google.com/linux/chrome/deb/ stable main' in text:
            check_call([
                'sudo', 'bash', '-c', 
                'echo '
                '"deb http://dl.google.com/linux/chrome/deb/ stable main"'
                '>> /etc/apt/sources.list.d/google.list'])
        check_call(['sudo', 'apt-get', 'update'])
        check_call([
            'sudo', 'apt-get', 'install', '-y', 'google-chrome-stable'])
    except CalledProcessError:
        pass


def setup_script(script_name):
    path = join(homedir, script_name)
    if exists(path) and islink(path):
        return
    if exists(path):
        call(['mv', path, path + '_original'])
    call(['ln', '-s', join(homedir, 'scripts', script_name), homedir])


def call(cmd, shell=False):
    print cmd
    status_code = subprocess.call(cmd, shell=shell)
    if status_code != 0:
        failed_calls.append(cmd)
    return status_code


def check_call(cmd, shell=False):
    r = call(cmd, shell=shell)
    if r != 0:
        raise CalledProcessError(r, str(cmd))




if __name__ == '__main__':
    main()
