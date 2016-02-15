from subprocess import call, DEVNULL


def run_editor(cmds):
    for cmd in cmds:
        if program_exists(cmd[0]):
            call(cmd)
            break
    else:
        print('No editors found (tried {})'
              .format(', '.join(x[0] for x in cmds)))
        sys.exit(1)


def program_exists(name):
    return call(['which', name], stdout=DEVNULL, stderr=DEVNULL) == 0
