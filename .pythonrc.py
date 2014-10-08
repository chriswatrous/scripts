if 'pythonrc_guard' not in globals():
    import os, sys
    from subprocess import call
    from pprint import *

    def pdir(obj):
        pprint(dir(obj))

    def reloadall():
        "Reload all modules"
        for x in list(sys.modules):
            if x != '__main__' and type(sys.modules[x]) == type(sys):
                reload(sys.modules[x])

    # enable syntax completion
    try:
        import readline
    except ImportError:
        print("Module readline not available.")
    else:
        import rlcompleter
        readline.parse_and_bind("tab: complete")


    # set color prompt, see http://ascii-table.com/ansi-escape-sequences.php
    sys.ps1 = '\001\033[32m\002>>>\001\033[0m\002 '
    sys.ps2 = '\001\033[32m\002...\001\033[0m\002 '

    # read local .pythonrc.py
    if os.path.exists('.pythonrc.py'):
        pythonrc_guard = None
        with open('.pythonrc.py') as f:
            exec(f.read())
        del pythonrc_guard

