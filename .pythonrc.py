if 'pythonrc_guard' not in globals():
    import base64
    import json
    import os
    import pdb
    import re
    import sys
    import requests
    from subprocess import *
    from datetime import *
    from time import *
    from pprint import *
    from collections import *
    from concurrent.futures import *
    from base64 import *
    from uuid import *

    def pdir(obj):
        pprint(dir(obj))

    def attrs(obj):
        d = {}
        for name in dir(obj):
            d[name] = getattr(obj, name)
        pprint(d)

    def reloadall():
        """Reload all modules"""
        for x in list(sys.modules):
            if x != '__main__' and type(sys.modules[x]) == type(sys):
                reload(sys.modules[x])

    def b64j(_in):
        pprint(json.loads(base64.b64decode(_in)))

    # enable syntax completion
    def f():
        try:
            import readline
        except ImportError:
            print('Module readline not available.')
        else:
            import atexit
            import rlcompleter

            readline.parse_and_bind('tab: complete')
            path = os.path.expanduser('~/.pyhistory')

            atexit.register(lambda: readline.write_history_file(path))

            if os.path.exists(path):
                readline.read_history_file(path)
    f()
    del f

    # set color prompt, see http://ascii-table.com/ansi-escape-sequences.php
    sys.ps1 = '\001\033[32m\002>>>\001\033[0m\002 '
    sys.ps2 = '\001\033[32m\002...\001\033[0m\002 '

    # read local .pythonrc.py
    if os.path.exists('.pythonrc.py'):
        pythonrc_guard = None
        with open('.pythonrc.py') as f:
            exec(f.read())
        del pythonrc_guard
