if 'sys' not in globals():
    import sys
    from pprint import *

    imports = [
        'base64',
        'collections',
        'datetime',
        'functools',
        'itertools',
        'json',
        'msgpack',
        'multiprocessing',
        'operator',
        'os',
        'pdb',
        'psutil',
        'pyrsistent',
        're',
        'requests',
        'signal',
        'socket',
        'sqlite3',
        'ssl',
        'statistics',
        'streams',
        'subprocess',
        'threading',
        'time',
        'urlparse',
        'uuid',
        'yaml',
    ]
    star_imports = [
        'concurrent.futures',
        'pyrsistent',
        'subprocess',
    ]

    for name in imports:
        try:
            exec('import ' + name)
        except ImportError as e:
            print(e)

    for name in star_imports:
        try:
            exec('from {} import *'.format(name))
        except ImportError as e:
            print(e)

    try:
        from streams import Stream
    except ImportError:
        pass

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
            exec(f.read(), globals())
        del pythonrc_guard
