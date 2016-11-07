from requests import request
import colors
import json
from pygments import highlight, lexers, formatters


def restcall(**kwargs):
    print colors.faint('=' * 120)
    r = request(**kwargs)

    print colors.green(r.request.method + ' ' + r.request.url)
    print_headers(r.request.headers)
    print

    if 'data' in kwargs:
        print colors.faint('Form data:')
        print_headers(kwargs['data'])
    elif 'json' in kwargs:
        print_json(kwargs['json'])
    else:
        print r.request.body

    print
    print colors.faint('({:.3f} seconds)'.format(r.elapsed.total_seconds()))
    print

    if r.status_code < 300:
        color = colors.green
    elif r.status_code < 400:
        color = colors.yellow
    else:
        color = colors.red
    print color('{} {}'.format(r.status_code, r.reason))
    print_headers(r.headers, format_names=True)
    print
    try:
        print_json(r.json())
    except:
        print r.text
    return r


def print_headers(headers, format_names=False):
    if headers is not None:
        for key in sorted(headers):
            value = headers[key]
            if format_names:
                key = format_header_name(key)
            print '{}: {}'.format(colors.magenta(key), value)


def print_json(obj):
    print highlight(
        json.dumps(obj, sort_keys=True, indent=4),
        lexers.JsonLexer(),
        formatters.TerminalFormatter()
    )


def format_header_name(name):
    return '-'.join(x.capitalize() for x in name.split('-'))
