# Vim startup script written in python.
import vim
import sys
from subprocess import Popen, call, PIPE
from time import sleep

# Set this option first.
vim.command('set nocompatible')

# Clear mappings and auto commands in case we are reloading the file.
vim.command('mapclear')
vim.command('imapclear')
vim.command('autocmd!')

# General options
vim.command('set backspace=indent,eol,start')
vim.command('set mouse=a')
vim.command('set nofoldenable')
vim.command('set number')
vim.command('set ruler')
vim.command('set scrolloff=5')
vim.command('set showtabline=2')
vim.command('syntax on')

# Swap dirs
swap_dirs = ['.', '~/.vim/backup']
vim.command('set directory=' + ','.join(swap_dirs))
vim.command('set backupdir=' + ','.join(swap_dirs))

# Search settings
vim.command('set hlsearch')
vim.command('nohlsearch')
vim.command('set ignorecase')
vim.command('set incsearch')
vim.command('set nowrapscan')
vim.command('set smartcase')

# Tab settings
vim.command('set autoindent')
vim.command('set expandtab')
vim.command('set shiftwidth=4')
vim.command('set smarttab')
vim.command('set softtabstop=4')
vim.command('set tabstop=4')

# Change line number colors in insert mode.
vim.command('au InsertEnter * hi LineNr ctermfg=0 ctermbg=darkgreen')
vim.command('au InsertLeave * hi LineNr ctermfg=darkgreen ctermbg=8')
vim.command('hi LineNr ctermfg=darkgreen ctermbg=8')

vim.command('autocmd BufRead * :py set_file_type()')


def do_keybindings():
    # Replace some built in vim commands with more useful commands.
    nnoremap(')', '/[)}\]({\[]<Enter>:noh<Enter>')
    nnoremap('(', '?[)}\]({\[]<Enter>:noh<Enter>')

    # Arrow Keys
    nnoremap('<S-Right>', '/\<[a-zA-Z_]<Enter>:noh<Enter>')
    nnoremap('<S-Left>', '?\<[a-zA-Z_]<Enter>:noh<Enter>')
    nnoremap('<C-Down>', '5<C-E>')
    nnoremap('<C-Up>', '5<C-Y>')

    # Control-keys
    inoremap('<C-s>', '<Esc>:w<Enter>')
    nnoremap('<C-s>', ':w<Enter>')

    # Don't use bind to alt-keys since alt+key sends ESC, key.

    # backslash bindings
    nnoremap('\\a', ':py copy_comment_line()<Enter>')
    nnoremap('\\b', ':set relativenumber!<Enter>')
    nnoremap('\\c', replace_string_contents)
    nnoremap('\\d', ':norm oimport ipdb<Enter>:norm oipdb.set_trace()<Enter>')
    nnoremap('\\o', 'O<Esc>')  # Insert blank line at cursor.
    nnoremap('\\r', ':tabe ~/.vimrc.py<Enter>')  # Edit .vimrc.py
    nnoremap('\\R', ':source $MYVIMRC<Enter>')  # Reload .vimrc

    # Function keys
    nnoremap('<F2>', comment_line)
    nnoremap('<F3>', ':noh<Enter>')
    nnoremap('<F4>', uncomment_line)
    nnoremap('<F5>', ':%s/ \+$//g<Enter>:noh<Enter>')
    nnoremap('<F6>', ':checktime<Enter>')
    nnoremap('<F7>', toggle_overlength_highlight)
    nnoremap('<F8>', ':tabe .<Enter>')  # Open file in new tab, starting from current working directory.
    nnoremap('<F9>', exec_current_block)
    nnoremap('<F10>', exec_current_buffer)
    nnoremap('<F11>', python_shell)
    nnoremap('<F12>', ':!bash<Enter>')

    # Shift function keys
    nnoremap('<Esc>[1;2P', ':rightb vnew<Enter>')  # <S-F1> New file in vertical split to right.
    nnoremap('<Esc>[1;2Q', ':rightb new<Enter>')  # <S-F2> New file in horizontal split below.
    nnoremap('<Esc>[1;2R', pep8_first_error)  # <S-F3> Jump to first pep8 error.
    nnoremap('<Esc>[19;2~', ':tabe ~/<Enter>')  # <S-F8> Open file in new tab, starting from home directory.

    # Make sure ^C toggles the line number colors the way escape would.
    inoremap('<C-c>', '<ESC>')


def replace_string_contents():
    """
    Delete the contents of a string literal and go into insert mode.

    Does not work for string literals spanning more than one line or
    for Python tripple quoted string literals.
    """
    in_string = []
    current_quote = None
    last_char = ''
    for char in vim.current.line:
        if current_quote == None:
            if char in ['"', "'"]:
                current_quote = char
            in_string.append(False)
        else:
            if char == current_quote and last_char != '\\':
                current_quote = None
                in_string.append(False)
            else:
                in_string.append(True)
        last_char = char
    _, pos = vim.current.window.cursor
    if not in_string[pos]:
        print 'not currently in a string literal'
    else:
        pos1 = pos
        while in_string[pos1 - 1]:
            pos1 -= 1
        pos2 = pos
        while in_string[pos2 + 1]:
            pos2 += 1
        print vim.current.line[pos1 : pos2 + 1]


# 'asdfsd' "weqrwqerqwe" 'as"dfsd' "weqr'wqerqwe" 'as\'dfsd' "weqr\"wqerqwe"


def set_file_type():
    if vim.current.buffer.name.endswith('SConstruct'):
        vim.command('set filetype=python')


def exec_current_buffer():
    filetype = vim.eval('&filetype')
    if filetype != 'python':
        sys.stderr.write('Unsupported file type: ' + filetype)
        return
    script = '\n'.join(vim.current.buffer[:])
    exec script in globals()


def exec_current_block():
    filetype = vim.eval('&filetype')
    if filetype != 'python':
        sys.stderr.write('Unsupported file type: ' + filetype)
        return
    lines = vim.current.buffer[:]

    # Find start of block.
    L1 = vim.current.window.cursor[0] - 1
    while (lines[L1].startswith(' ') or
           lines[L1].startswith('#') or
           lines[L1] == ''):
        L1 -= 1

    # Find end of block.
    L2 = vim.current.window.cursor[0]
    while L2 < len(lines) and (lines[L2].startswith(' ') or
                               lines[L2].startswith('#') or
                               lines[L2] == ''):
        L2 += 1

    script = '\n'.join(lines[L1:L2])
    exec script in globals()


_ov_toggle = False


def toggle_overlength_highlight():
    global _ov_toggle
    if _ov_toggle:
        _ov_toggle = False
        vim.command('highlight clear OverLength')
        print 'overlength highlight off'
    else:
        _ov_toggle = True
        vim.command('highlight OverLength '
                    'ctermbg=red ctermfg=white guibg=#592929')
        vim.command('match OverLength /\%80v.\+/')
        print 'overlength highlight on'


def python_shell():
    if call('which ipython &> /dev/null', shell=True) == 0:
        vim.command('!ipython')
    else:
        vim.command('!python')


def pep8_first_error():
    script = '\n'.join(vim.current.buffer[:]) + '\n'
    proc = Popen(['pep8', '-'], stdin=PIPE, stdout=PIPE)
    out, _ = proc.communicate(script)
    if proc.wait() == 0:
        print 'no pep8 errors'
    else:
        error = out.split('\n')[0]
        row, col = (int(x) for x in error.split(':')[1:3])
        vim.current.window.cursor = row, col - 1
        print error


def comment_line():
    text = vim.current.line
    if text == '':
        text = '#'
    elif text.startswith('  '):
        text = '# ' + text[2:]
    else:
        text = '# ' + text
    vim.current.line = text
    move_by(1, 0)


def uncomment_line():
    text = vim.current.line
    if text == '#':
        text = ''
    elif text.startswith('#'):
        text = ' ' + text[1:]
        if len(text) - len(text.lstrip()) < 4:
            text = text.lstrip()
    elif text.lstrip().startswith('#'):
        text = text.replace('#', '', 1)
    vim.current.line = text
    move_by(1, 0)


def copy_comment_line():
    row, col = vim.current.window.cursor
    vim.command('norm yypk')
    comment_line()
    vim.current.window.cursor = (row + 1, col)


def move_by(rows, columns):
    row, col = vim.current.window.cursor
    row += rows
    col += columns
    try:
        vim.current.window.cursor = (row, col)
    except:
        pass


def inspect(obj):
    print type(obj)
    print obj
    for attr in dir(obj):
        try:
            print '{}: {}'.format(attr, getattr(obj, attr))
        except:
            print '{}: could not get value'.format(attr)


key_codes = {
    '<S-F1>': '<Esc>[1;2P',
    '<S-F2>': '<Esc>[1;2Q',
    '<S-F3>': '<Esc>[1;2Q',
    '<S-F4>': '<Esc>[1;2S',
    '<S-F5>': '<Esc>[15;2~',
    '<S-F6>': '<Esc>[17;2~',
    '<S-F7>': '<Esc>[18;2~',
    '<S-F8>': '<Esc>[19;2~',
    '<S-F9>': '<Esc>[20~',
    '<S-F10>': '<Esc>[21~',
    '<S-F11>': '<Esc>[23~',
    '<S-F12>': '<Esc>[24~',
    }


def map_func(map_cmd):
    def f(key, cmd):
        if hasattr(cmd, '__call__'):
            cmd = ':py {}()<Enter>'.format(cmd.__name__)
        vim.command('{} {} {}'.format(map_cmd, key, cmd))
    return f
nnoremap = map_func('nnoremap')
vnoremap = map_func('vnoremap')
inoremap = map_func('inoremap')


do_keybindings()
