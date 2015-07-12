# Vim startup script written in python.
import vim
import sys
from subprocess import Popen, call
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
color = 'darkgreen'
vim.command('hi LineNr ctermfg=' + color)
vim.command('au InsertEnter * hi LineNr ctermfg=black')
vim.command('au InsertEnter * hi LineNr ctermbg=' + color)
vim.command('au InsertLeave * hi LineNr ctermfg=' + color)
vim.command('au InsertLeave * hi LineNr ctermbg=black')

vim.command('autocmd BufRead * :py set_file_type()')


def do_keybindings():
    # Replace some built in vim commands with more useful commands.
    nmap(')', '/[)}\]({\[]<Enter>:noh<Enter>')
    nmap('(', '?[)}\]({\[]<Enter>:noh<Enter>')

    # Arrow Keys
    nmap('<S-Right>', '/\<[a-zA-Z_]<Enter>:noh<Enter>')
    nmap('<S-Left>', '?\<[a-zA-Z_]<Enter>:noh<Enter>')
    nmap('<C-Down>', '5<C-E>')
    nmap('<C-Up>', '5<C-Y>')

    # Control-keys
    imap('<C-s>', '<Esc>:w<Enter>')
    nmap('<C-s>', ':w<Enter>')

    # Alt-keys
    nmap('<Esc>o', 'O<Esc>')
    nmap('<Esc>R', ':source $MYVIMRC<Enter>')

    # Function keys
    nmap('<F2>', comment_line)
    nmap('<F3>', ':noh<Enter>')
    nmap('<F4>', uncomment_line)
    nmap('<F5>', ':%s/ \+$//g<Enter>:noh<Enter>')
    nmap('<F6>', ':checktime<Enter>')
    nmap('<F7>', toggle_overlength_highlight)
    nmap('<F8>', ':tabe .<Enter>')
    nmap('<F9>', exec_current_block)
    nmap('<F10>', exec_current_buffer)
    nmap('<F11>', python_shell)
    nmap('<F12>', ':!bash<Enter>')

    # Shift function keys
    nmap('<ESC>[1;2P', ':rightb vnew<Enter>')  # <S-F1>
    nmap('<ESC>[1;2Q', ':rightb new<Enter>')  # <S-F1>
    nmap('<ESC>[1;2R', pep8_first_error)  # <S-F3>

    # Make sure ^C toggles the line number colors the way escape would.
    inoremap('<C-c>', '<ESC>')


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


def toggle_overlength_highlight(print_message=True):
    global _ov_toggle
    if _ov_toggle:
        _ov_toggle = False
        vim.command('highlight clear OverLength')
        if print_message:
            print 'overlength highlight off'
    else:
        _ov_toggle = True
        vim.command('highlight OverLength '
                    'ctermbg=red ctermfg=white guibg=#592929')
        vim.command('match OverLength /\%80v.\+/')
        if print_message:
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
        line = int(error.split(':')[1])
        vim.current.window.cursor = line, 1
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


def map_func(map_cmd):
    def f(key, cmd):
        if hasattr(cmd, '__call__'):
            cmd = ':py {}()<Enter>'.format(cmd.__name__)
        vim.command('{} {} {}'.format(map_cmd, key, cmd))
    return f
nmap = map_func('nmap')
vmap = map_func('vmap')
imap = map_func('imap')
inoremap = map_func('inoremap')


do_keybindings()
