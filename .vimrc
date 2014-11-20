" set this at the very beginning
set nocompatible
autocmd!

set backspace=indent,eol,start
set guifont=DejaVu_Sans_Mono:h10:cANSI
set mouse=a
set number
set ruler
set scrolloff=5
set timeoutlen=200
set viminfo='100,f1
set guioptions=eg
set directory=C:/Users/Chris/vimbackup,C:/Users/cwatrous/vimbackup,~/.vimbackup,.
set backupdir=C:/Users/Chris/vimbackup,C:/Users/cwatrous/vimbackup,~/.vimbackup,.
set showtabline=2

" search settings
set hlsearch
set ignorecase
set incsearch
set nowrapscan
set smartcase

" tab settings
set autoindent
set expandtab
set smarttab
set shiftwidth=4
set softtabstop=4
set tabstop=4

syntax on

function! SetFiletypeOptions()
    if &filetype == 'python' || &filetype == 'pyrex' || &filetype == 'sh' || &filetype == 'make' || &filetype == 'gitconfig'
       "commenting and uncommenting that uses '#'
       nmap <F2> :norm I#<Enter>
       nmap <F4> :s/\(^\s*\)\@<=#/<Enter>:noh<Enter>
       vmap <F2> :norm I#<Enter>
       vmap <F4> :s/\(^\s*\)\@<=#/<Enter>:noh<Enter>
    endif
    if &filetype == 'vim'
       "commenting and uncommenting that uses '"' 
       nmap <F2> :norm I"<Enter>
       nmap <F4> :s/\(^\s*\)\@<="/<Enter>:noh<Enter>
       vmap <F2> :norm I"<Enter>
       vmap <F4> :s/\(^\s*\)\@<="/<Enter>:noh<Enter>
    endif
    if &filetype == 'c' || &filetype == 'cpp'
       "commenting and uncommenting that uses '//'
       nmap <F2> :norm I//<Enter>
       vmap <F2> :norm I//<Enter>
       nmap <F4> :s/\(^\s*\)\@<=\/\//<Enter>:noh<Enter>
       vmap <F4> :s/\(^\s*\)\@<=\/\//<Enter>:noh<Enter>
    endif
    if &filetype == 'haskell'
       "commenting and uncommenting that uses '-- '
       nmap <F2> :norm I-- <Enter>
       vmap <F2> :norm I-- <Enter>
       nmap <F4> :s/\(^\s*\)\@<=-- /<Enter>:noh<Enter>
       vmap <F4> :s/\(^\s*\)\@<=-- /<Enter>:noh<Enter>
    endif
    if &filetype == 'autohotkey' || &filetype == 'dosini'
       "commenting and uncommenting that uses ';'
       nmap <F2> :norm I;<Enter>
       vmap <F2> :norm I;<Enter>
       nmap <F4> :s/\(^\s*\)\@<=;/<Enter>:noh<Enter>
       vmap <F4> :s/\(^\s*\)\@<=;/<Enter>:noh<Enter>
    endif
    if &filetype == 'make'
        " GNU make needs actual tabs for indentation. Urgh.
        set noexpandtab
    else
        set expandtab
    endif
endfunction

au Filetype,BufEnter * call SetFiletypeOptions()

" GUI starts maximized
au GUIEnter * simalt ~x

nmap ) /[)}\]({\[]<Enter>:noh<Enter>
nmap ( ?[)}\]({\[]<Enter>:noh<Enter>
nmap <S-Right> /\<[a-zA-Z_]<Enter>:noh<Enter>
nmap <S-Left> ?\<[a-zA-Z_]<Enter>:noh<Enter>
nmap <C-Down> 5<C-E>
nmap <C-Up> 5<C-Y>
nmap <F3> :noh<Enter>

" quick saving
imap <C-s> :w
nmap <C-s> :w

" check open files for modification outside the editor
nmap <F6> :checktime<Enter>

" change text color when in insert mode
inoremap <C-c> <ESC>
au InsertEnter * hi LineNr ctermfg=cyan
au InsertLeave * hi LineNr ctermfg=darkyellow

" Highlight all instances of word under cursor, when idle.
" Useful when studying strange source code.
" Type z/ to toggle highlighting on/off.
" from http://vim.wikia.com/wiki/Auto_highlight_current_word_when_idle
nnoremap z/ :if AutoHighlightToggle()<Bar>set hls<Bar>endif<CR>
function! AutoHighlightToggle()
    let @/ = ''
    if exists('#auto_highlight')
        au! auto_highlight
        augroup! auto_highlight
        setl updatetime=4000
        echo 'Highlight current word: off'
        return 0
    else
        augroup auto_highlight
            au!
            au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
        augroup end
        setl updatetime=200
        echo 'Highlight current word: ON'
        return 1
    endif
endfunction
