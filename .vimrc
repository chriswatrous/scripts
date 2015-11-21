pyf ~/.vimrc.py

"function! SetFiletypeOptions()
"    if &filetype == 'python' || &filetype == 'pyrex' || &filetype == 'sh' || &filetype == 'make' || &filetype == 'gitconfig' || &filetype == 'conf'
"       "commenting and uncommenting that uses '#'
"       nmap <F2> :norm I#<Enter>j
"       nmap <F4> :s/\(^\s*\)\@<=#/<Enter>:noh<Enter>j
"       vmap <F2> :norm I#<Enter>j
"       vmap <F4> :s/\(^\s*\)\@<=#/<Enter>:noh<Enter>j
"    endif
"    if &filetype == 'vim'
"       "commenting and uncommenting that uses '"'
"       nmap <F2> :norm I"<Enter>
"       nmap <F4> :s/\(^\s*\)\@<="/<Enter>:noh<Enter>
"       vmap <F2> :norm I"<Enter>
"       vmap <F4> :s/\(^\s*\)\@<="/<Enter>:noh<Enter>
"    endif
"    if &filetype == 'c' || &filetype == 'cpp' || &filetype == 'java'
"       "commenting and uncommenting that uses '//'
"       nmap <F2> :norm I//<Enter>
"       vmap <F2> :norm I//<Enter>
"       nmap <F4> :s/\(^\s*\)\@<=\/\//<Enter>:noh<Enter>
"       vmap <F4> :s/\(^\s*\)\@<=\/\//<Enter>:noh<Enter>
"    endif
"    if &filetype == 'haskell'
"       "commenting and uncommenting that uses '-- '
"       nmap <F2> :norm I-- <Enter>
"       vmap <F2> :norm I-- <Enter>
"       nmap <F4> :s/\(^\s*\)\@<=-- /<Enter>:noh<Enter>
"       vmap <F4> :s/\(^\s*\)\@<=-- /<Enter>:noh<Enter>
"    endif
"    if &filetype == 'autohotkey' || &filetype == 'dosini' || &filetype == 'asm' || &filetype == 'csound'
"       "commenting and uncommenting that uses ';'
"       nmap <F2> :norm I;<Enter>
"       vmap <F2> :norm I;<Enter>
"       nmap <F4> :s/\(^\s*\)\@<=;/<Enter>:noh<Enter>
"       vmap <F4> :s/\(^\s*\)\@<=;/<Enter>:noh<Enter>
"    endif
"    if &filetype == 'matlab'
"       "commenting and uncommenting that uses '%'
"       nmap <F2> :norm I%<Enter>
"       vmap <F2> :norm I%<Enter>
"       nmap <F4> :s/\(^\s*\)\@<=%/<Enter>:noh<Enter>
"       vmap <F4> :s/\(^\s*\)\@<=%/<Enter>:noh<Enter>
"    endif
"    if &filetype == 'make'
"        " GNU make needs actual tabs for indentation. Urgh.
"        set noexpandtab
"    else
"        set expandtab
"    endif
"endfunction
"
"au Filetype,BufEnter * call SetFiletypeOptions()
