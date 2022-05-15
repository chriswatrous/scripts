alias ag='ag --color-match "1;31"'
alias agj='ag -G .java$'
alias agpy='ag -G .py$'

alias df='df -Th'

alias du2='du -BM -d 1 | sort -n'
alias du='du -BM'

alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'

alias hd='hexdump -C'

alias tree='tree -C'

alias vi=vim
alias vim='vim -p'

alias wt='watch -n 1'

alias psf='ps -ef | grep -i'

alias uuid='python -c "import uuid; print uuid.uuid4().hex"'

alias diff='git --no-pager diff --no-index'

alias p='python -m IPython || python'
alias p3='python3 -m IPython || python3'
alias pdb='python -m pdb'

alias c.='cd ..'
alias c..='cd ../..'
alias c...='cd ../../..'
alias c....='cd ../../../..'
alias c.....='cd ../../../../..'
alias c......='cd ../../../../../..'

alias dcim='docker images | grep -v REPOSITORY | sort'
alias dcps='docker ps -a'

alias only='ag "\.only\b"'

alias ls='ls --color=auto'
alias l='ls -alh'
