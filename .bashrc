# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Adjust Colors
#echo -ne '\e]4;4;#8080FF\a'   # blue
#echo -ne '\e]4;12;#A0A0FF\a'  # bold blue
echo -ne '\e]4;4;#7AA8DE\a'   # blue
echo -ne '\e]4;12;#85BDFF\a'  # bright blue
echo -ne '\e]4;2;#008000\a'   # green
echo -ne '\e]4;10;#00FF00\a'  # bright green

#85BDFFp

# History options
HISTCONTROL=erasedups:ignorespace
HISTFILESIZE=5000
HISTSIZE=5000
shopt -s histappend

# Update window size after commands
shopt -s checkwinsize

shopt -s dotglob

# which setterm &> /dev/null && setterm -blank 0

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Set color prompt. There may be a rare terminal where this doesn't work. I'll cross that bridge when I come to it.
# See http://ascii-table.com/ansi-escape-sequences.php
# Disble git branch in prompt if on cygwin.
if [ -z "$WINDIR" ]; then  # if WINDER is empty
    PS1='[\[\e[3;33m\]\u@\h \[\e[01;34m\]${PWD}\[\e[01;31m\]$(git-br)\[\e[0m\]] '
else
    PS1='[\[\e[3;33m\]\u@\h \[\e[01;34m\]${PWD}\[\e[0m\]] '
fi

# Non-color prompt
#PS1='[\u@\h ${PWD}] '

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# Try to enable bash completion
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi
complete -d cd

# Disable handling of ^s
stty -ixon

export EDITOR=vim
export LC_ALL=C
export LESS='-M -R'
export PAGER=less
export PYTHONSTARTUP=~/.pythonrc.py
export PYTHONIOENCODING=utf_8
export DB_NAME=chris_local

unset JAVA_TOOL_OPTIONS

if [[ -e /cygdrive/c/Users/Chris ]]; then
    export PRINTER=DCP7040
    export PATH="${PATH}:/cygdrive/c/Program Files (x86)/SMPlayer"
    export PATH="${PATH}:/cygdrive/c/Program Files (x86)/Audacity"
    export PATH="${PATH}:/cygdrive/c/Program Files (x86)/CSound6/bin"
fi

# export PYTHONPATH='.:..:../..:../../..:../../../..:../../../../..:../../../../../..:../../../../../../..'
export PATH="~/bin:~/stuff/bin:$PATH:."

export REQUEST_STATS_FILE=~/request_stats

alias ag='ag --color-match "1;31"'
alias df='df -Th'
alias du2='du -BM -d 1 | sort -n'
alias du='du -BM'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias grep='grep --color=auto'
alias hd='hexdump -C'
alias install='sudo apt-get install -y'
alias l='ls -alh'
alias ls='ls -F --color=auto'
alias o="octave -q"
alias pdb='python -m pdb'
alias tree='tree -C'
alias vi=vim
alias vim='vim -p'
alias wt='watch -n 1'

# Use ipython if it exists.
if which ipython &> /dev/null; then
    alias p=ipython
else
    alias p=python
fi
if which ipython3 &> /dev/null; then
    alias p3=ipython3
else
    alias p3=python3
fi

# aliases for git
alias g='git status'
alias gb='git branch'
alias gd='git diff'
alias gl='git log --decorate --graph'
alias gla='git log --decorate --graph --all --oneline'
alias gc='git checkout'
alias ga='git add -A :/'

# aliases for changing directories
alias cd='my_cd'
alias c.='cd ..'
alias c..='cd ../..'
alias c...='cd ../../..'
alias c....='cd ../../../..'
alias c.....='cd ../../../../..'
alias c......='cd ../../../../../..'
alias r=select_recent_dir

update_recent_dirs()
{
    recent_dirs=()
    while read -r line
    do
        recent_dirs+=("$line")
    done < "$HOME/.recent_dirs"
    new_dirs=()
    new_dirs+=("$PWD")
    count=0
    for s in "${recent_dirs[@]}"; do
        if [ "$PWD" != "$s" ] && [ $count -lt 20 ]; then
            new_dirs+=("$s")
        fi
        ((count++))
    done
    printf "%s\n" "${new_dirs[@]}" > ~/.recent_dirs
}

my_cd()
{
    if [ $# -lt 1 ]; then
        \cd && update_recent_dirs
    else
        \cd "$1" && update_recent_dirs
    fi
}

select_recent_dir()
{
    cat -n ~/.recent_dirs | tac
    read -p 'Enter the number of the directory to cd to: ' n
    num_lines=`wc -l < ~/.recent_dirs`
    if [[ $n =~ ^[0-9]+$ ]] && [ $n -gt 0 ] && [ $n -le $num_lines ]; then
        cd "`sed -n ${n}p ~/.recent_dirs`"
    else
        echo Bad number
    fi
}

# Record the working directory aftereach command and start new bash instances in that directory
export PROMPT_COMMAND='echo $PWD > ~/.last_dir'
if [ -e ~/.last_dir ]; then
    cd "`cat ~/.last_dir`"
fi


# git-br()
# {
#   set -f
#   var=`git branch 2> /dev/null | grep *`
#   var=${var:2}
#   if [ ! -z "$var" ]; then
#       var=" $var"
#   fi
#   echo "$var"
# }


# Maybe faster version. We'll see.
git-br()
{
    # Find the .git directory.
    while true; do
        if [ -e .git ]; then
            break
        fi
        last_pwd="$PWD"
        \cd ..  # Don't use the cd alias for my_cd.
        if [ "$last_pwd" = "$PWD" ]; then
            break
        fi
    done

    # Read .git/HEAD
    if [ -e .git ]; then
        head=`cat .git/HEAD`
        if [[ "$head" == "ref: refs/heads/"* ]]; then
            echo " "${head:16}
        else
            echo " "$head
        fi
    fi
}


################## old color prompt stuff #####################################
# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes
#
#if [ -n "$force_color_prompt" ]; then
    #if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	## We have color support; assume it's compliant with Ecma-48
	## (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	## a case would tend to support setf rather than setaf.)
	#color_prompt=yes
    #else
	#color_prompt=
    #fi
#fi
#
#if [ "$color_prompt" = yes ]; then
    #PS1='[\[\e[3;33m\]\u@\h \[\e[01;34m\]${PWD}\[\e[0m\]\[\e[33m\]\[\e[0m\]] '
#else
    #PS1='[\u@\h ${PWD}] '
#fi
#unset color_prompt force_color_prompt

