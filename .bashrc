# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Make blue text readable
echo -ne '\e]4;4;#8080FF\a'   # blue
echo -ne '\e]4;12;#A0A0FF\a'  # bold blue

# History options
HISTCONTROL=erasedups:ignorespace
HISTFILESIZE=5000
HISTSIZE=5000
shopt -s histappend

# Update window size after commands
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Set color prompt. There may be a rare terminal where this doesn't work. I'll cross that bridge when I come to it.
# See http://ascii-table.com/ansi-escape-sequences.php
PS1='[\[\e[3;33m\]\u@\h \[\e[01;34m\]${PWD}\[\e[0m\]\[\e[33m\]\[\e[0m\]] '

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
export LESS='-M -r'
export PAGER=less
export PYTHONSTARTUP=~/.pythonrc.py
if [[ -e /cygdrive/c/Users/Chris ]]; then
    export PRINTER=DCP7040
    export PATH="${PATH}:/cygdrive/c/Program Files (x86)/SMPlayer"
    export PATH="${PATH}:/cygdrive/c/Program Files (x86)/Audacity"
fi
export PATH="~/bin:${PATH}:."

# stuff for work linux
if [[ -e /home/cwatrous ]]; then
    # make git push work on work linux
    unset SSH_ASKPASS

    # fix curl
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/usr/local/lib:/usr/local/lib64
fi


alias ls='ls -F --color=auto'
alias l='ls -alh'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias df='df -h'
alias du='du -h'
alias o="octave -q"
alias p=python
alias p3=python3
alias vi=vim
alias vim='vim -p'
alias tree='tree -C'
alias hd='hexdump -C'
alias pdb='python -m pdb'

# aliases for git
alias g='git status'
alias gb='git branch'
alias gd='git diff'

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

################## old color prompt stuff ########################################
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

