# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[[ "$-" != *i* ]] && return

# Adjust Colors
if [[ "$TERM" == xterm ]]; then
    #echo -ne '\033]4;4;#8080FF\a'   # blue
    #echo -ne '\033]4;12;#A0A0FF\a'  # bold blue
    echo -ne '\033]4;4;#7AA8DE\a'   # blue
    echo -ne '\033]4;12;#85BDFF\a'  # bright blue
    echo -ne '\033]4;2;#008000\a'   # green
    echo -ne '\033]4;10;#00FF00\a'  # bright green
fi

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


################################################################################

# Disable handling of ^s
stty -ixon

stty erase ^?

export LC_ALL=C
export LESS='-M -R -c'
export PAGER=less
export PYTHONSTARTUP=~/.pythonrc.py
export PYTHONIOENCODING=utf_8
export PYTHONPATH=.:~/scripts/pylib
export GOPATH="${HOME}/.go"

if [[ -e /cygdrive/c/Users/Chris ]]; then
    export PRINTER=DCP7040
fi

# Work related stuff.
# export DB_NAME=chris_local
export NO_LOG_HEADER=true
export LOGGING_206_AS_ERROR=True
export REQUEST_STATS_FILE=~/request_stats
export CFS_LOGS_DIR=~/projects/cams/cfs-python-utils/logs
export GCC_COLORS='error=01;31:warning=01;33:note=01;36:caret=01;32:locus=01:quote=01'
export NVM_DIR=~/.nvm

if which brew &> /dev/null; then
    nvm_script="$(brew --prefix nvm)/nvm.sh"
    if [ -e $nvm_script ]; then
        source $nvm_script
        # nvm use v4.4.4
    fi
fi

# Set default editor.
if [ -e ~/scripts/bin/find-editor ]; then
    export EDITOR="$HOME/scripts/bin/find-editor"
else
    export EDITOR=vim
fi
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"

unset JAVA_TOOL_OPTIONS


# PATH setup ###################################################################

showpath() {
    echo $PATH | tr ":" "\n"
}

path_prepend() {
    export PATH="$1:$PATH"
}

path_append() {
    export PATH="$PATH:$1"
}

export PATH=$(~/scripts/path.py)

# path_append "."

# virtualenv setup #############################################################
#
# This must be done after path setup
#

if [ -e ~/venv/bin/activate ]; then
    source ~/venv/bin/activate
fi

# Prompt setup #################################################################

# Set color prompt. There may be a rare terminal where this doesn't
# work. I'll cross that bridge when I come to it.
# See http://ascii-table.com/ansi-escape-sequences.php
# Disble git branch in prompt if on cygwin.
if [ -z "$WINDIR" ]; then
    cyan='\[\033[36m\]'
    yellow='\[\033[33m\]'
    PS1="${cyan}bash> ${yellow}"

    prompt_command() {
        echo $PWD > ~/.last_dir
        ~/scripts/prompter/prompter
    }
    PROMPT_COMMAND='prompt_command'
else
    # My go prompter program doen't work in Cygwin at the moment.
    PS1='[\[\033[3;33m\]\u@\h \[\033[01;34m\]${PWD}\[\033[0m\]] '

    prompt_command() {
        echo $PWD > ~/.last_dir
    }
fi

# Reset the terminal color before every command.
trap 'echo -n -e "\033[0m"' DEBUG

# Non-color prompt
#PS1='[\u@\h ${PWD}] '

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\033]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# bash completion ##############################################################

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
if which brew &> /dev/null && [ -f $(brew --prefix)/etc/bash_completion ]; then
    . $(brew --prefix)/etc/bash_completion
fi
complete -d cd

# Aliases ######################################################################

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
alias o="octave -q --no-gui"
alias pdb='python -m pdb'
alias tree='tree -C'
alias vi=vim
alias vim='vim -p'
alias wt='watch -n 1'
alias ports='netstat -tulpn'
alias psy='ps -ef | grep -i python'
alias psyt='ps -eLf | grep -i python'  # only works on linux
alias psf='ps -ef | grep -i'
alias jc='source /home/chris/gitrepos/cams/cams-dist/jenkins-config-manager/jenkins-creds.sh'
alias kpy='killall -9 python Python'

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

# git aliases ##################################################################

# show status in short foramt
alias g='git status --short'

# show local and remote branches with current commit ids and messages
alias gb='git branch -a -vv --color=always | perl -pe '\''s/^((?:(?>(?:\033\[.*?m)*).){'\''${COLUMNS}'\''}).*/$1\033[m/'\'

# diff between staging area and working dir
alias gd='git diff'

# Show log
alias gl='git log --decorate --graph'

# Show log with all branches in a more compact format
alias gla='git log --decorate --graph --all --oneline'

# Add all modified, new, and deleted files.
alias ga='git add -A :/'

# Put current branch on fork.
alias git-pub='git push -u origin `git rev-parse --abbrev-ref HEAD`'

# Fetch all branches and remove deleted remote branches.
alias gf='git fetch --all --prune; gb'

alias gc='git checkout'
alias gd1='git diff `git merge-base upstream/master HEAD` HEAD'
alias gd2='git diff `git merge-base upstream/master HEAD` .'
alias gd3='git --no-pager diff --stat `git merge-base upstream/master HEAD` .'

git-brd()
{
    git branch -d $1 && git push --delete origin $1
}

# aliases for changing directories
alias cd='my_cd'
alias c.='cd ..'
alias c..='cd ../..'
alias c...='cd ../../..'
alias c....='cd ../../../..'
alias c.....='cd ../../../../..'
alias c......='cd ../../../../../..'
alias r=select_recent_dir

# Common directories
alias c1='cd ~/projects/cams/cfs-python-utils'
alias c2='cd ~/projects/cams/acs-utils'
alias c3='cd ~/projects/cams/cams-config'
alias c4='cd ~/projects/cams/cams-test'
alias c5='cd ~/projects/cams/cams-dist/jenkins-config-manager'
alias c6='cd ~/projects/cams/cams-pdp'
alias c7='cd ~/projects/cams/access-management'
alias c8='cd ~/projects/cams/PEP_python_lib'
alias c9='cd ~/projects/cams/PEP_node_lib'


################################################################################

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

cdpy()
{
    cd $(python2 -c "from os.path import dirname; import $1; print dirname($1.__file__)")
}

cdpy3()
{
    cd $(python3 -c "from os.path import dirname; import $1; print(dirname($1.__file__))")
}


# Record the working directory aftereach command and start new bash instances in that directory
if [ -e ~/.last_dir ]; then
    cd "`cat ~/.last_dir`"
fi


if [ -e ~/.bashrc_local ]; then
    source ~/.bashrc_local
fi


if [ "$PWM_ASK_PASSWORD" == "true" ] && [ -z "$PWM_PASSWORD" ]; then
    read -s -p 'Enter pwm master password: ' PWM_PASSWORD
    echo
    export PWM_PASSWORD
fi
if [ -n "$PWM_PASSWORD" ]; then
    PS1='(pwm) '"$PS1"
fi

alias pw='PWM_ASK_PASSWORD=true bash'


if [ -e ~/.bashrc-local ]; then
    source ~/.bashrc-local
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
# git-br()
# {
#     # Find the .git directory.
#     while true; do
#         if [ -e .git ]; then
#             break
#         fi
#         last_pwd="$PWD"
#         \cd ..  # Don't use the cd alias for my_cd.
#         if [ "$last_pwd" = "$PWD" ]; then
#             break
#         fi
#     done

#     # Read .git/HEAD
#     if [ -e .git ]; then
#         head=`cat .git/HEAD`
#         if [[ "$head" == "ref: refs/heads/"* ]]; then
#             echo " "${head:16}
#         else
#             echo " "$head
#         fi
#     fi
# }


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
    #PS1='[\[\033[3;33m\]\u@\h \[\033[01;34m\]${PWD}\[\033[0m\]\[\033[33m\]\[\033[0m\]] '
#else
    #PS1='[\u@\h ${PWD}] '
#fi
#unset color_prompt force_color_prompt
