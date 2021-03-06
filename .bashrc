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

export LC_ALL=en_US.UTF-8
export LESS='-M -R -c -i'
export PAGER=less
export PYTHONSTARTUP=~/.pythonrc.py
export PYTHONIOENCODING=utf_8
export PYTHONPATH=.:~/scripts/pylib
export GOPATH="${HOME}/.go"

nvm() {
    # This function will be replaced by the real nvm.
    source $(brew --prefix nvm)/nvm.sh
    nvm "$@"
}

# Set default editor.
# function use-editor-atom() {
#   echo 'atom --wait' > ~/.editor
# }
# function use-editor-vim() {
#   echo vim > ~/.editor
# }
export EDITOR=vim
export VISUAL="$EDITOR"
export GIT_EDITOR="$EDITOR"

unset JAVA_TOOL_OPTIONS

# Prompt setup #################################################################

prompt_command() {
    echo $PWD > ~/.last_dir
    ~/scripts/prompter/prompter
}
PROMPT_COMMAND='prompt_command'

cyan='\[\033[36m\]'
yellow='\[\033[33m\]'

PS1="${cyan}bash> ${yellow}"

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


# virtualenv setup #############################################################
#
# This must be done after path setup
#
old_ps1="$PS1"
if [ "$USE_VENV_IN_CURRENT_DIR" == "true" ]; then
    if [ ! -e venv/bin/activate ]; then
        echo 'venv/bin/activate not found'
        exit 1
    fi
    source venv/bin/activate
    PS1="($(basename "$PWD") venv) $old_ps1"
    unset USE_VENV_IN_CURRENT_DIR
elif [ -e ~/venv/bin/activate ]; then
    source ~/venv/bin/activate
    PS1="$old_ps1"
fi
unset old_ps1

va() {
    export USE_VENV_IN_CURRENT_DIR=true
    bash
}


# Aliases ######################################################################

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
alias i=idea
alias findlogs="find -regex '.*\.log\(\.[0-9]+\)?'"
alias gr=gradle
alias tokens='python -m acs_utils.build.get_test_info'
alias uuid='python -c "import uuid; print uuid.uuid4().hex"'
alias n='node'
alias diff='git --no-pager diff --no-index'
alias pwgen2='python -c "import base64, uuid; print base64.b64encode(uuid.uuid4().bytes).strip(\"=\")"'
alias nvm.='nvm use $(cat package.json | jq -r .engines.node)'

alias itest-mccp='itest https://mccp.ng.bluemix.net/info'
alias itest-mccp-staging='itest https://mccp.stage1.ng.bluemix.net/info'
alias itest-mccp-london='itest https://mccp.eu-gb.bluemix.net/info'

alias grasp='~/.nvm/versions/node/v10.7.0/bin/node ~/.nvm/versions/node/v10.7.0/lib/node_modules/grasp/bin/grasp'
alias graspr='grasp -r --exclude "node_modules/**"'

# Use ipython if it exists.
alias p='python -m IPython || python'
alias p3='python3 -m IPython || python3'

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
alias gf='git fetch --all --prune && gb'

alias gd1='git diff `git merge-base upstream/master HEAD` HEAD'
alias gd2='git diff `git merge-base upstream/master HEAD` .'
alias gd3='git --no-pager diff --stat `git merge-base upstream/master HEAD` .'
alias git-show-merged='git log --decorate | grep "commit.*(.*)"'
alias gu='git checkout upstream/master'
alias gru='git rebase upstream/master'
alias git-ignored='git ls-files . --ignored --exclude-standard --others'

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
alias c0='cd ~/projects/cams'
alias c1='cd ~/projects/cams/cfs-python-utils'
alias c2='cd ~/projects/cams/acs-utils'
alias c3='cd ~/projects/cams/cams-config'
alias c4='cd ~/projects/cams/cams-test'
alias c5='cd ~/projects/cams/jenkins-config-manager'
alias c6='cd ~/projects/cams/cams-pdp'
alias c7='cd ~/projects/cams/access-management'
alias c8='cd ~/projects/cams/xacml-ms'

# docker aliases
alias dcim='docker images | grep -v REPOSITORY | sort'
alias dcps='docker ps -a'

alias only='ag "\.only\b"'

alias kimages='kubectl describe deployments | grep Image'


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

if [ -e ~/.bashrc-local.sh ]; then
    source ~/.bashrc-local.sh
fi

if [ -e /usr/local/Bluemix/bx/bash_autocomplete ]; then
    source /usr/local/Bluemix/bx/bash_autocomplete
fi

export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

export IKS_BETA_VERSION=1
