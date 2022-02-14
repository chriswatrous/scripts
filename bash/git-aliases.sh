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

# Publish branch.
# alias git-pub='(name=`git rev-parse --abbrev-ref HEAD`; git push -u origin ${name}:clwatrou/${name})'
function git-pub {
  local name=`git rev-parse --abbrev-ref HEAD`
  if [[ "$name" != clwatrou/* ]]; then
    name="clwatrou/$name"
    echo "changing branch name to $name"
    git branch -m $name
  fi
  git push -u origin $name
}

# Fetch all branches and remove deleted remote branches.
alias gf='git fetch --all --prune && gb'

alias gd1='git diff `git merge-base origin/master HEAD` HEAD'
alias gd2='git diff `git merge-base origin/master HEAD` .'
alias gd3='git --no-pager diff --stat `git merge-base origin/master HEAD` .'
alias git-show-merged='git log --decorate | grep "commit.*(.*)"'
alias gu='git checkout origin/master'
alias gru='git rebase origin/master'
alias git-ignored='git ls-files . --ignored --exclude-standard --others'
