update_recent_dirs()
{
  if [ "$PWD" != "$last_dir" ]; then
    last_dir="$PWD"
    local recent_dirs=()
    while read -r line
    do
      recent_dirs+=("$line")
    done < "$HOME/.recent_dirs"
    local new_dirs=()
    new_dirs+=("$PWD")
    local count=0
    for s in "${recent_dirs[@]}"; do
      if [ "$PWD" != "$s" ] && [ $count -lt 20 ]; then
        new_dirs+=("$s")
      fi
      ((count++))
    done
    printf "%s\n" "${new_dirs[@]}" > ~/.recent_dirs
  fi
}

select_recent_dir()
{
  cat -n ~/.recent_dirs | tac
  if [ -n "$ZSH_VERSION" ]; then
    read 'n?Enter the number of the directory to cd to: '
  else
    read -p 'Enter the number of the directory to cd to: ' n
  fi
  num_lines=`wc -l < ~/.recent_dirs`
  if [[ $n =~ ^[0-9]+$ ]] && [ $n -gt 0 ] && [ $n -le $num_lines ]; then
    cd "`sed -n ${n}p ~/.recent_dirs`"
  else
    echo Bad number
  fi
}

# alias cd='my_cd'
alias r=select_recent_dir
