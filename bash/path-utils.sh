
show-path() {
    echo $PATH | tr ":" "\n"
}

prepend-path() {
    export PATH="$1:$PATH"
}

append-path() {
    export PATH="$PATH:$1"
}
