# NVM and Node Setup
export NVM_DIR=~/.nvm

nvm() {
  source $(brew --prefix nvm)/nvm.sh
  nvm "$@"
}

append-path /Users/chris/.nvm/versions/node/v14.17.5/bin
