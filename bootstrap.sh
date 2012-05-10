#!/bin/bash

cd "$(dirname "$0")"

if [ "$1" == "--quick" -o "$1" == "-q" ]; then
    shift
else
    git pull
fi

function doIt() {
    echo "==> Bash Files"
    rsync -av ./bash/ ~
    echo "==> EMACS Files"
    rsync -av ./emacs/ ~
    echo "==> GIT Files"
    rsync -av ./git/ ~
}

if [ "$1" == "--force" -o "$1" == "-f" ]; then
    doIt
else
    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        doIt
        fi
fi
unset doIt
source ~/.bash_profile
