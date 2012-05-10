#!/bin/bash

cd "$(dirname "$0")"

if [ "$1" == "--quick" -o "$1" == "-q" ]; then
    shift
else
    git pull
fi

function doIt() {
    echo -e ${GREEN}"==> Bash Files"${WHITE}
    rsync -av ./bash/ ~
    echo -e ${GREEN}"==> Emacs Files"${WHITE}
    rsync -av ./emacs/ ~
    echo -e ${GREEN}"==> GIT Files"${WHITE}
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
