#!/bin/bash

cd "$(dirname "$0")"

hflag=no
qflag=no
fflag=no

while getopts "hqfm" FLAG; do
    case $FLAG in
        h)
            echo "Usage: `basename $0`"
            echo -e "\t-h := help\tThis text"
            echo -e "\t-f := force\tDo not diff with existing files"
            echo -e "\t-q := quick\tDo not perform a \`git put; git pull\`"
            exit
            ;;
        q)
            qflag=yes
            ;;
        f)
            fflag=yes
            ;;
    esac
done

function diffEm() {
    for file in `find ./*/ -depth 1`; do
        diff $file ~/`basename $file`
        if [ $? -eq 1 ]; then
            read -p "==> pull changes for `basename $file`? (y/n) " -n 1
            echo
            if [[ $REPLY =~ ^[Yy]$ ]]; then
                cp ~/`basename $file` $file
            fi
        fi
    done
}

function doIt() {
    rsync -av `find ./ -depth 1 -type d -regex "\.//[^\.].*"`/ ~
}

if [[ $fflag == no ]]; then
    diffEm
fi

if [[ $qflag == no ]]; then
    git cm "made some local changes"
    git put
    git pul
fi

doIt

unset diffEm
unset doIt
source ~/.bash_profile
