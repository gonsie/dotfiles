#!/bin/bash
install_dir=~/.config
link_file=links

function diffIt() {
    diff $1 $install_dir/$1 > tmp
    if [ $? -ne 0 ]; then
        echo "diff $1"
        cat tmp
        echo -ne "\nUse < (c)loud or > (l)ocal copy or (n)either? "
        read copy
        if [ $copy == "c" ]; then 
            cp $1 $install_dir/$1
        elif [ $copy == "l" ]; then
            cp $install_dir/$1 $1
        fi
    fi
}

for d in `ls -d */`; do
    d=`basename $d`
    # move files to install dir
    if [ -e $install_dir/$d ]; then
        for f in `ls -A $d`; do
            if [ -d $d/$f ]; then
                # search one level down
                for df in `ls -A $d/$f`; do
                    diffIt $d/$f/$df
                done
            elif [ $f != $link_file ]; then
                diffIt $d/$f
            fi
        done
    else
        echo "Creating $install_dir/$d"
        cp -r $d $install_dir/$d
    fi
    # ensure links are properly set
    for l in `sed "s.~.$HOME." $d/$link_file`; do
        if [ ! -h $l ]; then
            diff $l $d/`basename $l` > tmp
            if [ $? -eq 0 ]; then
                echo "Creating link for $l"
                mv $l $l.bak
                ln -s $install_dir/$d/`basename $l` $l
            else
                echo "ERROR: Cannot create link for $l, file is different:"
                cat tmp
            fi
        fi
    done
done

unset install_dir
unset link_file
unset diffIt
rm tmp

source ~/.bash_profile
