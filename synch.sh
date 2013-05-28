#!/bin/bash
install_dir=~/.config
link_file=links

for d in `ls -d */`; do
    d=`basename $d`
    # move files to install dir
    if [ -e $install_dir/$d ]; then
        echo "$d"
        for f in `ls -A $d`; do
            if [ $f != $link_file ]; then
                echo "   $f"
                diff $d/$f $install_dir/$d/$f
                if [ $? -ne 0 ]; then
                    echo "SHIT NOT THE SAME"
                fi
            fi
        done
    else
        echo "$d (copying)"
        cp -r $d $install_dir/$d
    fi
    # ensure links are properly set
    echo "setting up links"
    for l in `sed "s.~.$HOME." $d/$link_file`; do
        if [ ! -h $l ]; then
            echo "diffing `basename $l`"
            diff $l $d/`basename $l`
            if [ $? -eq 0 ]; then
                echo "making link"
                mv $l $l.bak
                ln -s $install_dir/$d/`basename $l` $l
            fi
        fi
    done
done

unset install_dir
unset link_file
unset diffIt
