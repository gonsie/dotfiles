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

# create install_dir if it doesn't exist
if [ ! -e $install_dir ]; then
    echo "* Welcome *"
    first_time=true
    echo "I assume it's your first time here: Creating $install_dir"
    mkdir $install_dir
    mkdir $install_dir/originals

    # touch things needed for current settings
    if [ ! -e ~/.histories ]; then
    	mkdir ~/.histories
    fi
    touch ~/.emacs-custom.el
fi

for d in `ls -d */`; do
    d=`basename $d`
    # don't install if links file DNE
    if [ ! -e $d/$link_file ]; then
        continue
    fi

    CAP=`echo "${d}" | tr '[a-z]' '[A-Z]'`
    echo -ne "\n** $CAP **\n"
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
        if [ ! -e $l ]; then # file doesn't exist
            ln -s $install_dir/$d/`basename $l` $l
        elif [ ! -h $l ]; then # file exists and isn't a link
	    if [ "$first_time" = true ]; then # it's our first time
		mv $l $install_dir/originals  # save the original file
		ln -s $install_dir/$d/`basename $l` $l # link github file
	    else
		diff $l $d/`basename $l` > tmp
		if [ $? -eq 0 ]; then # no diff
                    echo "Creating link for $l"
                    mv $l $l.bak
                    ln -s $install_dir/$d/`basename $l` $l
		else
                    echo "ERROR: Cannot create link for $l, file is different:"
                    cat tmp
		fi
	    fi
        fi
    done
done

if [ "$first_time" = true ]; then
    echo "** Installing Bashmarks **"
    cd ../
    git clone https://github.com/gonsie/bashmarks.git
    cd bashmarks
    make install

    echo "** Installing Fishmarks **"
    cd ../
    git clone https://github.com/gonsie/fishmarks.git
    cd fishmarks
    make install

    echo "** Setting up Git **"
    echo -ne "Enter user.name: "
    read username
    echo -ne "Enter user.email: "
    read useremail
    git config --global user.name "$username"
    git config --global user.email "$useremail"

    echo "* Setup Complete *"
    echo "Please review the files saved in $install_dir/originals"
fi

unset install_dir
unset first_time
unset link_file
unset diffIt
rm tmp

source ~/.bash_profile
