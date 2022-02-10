#!/bin/bash
install_dir=~/.config
link_file=links
diffs=""

# takes one argument:
# a filepath to diff between repo and $install_dir
function diffIt() {
    # run diff silently
    diff -N $1 $install_dir/$1 > tmp

    if [ $? -ne 0 ]; then
        echo -e "\n==> $1\n"

        if [ -e $install_dir/$1 ]; then
            # differences found
            cat tmp

            echo -e "\n<== $1"
            echo -ne "\n<(c)loud >(l)ocal (n)either (q)uit : "
            read copy
            if [ $copy == "c" ]; then
                cp $1 $install_dir/$1
            elif [ $copy == "l" ]; then
                cp $install_dir/$1 $1
            elif [ $copy == "q" ]; then
                rm tmp
                exit 0
            else
                diffs="$diffs $1"
            fi
        else
            # file DNE at destination
            echo "ALERT: $i does not exist in destination"
            cat tmp

            echo -e "\n<== $1"
            echo -ne "\n(i)install (s)kip (q)uit : "
            read cmd
            if [ $cmd == "i" ]; then
                mkdir -p $install_dir/`dirname $1`
                cp $1 $install_dir/$1
            elif [ $cmd == "q" ]; then
                exit 0
            fi
        fi
    fi
}

function fileLoop() {
    for f in `ls -A $1`; do
        if [ -d $1/$f ]; then
            fileLoop $1/$f
        elif [ $f != $link_file ]; then
            diffIt $1/$f
        fi
    done
}

# First Time Setup
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
    touch ~/.emacs.d/custom-`hostname -s`.el
    if [ ! -e ~/.ssh ]; then
        mkdir ~/.ssh
        echo ""
        echo "WARNING : submodules require ssh-keys"
        echo "set up keys with github, then init git submodules"
        echo ""
    else
        # assuming ssh-keys are set up
        # make sure submodules are loaded
        git submodule init
        git submodule update
    fi

fi

# setup submodule symlinks
one_time=0
for m in `grep 'path' .gitmodules | cut -d' ' -f3`; do
    if [ ! -e $install_dir/$m ]; then
        if [ ! $one_time ]; then
            echo "creating submodule symlink $install_dir/$m"
            one_time=1
        fi
        ln -s `pwd`/$m $install_dir/$m
    fi
done

# make sure dotfiles repo is up-to-date
git pull origin master
if [[ $? -ne 0 && $1 != "-f" ]]; then
    echo ""
    echo "Error with git pull. Exiting."
    echo "Override with -f"
    exit
fi

# main loop
for d in `ls -d */`; do
    d=`basename $d`
    # don't install if links file DNE
    if [ ! -e $d/$link_file ]; then
        continue
    fi

    CAP=`echo "${d}" | tr '[a-z]' '[A-Z]'`
    echo -ne "\n** $CAP **\n"
    # recursively diff and move files to install dir
    if [ -e $install_dir/$d ]; then
        fileLoop $d
    else
        echo "Creating $install_dir/$d"
        cp -r $d $install_dir/$d
    fi
    # ensure links are properly set
    for l in `sed "s#~#$HOME#" $d/$link_file`; do
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
echo ""
echo "** Synchronization Complete **"
echo ""
echo "Differences Remaining:"
for i in `echo $diffs`; do
    echo "- $i"
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
unset diffs
unset diffIt
rm tmp

source ~/.bash_profile
