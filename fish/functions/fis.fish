function fis -d "create an archive of a git repo and send it to fis"
    if test (count $argv) -lt 1
        echo "Usage: fis directory"
        return 0
    end

    pushd .
    cd $argv[1]

    set -l hash (git rev-parse --short HEAD)
    set -l name (basename (pwd))

    cd ..
    tar -cf $name-$hash.tar $name/
    set -l archive (pwd)/$name-$hash.tar

    popd
    mv -f $archive ./

    echo "Created: $name-$hash.tar"
    scp $name-$hash.tar rzgenie:~/FIS/
    echo "Don't forget to FIS from RZ machine"
end
