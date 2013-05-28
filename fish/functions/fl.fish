function fl -d "Move something, then follow it"
         mv $argv[1] $argv[2]
         cd $argv[2]
end
