function md -d "Create and enter a directory (unsofisticated)"
         mkdir $argv
         cd $argv
end

function fl -d "Move something, then follow it"
         mv $argv[1] $argv[2]
         cd $argv[2]
end

function sw -d "Switch two files"
         mv $argv[1] {$argv[2]}-temp
         mv $argv[2] $argv[1]
         mv {$argv[2]}-temp $argv[2]
end

function title -d "Change the name of the window"
         echo "\033]0;$argv\007"
end

function fish_title -d "Default tab title"
         echo $_
         pwd
end

function mva -d "append an extention to a file"
         mv $argv[1] $argv[1].$argv[2]
end
