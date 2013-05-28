function sw -d "Switch two files"
         mv $argv[1] {$argv[2]}-temp
         mv $argv[2] $argv[1]
         mv {$argv[2]}-temp $argv[2]
end
