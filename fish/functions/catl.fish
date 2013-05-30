function catl -d "Cat a specific line"
         cat $argv[2] | head -$argv[1] | tail -1
end
