function __emit_cwd --on-variable PWD -d "Tell Terminal the CWD"
         printf '\033]7;%s\07' (echo file://(hostname -s)$PWD |sed -e 's/ /%20/g')
end
