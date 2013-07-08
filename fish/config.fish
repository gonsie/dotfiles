alias em "emacs -nw"
alias emw "emacs"

# navigation
alias marco "pwd"
alias polo "dirs"
alias cd.. "cd .."
alias .. "cd .."
alias ... "cd ../.."

# shortcuts
alias g "git"
alias o "open"
alias oo "open ."

# http://defunkt.io/hub/
alias git hub

# enable aliases to be sudo'ed
alias sudo 'sudo '

function fish_user_key_bindings
         bind \t nextd-or-forward-word
         bind \[Z prevd-or-backward-word
end
