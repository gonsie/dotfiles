(defun my/config-synch-emacs ()
  (interactive)
  (ediff-directories "~/Projects/dotfiles/emacs" "~/.config/emacs" ""))
