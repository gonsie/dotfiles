;; 1. Presonal Preferences
;;    These settings should work in all versions of emacs

;; package.el will insert this, but I call it from init.el
;; (package-initialize)

;; minimal UI
(scroll-bar-mode -1)
(tool-bar-mode   -1)
(tooltip-mode    -1)
(menu-bar-mode   -1)

(if (<= 26 emacs-major-version)
    (global-display-line-numbers-mode t)
  (setq linum-format "%d "))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq gdb-gud-control-all-threads t)
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq require-final-newline t)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(set-default 'cursor-type 'bar)
(setq-default truncate-lines t)
(show-paren-mode 1)

;; scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;; never kill my frame (GUI window). CMD-w (s-w) on mac
(put 'delete-frame 'disabled t)

;; revert to disk
(global-auto-revert-mode)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; spell check all the time
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; MY FUNCTIONS

;; UNDO Chris's AutoFillText
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;; KEY BINDINGS

(define-key function-key-map "\e[Z" [S-tab])
(setq mac-command-modifier 'super) ;; s
(setq mac-option-modifier 'meta)   ;; M
(normal-erase-is-backspace-mode 0)
;;(defun remap-up-key-in-shell () (local-set-key (kbd "<up>") 'comint-previous-input))
;;(add-hook 'shell-mode-hook 'remap-up-key-in-shell)

(global-set-key (kbd "s-}") 'other-frame)
(global-set-key (kbd "s-{") 'other-frame)

(global-set-key (kbd "C-c g") 'revert-buffer)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)

;; MY KEY BOUND FUNCTIONS

(defun load-current-file ()
  (interactive)
  (load-file buffer-file-name))
(global-set-key (kbd "C-c C-l") 'load-current-file)

;; https://truongtx.me/2013/09/13/emacs-dired-new-terminal-window-at-current-directory-on-macos
;; default terminal application path
(defvar tmtxt/macos-default-terminal-app-path
  "/Applications/Utilities/Terminal.app" "The default path to terminal application in MacOS")
;;; function to open new terminal window at current directory
(defun tmtxt/open-current-dir-in-terminal ()
  "Open current directory in dired mode in terminal application.
For MacOS only"
  (interactive)
  (shell-command (concat "open -a "
                         (shell-quote-argument tmtxt/macos-default-terminal-app-path)
                         " "
                         (shell-quote-argument (file-truename default-directory)))))
(eval-after-load "dired"
  '(progn (define-key dired-mode-map (kbd "C-c C-o") 'tmtxt/open-current-dir-in-terminal)))

;; DELETE v KILL

;; from https://stackoverflow.com/a/12990359/1160876
(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun delete-word (arg)
  "Delete characters forwards until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-line (arg)
  "Delete (not kill) the current line, backwards from cursor.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (beginning-of-visual-line arg) (point))))

(defun delete-line (arg)
  "Delete (not kill) the current line, forwards from cursor.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (end-of-visual-line arg) (point))))

(defun backward-kill-line (arg)
  "Kill the current line, backwards from cursor.
With argument ARG, do this that many tmies."
  (interactive "p")
  (kill-region (point) (progn (beginning-of-visual-line arg) (point))))

;; from https://stackoverflow.com/a/35711240/1160876
(defun delete-current-line (arg)
  "Delete (not kill) the current line."
  (interactive "p")
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line arg) (point)))))

(global-set-key (kbd "<M-delete>") 'backward-delete-word)
(global-set-key (kbd "<M-S-backspace>") 'backward-kill-word)
(global-set-key (kbd "<C-backspace>") 'backward-delete-line)
(global-set-key (kbd "<C-S-backspace>") 'backward-kill-line)
(global-set-key (kbd "<s-backspace>") 'delete-current-line)
(global-set-key (kbd "<s-S-backspace>") 'kill-whole-line)
(global-set-key (kbd "<kp-delete>") 'delete-forward-char)
(global-set-key (kbd "<M-kp-delete>") 'delete-word)
(global-set-key (kbd "<M-S-kp-delete>") 'kill-word)
(global-set-key (kbd "<C-kp-delete>") 'delete-line)
(global-set-key (kbd "<C-S-kp-delete>") 'kill-line)
(global-set-key (kbd "<s-kp-delete>") 'delete-current-line)
(global-set-key (kbd "<s-S-kp-delete>") 'kill-whole-line)

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;; Load Packages if on a Mac
(when (eq system-type 'darwin)
  ;; ensure running latest emacs
  (when (< 24 emacs-major-version)
    (load "~/.config/emacs/init.el")
    ;; org-mode stuff gets its own file
    (load "~/.config/emacs/org-config.el")))

;; Load colors & theme
;; must come after init.el to ensure packages for guis
(load "~/.config/emacs/theme.el")

;; Machine-specific configs
(setq cursys (getenv "LCSCHEDCLUSTER"))
(setq custom-file (concat "~/.emacs.d/custom-" cursys ".el"))
(load custom-file 'noerror)
