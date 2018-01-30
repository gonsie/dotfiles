;; 1. Presonal Preferences
;;    These settings should work in all versions of emacs

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

;; never kill my frame (GUI window). CMD-w (s-w) on mac
(put 'delete-frame 'disabled t)

;; revert to disk
(global-auto-revert-mode)

;; disable menu bar
(menu-bar-mode -1)

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


;; 2. Package Stuff
;;    kept in a separate file, reqires v24+

(when (< 24 emacs-major-version)
  (load "~/.config/emacs/init.el"))

;; 3. Custom Variables
;;    Machine-specific, so keep in home dir

(setq custom-file "~/.emacs-custom.el")
     (load custom-file)
