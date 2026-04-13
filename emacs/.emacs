;;; 1. Presonal Preferences
;; These settings should work in all versions of emacs

;; package.el will insert this, but I call it from init.el
;; (package-initialize)

;; UI
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))
(tooltip-mode    -1)
(menu-bar-mode   -1)

(if (<= 26 emacs-major-version)
    (global-display-line-numbers-mode t)
  (when (fboundp 'linum-mode)
    (setq linum-format "%d ")
    (global-linum-mode 1)))

(set-default 'cursor-type 'bar)
(show-paren-mode 1)
(setq use-dialog-box nil)

;; tab bar settings
(when (< 26 emacs-major-version)
  (tab-bar-mode 1)
  (setq tab-bar-show 1)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*dashboard*")
  (setq tab-bar-tab-hints t)
  (setq tab-bar-tab-name-function 'tab-bar-tab-name-truncated)
  (setq tab-bar-tab-name-truncated-max 10)
  (setq tab-bar-select-tab-modifiers "super")
  ;; TODO: test if this version wrapper is necessary
  (when (< 27 emacs-major-version)
    (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))))

;; Fancy titlebar for MacOS
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq ns-use-proxy-icon  nil)
(setq frame-title-format nil)

;; pulse
(defun pulse-line (&rest _)
      "Pulse the current line."
      (pulse-momentary-highlight-one-line (point)))

(when (< 24 emacs-major-version)
;; advice-add function added in 24.4
;; TODO figure out how to do minor version comparison, too convoluted now
  (dolist (command '(scroll-up-command scroll-down-command
				       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line)))
(setq pulse-flag nil)

;;;; Behavior
;; startup
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode 1)
(setq delete-by-moving-to-trash 1)
(setq inhibit-splash-screen t)
(when (display-graphic-p)
  (setq default-directory (concat (getenv "HOME") "/")))

;; files settings
(global-auto-revert-mode t)
(setq global-auto-revert-non-file-buffers t)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq gdb-gud-control-all-threads t)
(setq require-final-newline t)
(setq-default truncate-lines t)
(setq large-file-warning-threshold 100000000)
(setq history-length 25)
(savehist-mode 1)
(when (< 24 emacs-major-version)
  (save-place-mode 1))
(recentf-mode 1)

(when (<= emacs-major-version 30)
  (global-visual-wrap-prefix-mode 1))

;; from Bozhidar Batsov
;; https://emacsredux.com/blog/2026/04/07/stealing-from-the-best-emacs-configs/

;; -- performance tweaks
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(setq redisplay-skip-fontification-on-input t)
(setq read-process-output-max (* 4 1024 1024)) ; 4MB
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; -- kill ring / clipboard
(setq kill-do-not-save-duplicates t)

;; -- editing
(setq reb-re-syntax 'string)

;; -- windows
(setq window-combination-resize t)
(setq help-window-select t)

;; hooks
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; scrolling
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))
;; ;; keep currently line vertically centered
;; (setq scroll-preserve-screen-position t
;;       scroll-conservatively 0
;;       maximum-scroll-margin 0.5
;;       scroll-margin 99999)

;; never kill my frame (GUI window). CMD-w (s-w) on mac
(when (display-graphic-p)
  (put 'delete-frame 'disabled t)
  (put 'save-buffers-kill-terminal 'disabled t))

;; spell check all the time
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; ibuffer
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-default-sorting-mode 'major-mode)

;; line mode
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(when (< 23 emacs-major-version)
  (add-hook 'prog-mode-hook #'hs-minor-mode))

;; repeat mode
(when (<= emacs-major-version 28)
  (repeat-mode 1))

;; Scratch
;; from https://www.emacswiki.org/emacs/RecreateScratchBuffer
(defun unkillable-scratch-buffer ()
  (if (equal (buffer-name (current-buffer)) "*scratch*")
      (progn
	(delete-region (point-min) (point-max))
        (insert initial-scratch-message)
        nil)
    t))
(add-hook 'kill-buffer-query-functions 'unkillable-scratch-buffer)
(defun switch-to-scratch-and-back ()
  "Toggle between *scratch* buffer and the current buffer.
If the *scratch* buffer does not exist, create it."
  (interactive)
  (let ((scratch-buffer-name (get-buffer-create "*scratch*")))
    (if (equal (current-buffer) scratch-buffer-name)
        (switch-to-buffer (other-buffer))
      (switch-to-buffer scratch-buffer-name (lisp-interaction-mode)))))
(global-set-key (kbd "C-c s") 'switch-to-scratch-and-back)
(set-variable 'initial-scratch-message
";; This buffer is for text that is not saved, and for Lisp evaluation.
;; Run lisp code by going to end-of-line and typing C-j

")

;; tramp
(customize-set-variable 'tramp-use-connection-share nil)

;;; 4. Alternate Load Files

;; KEYBINDINGS
(load "~/.config/emacs/my-keybindings.el")

;; MY FUNCTIONS & Packages
(add-to-list 'load-path "~/.config/emacs/elisp")

;; Load Packages if on a Mac
(when (eq system-type 'darwin)
  ;; ensure running latest emacs
  (when (< 24 emacs-major-version)
    (load "~/.config/emacs/init.el")))

;; Load colors & theme
;; must come after init.el to ensure packages for guis
(load "~/.config/emacs/theme.el")
(load "~/.config/emacs/my-eshell.el")

;;; 5. Machine-specific configs
(if (eq system-type 'darwin)
    (setq cursys (car (split-string (system-name) "\\." t)))
  (setq cursys (getenv "LCSCHEDCLUSTER")))
(setq custom-file (concat "~/.emacs.d/custom-" cursys ".el"))
(load custom-file 'noerror)
(put 'upcase-region 'disabled nil)
