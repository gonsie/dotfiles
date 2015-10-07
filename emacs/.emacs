;; personal preferences
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(global-set-key (kbd "C-o") 'other-window)
(setq gdb-gud-control-all-threads t)

;; Key Bindings
;; NOTE: shift key generally doesn't work in terminal.app
(define-key function-key-map "\e[Z" [S-tab])
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
;; remap of up key in shell mode
(defun remap-up-key-in-shell () (local-set-key (kbd "<up>") 'comint-previous-input))
(add-hook 'shell-mode-hook 'remap-up-key-in-shell)

;; Homebrew installed packages
(let ((default-directory "/usr/local/share/emacs/site-lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

;; single files should be saved here
(add-to-list 'load-path "~/.config/emacs/elisp")
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Whitespace
(require 'whitespace)
(autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

;; Linum
(setq linum-format "%d ")

;; CMake Syntax
; Add cmake listfile names to the mode list.
(setq auto-mode-alist
        (append
            '(("CMakeLists\\.txt\\'" . cmake-mode))
            '(("\\.cmake\\'" . cmake-mode))
            auto-mode-alist))

(autoload 'cmake-mode "/usr/local/share/cmake/editors/emacs/cmake-mode.el" t)

;; NEDfile Syntax
(autoload 'ned-mode "ned-mode" "Major Mode for editing Ned files" t)
(setq auto-mode-alist (cons '("\\.ned\\'" . ned-mode) auto-mode-alist))
(autoload 'ini-mode "ini-mode" "Major Mode for editing Ini files" t)
(setq auto-mode-alist (cons '("\\.ini\\'" . ini-mode) auto-mode-alist))


;; Autopair
(require 'autopair)
(autopair-global-mode)

;; WORD COUNT
(load "wc")

;; workgroups
(require 'workgroups)
(workgroups-mode 1)
(setq wg-switch-on-load nil)
(wg-load "~/.config/emacs/elisp/wg-saves")

;; Color-Theme 6.6.0
(add-to-list 'load-path "~/.config/emacs/elisp/color-theme-6.6.0/")
(require 'color-theme)
(defun color-theme-dusk ()
  "Color theme by Elsa Gonsiorowski, created 2012-05-15.
Based on color-theme-midnight by Gordon Messmer and the Xcode dusk theme.

Note that there is no background color provided. 282B35.

If you want to modify the font as well, you should customize variable
`color-theme-legal-frame-parameters' to \"\\(color\\|mode\\|font\\|height\\|width\\)$\".
The default setting will prevent color themes from installing specific
fonts."
  (interactive)
  (color-theme-install
   '(color-theme-midnight
     ((font . "fixed")
      (width . 130)
      (height . 50)
      (background-color . "282B35")
      (foreground-color . "FFFFFF")
      (background-mode . dark)
      (mouse-color . "FFFFFF")
      (cursor-color . "FFFFFF"))
     (default ((t (nil))))
     (font-lock-comment-face ((t (:italic t :foreground "#54BE5A"))))
     (font-lock-string-face ((t (:foreground "#E1404A"))))
     (font-lock-keyword-face ((t (:foreground "#BF369A"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (font-lock-constant-face ((t (:foreground "#8B86CE"))))
     (font-lock-type-face ((t (:foreground "#BF369A"))))
     (font-lock-variable-name-face ((t (:foreground "#FFFFFF"))))
     (font-lock-function-name-face ((t (:foreground "#95C76E"))))
     (font-lock-builtin-face ((t (:foreground "#D08E5D"))))  ;preprocessor stmts, brown
     (hl-line ((t (:background "#112233"))))
     (mode-line ((t (:foreground "#ffffff" :background "#333333"))))
     (region ((t (:foreground nil :background "#555555"))))
     (show-paren-match-face ((t (:bold t :foreground "#ffffff" :background "#050505"))))
     (highline-face ((t (:background "#919075")))) ;919075
     (setnu-line-number-face ((t (:background "Grey15" :foreground "White" :bold t))))
     (show-paren-match-face ((t (:background "grey30"))))
     (region ((t (:background "grey15"))))
     (highlight ((t (:background "#919075"))))
     (secondary-selection ((t (:background "navy"))))
     (widget-field-face ((t (:background "navy"))))
     (widget-single-line-field-face ((t (:background "royalblue")))))) )
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dusk)))

;; Mode line settings
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face 'font-lock-keyword-face
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
      (propertize "%02l" 'face 'font-lock-type-face) ","
      (propertize "%02c" 'face 'font-lock-type-face) 
    ") "

    ;; relative position, size of file
    "["
    (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "

    ;; the current major mode for the buffer.
    "["

    '(:eval (propertize "%m" 'face 'font-lock-string-face
              'help-echo buffer-file-coding-system))
    "] "


    "[" ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face 'font-lock-preprocessor-face
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face 'font-lock-warning-face
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face 'font-lock-type-face
                             'help-echo "Buffer is read-only"))))  
    "] "

    ;; add the time, with the date and the emacs uptime in the tooltip
    '(:eval (propertize (format-time-string "%H:%M")
              'help-echo
              (concat (format-time-string "%c; ")
                      (emacs-uptime "Uptime:%hh"))))
    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
))


;; ORG-MODE
(require 'org-install)
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gud-gdb-command-name "gdb --annotate=1")
 '(large-file-warning-threshold nil)
 )
