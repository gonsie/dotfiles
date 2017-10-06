;; personal preferences
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq gdb-gud-control-all-threads t)
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(global-auto-revert-mode)
(defalias 'yes-or-no-p 'y-or-n-p)
(autopair-global-mode)

;; disable menu bar
(menu-bar-mode -1)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; shift-arrow to change windows
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(normal-erase-is-backspace-mode 0)
(defun remap-up-key-in-shell () (local-set-key (kbd "<up>") 'comint-previous-input))
(add-hook 'shell-mode-hook 'remap-up-key-in-shell)

;; PACKAGES

;; Personally Included Packages
(add-to-list 'load-path "~/.config/emacs/elisp")

;; Linum
(setq linum-format "%d ")

;; neotree
(use-package neotree
  :bind ([f8] . neotree-toggle))

;; CMake
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; Markdown
(setq markdown-command "kramdown")
;; (use-package markdown-mode
;;   :ensure t
;;   :commands (markdown-mode gfm-mode)
;;   :mode (("README\\.md\\'" . gfm-mode)
;;          ("\\.md\\'" . markdown-mode)
;;          ("\\.markdown\\'" . markdown-mode))
;;   :init (setq markdown-command "kramdown"))

;; Word Count (dotfile copy)
(load "wc")

;; ORG

(require 'ox-md)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)
   (dot . t)
   (org . t)
   ))

;; adding the <[TAB] shortcuts
(eval-after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist '("n" "#+NAME: "))
     (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n?\n:END:"))
     (add-to-list 'org-structure-template-alist '("t" "#+title:?\n#+subtitle:\n#+author:\n#+date:\n"))
))

;; COLORS & THEME

;; ;; base16-theme
;; ;; this would work... if I didn't have a Terminal Profile already set up
;; (use-package base16-theme
;;  :ensure t
;;  :config
;;  (load-theme 'base16-xcode-dusk t))

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

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(neo-dir-link-face ((t (:foreground "yellow"))))
;;  '(neo-file-link-face ((t (:foreground "color-255"))))
;;  '(org-document-info ((t (:foreground "blue"))))
;;  '(org-document-title ((t (:foreground "blue" :weight bold)))))
