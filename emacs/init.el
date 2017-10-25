;; personal preferences
(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
(setq gdb-gud-control-all-threads t)
(delete-selection-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(defalias 'yes-or-no-p 'y-or-n-p)

;; autopair
(autopair-global-mode)
(diminish 'autopair-mode)

;; revert to disk
(global-auto-revert-mode)

;; disable menu bar
(menu-bar-mode -1)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

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
;;(defun remap-up-key-in-shell () (local-set-key (kbd "<up>") 'comint-previous-input))
;;(add-hook 'shell-mode-hook 'remap-up-key-in-shell)

;; PACKAGES

;; Personally Included Packages
(add-to-list 'load-path "~/.config/emacs/elisp")

;; Linum
(setq linum-format "%d ")

;; neotree
(use-package neotree
  :bind ([f8] . neotree-toggle)
  :config
  (setq neo-default-system-application "open")
  (set-face-attribute 'neo-dir-link-face t :foreground "yellow")
  (set-face-attribute 'neo-file-link-face t :foreground "color-255"))

;; CMake
(use-package cmake-mode
  :mode ("\\.cmake\\'"
         "CMakeLists\\.txt\\'")
  :config (use-package cmake-font-lock))

;; MAGIT
(use-package magit
  :bind ("C-x g" . magit-status)
  :config ((set-face-attribute 'magit-diff-context-highlight t :background "grey45" :foreground "grey50")
           (set-face-attribute 'magit-section-highlight t :background "grey45"))
  :ensure t)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "kramdown"))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("<f2>" . mc/mark-previous-like-this)
         ("<f3>" . mc/mark-next-like-this)
         ("C-c <f2>" . mc/mark-all-like-this)
         ("<ESC> <ESC>" . mc/keyboard-quit))
  :ensure t)

;; Word Count (dotfile copy)
(load "wc")

;; ORG

;; keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

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
     (add-to-list 'org-structure-template-alist '("t" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n"))
     ))

;; Add Org files to the agenda when we save them
(defun to-agenda-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))

(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)


;; COLORS & THEME

;; Customize Face Manually
(set-face-attribute 'org-table t :foreground "blue")
(set-face-attribute 'org-document-info t :foreground "blue")
(set-face-attribute 'org-document-title t :foreground "blue" :bold)

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
     (widget-field-face ((t (:background "navy" :foreground "white"))))
     (widget-single-line-field-face ((t (:background "royalblue" :foreground "white")))))) )
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
    " -- ("
    minor-mode-alist  ;; list of minor modes
    " ) %-" ;; fill with '-'
))
