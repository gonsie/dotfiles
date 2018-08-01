;; COLORS & THEME
;;
;; This file should work for any configuration of emacs
;; - Dusk Color Theme (for color-theme 6.6.0)
;; - Mode line


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
   '(color-theme-dusk
     ((font . "fixed")
      (width . 130)
      (height . 50)
      (background-mode . dark)
      (mouse-color . "#FFFFFF")
      (cursor-color . "#FFFFFF"))
     (default ((t (nill))))
     (font-lock-comment-face ((t (:italic t :foreground "#54BE5A"))))
     (font-lock-string-face ((t (:foreground "#E1404A"))))
     (font-lock-keyword-face ((t (:foreground "#BF369A"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))
     (font-lock-constant-face ((t (:foreground "#8B86CE"))))
     (font-lock-type-face ((t (:foreground "#BF369A"))))
     (font-lock-variable-name-face ((t (:foreground "Yellow"))))
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
     (minibuffer-prompt ((t (:foreground "orange"))))
     (link ((t (:foreground "#536fd6" :underline t))))
     (widget-field-face ((t (:background "navy" :foreground "white"))))
     (widget-single-line-field-face ((t (:background "royalblue" :foreground "white")))))) )

(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dusk)
     (when (display-graphic-p)
       ;; settings for GUI emacs
       (add-to-list 'default-frame-alist '(background-color . "#282B35"))
       (add-to-list 'default-frame-alist '(foreground-color . "White"))
       (set-frame-font "Inconsolata 18" nil t)
       (put 'save-buffers-kill-terminal 'disabled t))))

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
