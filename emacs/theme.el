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
     (font-lock-type-face ((t (:foreground "#f56dca"))))
     (font-lock-variable-name-face ((t (:foreground "Yellow"))))
     (font-lock-function-name-face ((t (:foreground "#95C76E"))))
     (font-lock-builtin-face ((t (:foreground "#D08E5D"))))  ;preprocessor stmts, brown
     (hl-line ((t (:background "#112233"))))
     (region ((t (:foreground nil :background "#555555"))))
     (show-paren-match-face ((t (:bold t :foreground "#ffffff" :background "#050505"))))
     (highline-face ((t (:background "#919075")))) ;919075
     (pulse-highlight-face-start ((t (:background "#919075"))))
     (setnu-line-number-face ((t (:background "Grey15" :foreground "White" :bold t))))
     (show-paren-match-face ((t (:background "grey30"))))
     (region ((t (:background "grey15"))))
     (highlight ((t (:background "#919075"))))
     (secondary-selection ((t (:background "navy"))))
     (minibuffer-prompt ((t (:foreground "orange"))))
     (link ((t (:foreground "#536fd6" :underline t))))
     (widget-field-face ((t (:background "navy" :foreground "white"))))
     (widget-single-line-field-face ((t (:background "royalblue" :foreground "white")))))) )


;; fonts
(require 'cl-lib)

(defun* test-and-set-font (fontlist)
  "Given a priority list of font names, test for font to exist and if so, set it"
  (while fontlist
    (let* ((fontname (car fontlist)))
      (setq fontlist (cdr fontlist))
      (when (member fontname (font-family-list))
        (set-frame-font (concat fontname " 18") nil t)
        (set-frame-font fontname)
        (let* ((fontfile (concat "~/.config/emacs/font-"
                                 (downcase (replace-regexp-in-string " " "-" fontname))
                                 ".el")))
          (when (file-exists-p fontfile)
            (load-file fontfile)))
                                        ;(princ (concat "Set font to " fontname))
        (return-from test-and-set-font)))))


(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dusk)
     (when (display-graphic-p)
       ;; settings for GUI emacs
       (add-to-list 'default-frame-alist '(background-color . "#282B35"))
       (add-to-list 'default-frame-alist '(foreground-color . "White"))
       (when (< 24 emacs-major-version)
         (test-and-set-font '("JetBrains Mono" "Fira Code" "Inconsolata"))))))

;; Mode line settings
;; use setq-default to set it for /all/ modes

(setq-default mode-line-format
              (list
               ;; day and time
               '(:eval (propertize (format-time-string " %b %d %H:%M ")
                                   'face 'font-lock-builtin-face))


               '(:eval (propertize (substring vc-mode 5)
                                   'face 'font-lock-comment-face))

               ;; the buffer name; the file name as a tool tip
               '(:eval (propertize " %b "
                                   'face
                                   (let ((face (buffer-modified-p)))
                                     (if face 'font-lock-warning-face
                                       'font-lock-type-face))
                                   'help-echo (buffer-file-name)))

               ;; line and column
               " (" ;; '%02' to set to 2 chars at least; prevents flickering
               (propertize "%02l" 'face 'font-lock-keyword-face) ","
               (propertize "%02c" 'face 'font-lock-keyword-face)
               ") "

               ;; relative position, size of file
               " ["
               (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
               "/"
               (propertize "%I" 'face 'font-lock-constant-face) ;; size
               "] "

               ;; spaces to align right
               '(:eval (propertize
                " " 'display
                `((space :align-to (- (+ right right-fringe right-margin)
                                      ,(+ 3 (string-width mode-name)))))))

               ;(propertize org-mode-line-string 'face '(:foreground "#5DD8FF"))

               ;; the current major mode
               (propertize " %m " 'face 'font-lock-string-face)
               ;;minor-mode-alist
               ))

(set-face-attribute 'mode-line nil
                    :background "#353644"
                    :foreground "white"
                    :box '(:line-width 8 :color "#353644")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#565063"
                    :foreground "white"
                    :box '(:line-width 8 :color "#565063")
                    :overline nil
                    :underline nil)
