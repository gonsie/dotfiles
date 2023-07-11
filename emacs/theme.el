(setq custom-theme-directory "~/.config/emacs/")
(load-theme 'dusk t)

(defun my/new-frame-reapply-theme (frame)
  (select-frame frame)
  (load-theme 'dusk t))
(add-hook 'after-make-frame-functions 'my/new-frame-reapply-theme)

;; fonts
(require 'cl-lib)

(cl-defun test-and-set-font (fontlist)
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
        (cl-return-from test-and-set-font)))))

(when (and (display-graphic-p) (< 24 emacs-major-version))
    (test-and-set-font '("JetBrains Mono" "Fira Code" "Inconsolata")))

;; change the behavior of ansi-term's mode line
(defun my/term-update-mode-line ()
  (let ((term-mode
         (if (term-in-char-mode)
             (propertize "term"
                         'help-echo "mouse-1: Switch to line (emacs) mode"
                         'mouse-face 'mode-line-highlight
                         'local-map
                         '(keymap
                           (mode-line keymap (down-mouse-1 . term-line-mode))))
           (propertize "emacs"
                       'help-echo "mouse-1: Switch to char (term) mode"
                       'mouse-face 'mode-line-highlight
                       'local-map
                       '(keymap
                         (mode-line keymap (down-mouse-1 . term-char-mode)))))))
    (setq mode-line-process
          (list term-mode)))
  (force-mode-line-update))
;;;; TODO: change color of mode line for term mode, some thing like
;;;; maybe this goes as a hook to the term-char/line-mode function?
;; (face-remap-add-relative
;;              'mode-line '((:foreground "ivory" :background "DarkOrange2") mode-line))


(advice-add 'term-update-mode-line :override
            'my/term-update-mode-line)


;; Mode line settings
;; use setq-default to set it for /all/ modes

(setq-default mode-line-format
              (list

               ;; ;; meow modal mode
               ;; (when (require 'meow nil 'noerror)
               ;;   (meow-indicator))

               ;; day and time
               '(:eval (propertize (format-time-string " %b %d %H:%M ")
                                   'face 'font-lock-builtin-face))


               ;; '(:eval (propertize (substring vc-mode 5)
               ;;                     'face 'font-lock-comment-face))

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

               ;; ansi-term's line or char mode
               ":"
               'mode-line-process
               ":"

               ;; spaces to align right
               '(:eval (propertize
                " " 'display
                `((space :align-to (- (+ right right-fringe right-margin)
                                      ,(+ 3 (string-width (if (listp mode-name) (car mode-name) mode-name))))))))

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
