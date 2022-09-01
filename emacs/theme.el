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

;; Mode line settings
;; use setq-default to set it for /all/ modes

(setq-default mode-line-format
              (list
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
