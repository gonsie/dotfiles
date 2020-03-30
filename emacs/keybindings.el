;; My Keys Mode

;; https://emacs.stackexchange.com/questions/352/how-to-override-major-mode-bindings/360#360
;; Create a minor mode to ensure some binidings override all others
;; (define-key my-mode-map (kbd "C-j") #'newline-and-indent)
;; only do this when a overriding mode is found
(defvar my-keys-mode-map (make-sparse-keymap)
  "Keymap for `my-keys-mode'.")

;;;###autoload
(define-minor-mode my-keys-mode
  "A minor mode so that my key settings override annoying major modes."
  ;; If init-value is not set to t, this mode does not get enabled in
  ;; `fundamental-mode' buffers even after doing \"(global-my-mode 1)\".
  ;; More info: http://emacs.stackexchange.com/q/16693/115
  :init-value t
  :lighter ""
  :keymap my-keys-mode-map)

;;;###autoload
(define-globalized-minor-mode global-my-keys-mode my-keys-mode my-keys-mode)

;; https://github.com/jwiegley/use-package/blob/master/bind-key.el
;; The keymaps in `emulation-mode-map-alists' take precedence over
;; `minor-mode-map-alist'
(add-to-list 'emulation-mode-map-alists `((my-keys-mode . ,my-keys-mode-map)))

;; Turn off the minor mode in the minibuffer
(defun turn-off-my-keys-mode ()
  "Turn off my-keys-mode."
  (my-keys-mode -1))
(add-hook 'minibuffer-setup-hook #'turn-off-my-keys-mode)

(provide 'my-keys-mode)
;; Minor mode tutorial: http://nullprogram.com/blog/2013/02/06/


;; My Bindings

(define-key function-key-map "\e[Z" [S-tab])
(setq mac-command-modifier 'super) ;; s
(setq mac-option-modifier 'meta)   ;; M
(normal-erase-is-backspace-mode 0)
;;(defun remap-up-key-in-shell () (local-set-key (kbd "<up>") 'comint-previous-input))
;;(add-hook 'shell-mode-hook 'remap-up-key-in-shell)

(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "s-w") nil)

(global-set-key (kbd "s-}") 'other-frame)
(global-set-key (kbd "s-{") 'other-frame)
(global-set-key (kbd "M-g") 'goto-line)

(global-set-key (kbd "M-RET") 'newline-and-indent)
(global-set-key (kbd "M-q")   'query-replace)
(global-set-key (kbd "M-SPC") 'execute-extended-command)

(global-set-key (kbd "C-c g") 'revert-buffer)
(global-set-key (kbd "C-c v") 'toggle-truncate-lines)
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c t") 'hs-toggle-hiding)

(global-set-key (kbd "<f5>") 'compile)

;; defaults overridden by multiple cursors
(global-set-key (kbd "M-<f2>") 'kmacro-start-macro)
(global-set-key (kbd "M-<f3>") 'kmacro-end-macro)

;; override html mode
(define-key my-keys-mode-map (kbd "C-c /") #'comment-or-uncomment-region)
;; override elpy mode
(define-key my-keys-mode-map (kbd "C-c T") #'hs-show-all)

;; My Key Bound Functions

(defun load-current-file ()
  (interactive)
  (load-file buffer-file-name))
(global-set-key (kbd "C-c C-l") 'load-current-file)
;; do not put this in my-keys. It should be overridden in org mode


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
;; (eval-after-load "eshell"
;;   '(progn (define-key eshell-mode-map (kbd "C-c C-o") 'tmtxt/open-current-dir-in-terminal)))


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

;(global-set-key (kbd "<backspace>") 'backward-delete-word)
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
