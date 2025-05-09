;;; 1. Setup

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized (package-initialize))

;; Bootstrap `use-package`
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Install by hand:
;; - org
(use-package org
  :ensure t
  :config
  (load "~/.config/emacs/org-config.el"))

;;; 2. PACKAGES

;;; * Personally Included Packages
(add-to-list 'load-path "~/.config/emacs/elisp")
(load "wc")
;(load "xah")
(load "my-config-synch")

;; autopair
;; (autopair-global-mode)
;; (diminish 'autopair-mode)
(electric-pair-mode)

(use-package osx-trash
  :if (eq system-type 'darwin)
  :ensure t)

;; dired-sidebar
(use-package dired-sidebar
  :bind ("<f10>" . dired-sidebar-toggle-sidebar)
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; CMake
(use-package cmake-mode
  :mode ("\\.cmake\\'"
         "CMakeLists\\.txt\\'"))

(use-package cmake-font-lock
  :init
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package eldoc-cmake
  :hook (cmake-mode . eldoc-cmake-enable))

;; MAGIT
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-file-dispatch))
  :config
  (set-face-attribute 'magit-diff-context-highlight t :background "grey45" :foreground "grey50")
  (set-face-attribute 'magit-section-highlight t :background "grey45")
  :ensure t)

(use-package forge
  :after magit
  :ensure t)

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :bind ("C-c C-e" . markdown-export-and-preview)
  :config (setq markdown-command "kramdown"))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("<f2>" . mc/mark-previous-like-this)
         ("S-<f2>" . mc/unmark-previous-like-this)
         ("<f3>" . mc/mark-next-like-this)
         ("S-<f3>" . mc/unmark-next-like-this)
         ("C-c <f2>" . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)
         ("<ESC> <ESC>" . mc/keyboard-quit))
  :ensure t)

(use-package expand-region
  :ensure expand-region
  :bind ("M-=" . er/expand-region))

;; csv
(use-package csv-mode
  :mode ("\\.csv\\'"))

;; json
(use-package json-mode
  :mode ("\\.json\\'"))

;; rust
(use-package rust-mode
  :mode ("\\.rs\\'"))

;; graphviz
(use-package graphviz-dot-mode
  :mode ("\\.gv\\'"))

;; counsel -> swiper -> ivy
;; thanks abo-abo
(use-package counsel
  :ensure t
  :diminish ivy-mode
;;  )

;; ;; swiper pop-up search
;; (use-package swiper
;;   :ensure t
;;   :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-c j" . ivy-immediate-done)
         ("M-C-s" . counsel-git-grep))
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virutal-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) ")
  ;; (setq ivy-history t)
  ;; (setq counsel-M-x-history t)
  )

;; dashboard
(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :config
  (load "~/.config/emacs/my-dashboard.el"))

;; auto dim / dimmer
(use-package auto-dim-other-buffers
  :ensure t
  :diminish auto-dim-other-buffers-mode
  :init (add-hook 'after-init-hook (lambda ()
                                     (when (fboundp 'auto-dim-other-buffers-mode)
                                       (auto-dim-other-buffers-mode t))))
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil :background "#42444C"))

;; Python development
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
(use-package flycheck
  :after elpy
  :hook (elpy-mode . flycheck-mode))
(use-package py-autopep8
  :after elpy
  :hook (elpy-mode . py-autopep8-enable-on-save))

;; window management
(use-package win-switch
  :disabled
  :load-path "~/.config/emacs/elisp/win-switch"
  :ensure t
  :config
  (win-switch-setup-keys-ijkl-dvorak "\C-xo")
  (win-switch-add-key "o" 'next-window)
  (setq win-switch-idle-time 2.0)
  (setq win-switch-window-threshold 2)
  (setq win-switch-other-window-first nil)
  (setq win-switch-feedback-background-color "#536fd6"))

;; Keybindings
(use-package bicycle
  :after outline
  :bind (:map outline-minor-mode-map
              ([C-tab] . bicycle-cycle)
              ([S-tab] . bicycle-cycle-global)))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))

;; projectile
(use-package projectile
  :init (projectile-mode +1)
  :bind (("s-p" . projectile-command-map)))

;; note: built in package
(use-package outline-mode
  :bind (("C-c C-n" . outline-next-heading)
         ("C-c C-p" . outline-previous-heading)
         ("C-c C-u" . outline-up-heading)))

(use-package meow
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-dvorak)
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("<" . meow-beginning-of-thing)
     '(">" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-line)
     '("E" . meow-goto-line)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-join)
     '("k" . meow-kill)
     '("L" . recenter-top-bottom)
     '("l" . meow-till)
     '("m" . meow-mark-word)
     '("M" . meow-mark-symbol)
     '("n" . meow-next)
     '("N" . meow-next-expand)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-prev)
     '("P" . meow-prev-expand)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-search)
     '("t" . meow-right)
     '("T" . meow-right-expand)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-next-word)
     '("W" . meow-next-symbol)
     '("x" . meow-save)
     '("X" . meow-sync-grab)
     '("y" . meow-yank)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     ;; '("<escape>" . ignore) ;; allow esc to continue to be alt key
     ))
  :config
  (meow-setup)
  (meow-global-mode 1)
  (meow-setup-indicator))
