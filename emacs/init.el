;; 1. Connect to MELPA

;; Package configs
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
			 ("gnu"   . "http://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))
(unless package--initialized (package-initialize t))

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; Install by hand:
;; - org

;; PACKAGES

;; Personally Included Packages
(add-to-list 'load-path "~/.config/emacs/elisp")

;; autopair
;; (autopair-global-mode)
;; (diminish 'autopair-mode)
(electric-pair-mode)

;; dired-sidebar
(use-package dired-sidebar
  :bind ([f8] . dired-sidebar-toggle-sidebar)
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

;; CMake
(use-package cmake-mode
  :mode ("\\.cmake\\'"
         "CMakeLists\\.txt\\'")
  :config (use-package cmake-font-lock))

;; MAGIT
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (set-face-attribute 'magit-diff-context-highlight t :background "grey45" :foreground "grey50")
  (set-face-attribute 'magit-section-highlight t :background "grey45")
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

;; rust
(use-package rust-mode
  :mode ("\\.rs\\'"))

;; graphviz
(use-package graphviz-dot-mode
  :mode ("\\.gv\\'"))

;; swiper pop-up search
(use-package swiper
  :ensure t
  :diminish ivy-mode
  :bind (("C-s" . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-c j" . ivy-immediate-done))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virutal-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-display-style 'fancy)
  (setq ivy-count-format "(%d/%d) "))

;; dashboard
(use-package dashboard
  :ensure t
  :diminish page-break-lines-mode
  :config
  (setq dashboard-startup-banner 'nil)
  (dashboard-setup-startup-hook))

(defun my/new-frame-dashboard ()
  "Create a new frame showing the dashboard"
  (interactive)
  (display-buffer (get-buffer "*dashboard*") '(display-buffer-pop-up-frame . nil)))
(global-set-key (kbd "C-c n") #'my/new-frame-dashboard)

(use-package auto-dim-other-buffers
  :ensure t
  :diminish auto-dim-other-buffers-mode
  :init (auto-dim-other-buffers-mode t)
  :config
  (set-face-attribute 'auto-dim-other-buffers-face nil
                      :background "#42444C"))

;; Word Count (dotfile copy)
(load "wc")
