;; 1. Connect to MELPA

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(package-initialize)

;; Install by hand:
;; - use-package
;; - org

;; PACKAGES

;; Personally Included Packages
(add-to-list 'load-path "~/.config/emacs/elisp")

;; autopair
;; (autopair-global-mode)
;; (diminish 'autopair-mode)
(electric-pair-mode)

;; Linum
(setq linum-format "%d ")

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

;; Word Count (dotfile copy)
(load "wc")

;; ORG

;; keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
;(setq org-default-notes-file (concat org-directory "/capture.org"))

;; settings
(setq org-adapt-indentation nil)

;; org export
(require 'ox-md)

;; active Babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (sh . t)
   (dot . t)
   (org . t)
   ))

;; adding the <[TAB] shortcuts
(eval-after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist '("n" "#+NAME: "))
     (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n?\n:END:"))
     (add-to-list 'org-structure-template-alist '("t" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n"))
     (add-to-list 'org-structure-template-alist '("T" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n\n#+property: exported: nil\n#+options:\n"))
     (add-to-list 'org-structure-template-alist '("E" "#+END_EXAMPLE\n?\n#+BEGIN_EXAMPLE"))
     ))

;; Add Org files to the agenda when we save them
(defun to-agenda-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))

;;(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)
