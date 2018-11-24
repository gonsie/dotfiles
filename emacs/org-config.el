;; This is called from .emacs and assumes v24+


;; Settings
(setq org-adapt-indentation nil)
(setq org-highlight-latex-and-related '(latex))

;; Packages
(use-package ob-applescript
  :ensure t)

(require 'ox-md)

(add-to-list 'load-path "~/.config/emacs/elisp/ox-jekyll/")
(require 'ox-jekyll)


;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)
   (awk . t)
   (dot . t)
   (org . t)
   (applescript . t)
   ))


;; Customize Faces
(when (require 'org nil 'noerror)
  (set-face-attribute 'org-document-info t :inherit 'shadow)
  (set-face-attribute 'org-document-info-keyword t :foreground "peru")
  (set-face-attribute 'org-document-title t :inherit 'shadow)
  (set-face-attribute 'org-level-1 t :inherit 'outline-1)
  (set-face-attribute 'org-level-4 t :foreground "SkyBlue2")
  (set-face-attribute 'org-level-5 t :inherit 'org-level-1)
  (set-face-attribute 'org-level-6 t :inherit 'org-level-2)
  (set-face-attribute 'org-level-7 t :inherit 'org-level-3)
  (set-face-attribute 'org-level-8 t :inherit 'org-level-4)
  (set-face-attribute 'org-meta-line t :foreground "peru")
  (set-face-attribute 'org-special-keyword t :inherit 'outline-4)
)


;; Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)
;(setq org-default-notes-file (concat org-directory "/capture.org"))


;; adding the <[TAB] shortcuts
(eval-after-load 'org
  '(progn
     (add-to-list 'org-structure-template-alist '("n" "#+NAME: "))
     (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n?\n:END:"))
     (add-to-list 'org-structure-template-alist '("t" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n"))
     (add-to-list 'org-structure-template-alist '("T" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n\n#+property: exported:nil\n#+options: toc:nil date:nil\n"))
     (add-to-list 'org-structure-template-alist '("E" "#+END_EXAMPLE\n?\n#+BEGIN_EXAMPLE"))
     ))


;; Functions

;; Add Org files to the agenda when we save them
(defun to-agenda-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))
;;(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

;; add exported: property with date to org files when exporting
(defun my/org-export-dispatch ()
  "updateds the exported: property before opening the dispatch"
  (interactive)
  (org-global-prop-set "exported:" (format-time-string (car org-time-stamp-formats) (current-time)))
  (org-export-dispatch))

(add-hook 'org
          '(local-set-key (kbd "C-c C-e") 'my/org-export-dispatch))

;; blog post capturing
(defun capture-blog-post-file ()
  (let* ((title (read-string "Slug: "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title))))
    (expand-file-name
     (format "~/Projects/blorg/blog/drafts/%s.org" slug))))

(setq org-capture-templates
      '(("b" "Blog Post" plain
         (file capture-blog-post-file)
         (file "templates/blog-post.org"))))
