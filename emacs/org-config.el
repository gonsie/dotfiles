;(advice-add 'org-edit-src-code :after #'delete-other-windows)

;; Settings
(setq org-adapt-indentation nil)
(setq org-highlight-latex-and-related '(latex))
(setq org-catch-invisible-edits 'show-and-error)

(setq org-ellipsis "⤵")

;; Modules


;; Packages
(use-package ob-applescript
  :ensure t)

(require 'ox-md)

(add-to-list 'load-path "~/.config/emacs/elisp/ox-jekyll-md/")
(require 'ox-jekyll-md)

;; beamer notes


(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(use-package org-ref
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-bibliography-notes "~/ORG/refs.org"))

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
  (set-face-attribute 'org-hide t :foreground "#686a71")
  )

;; My :highlighting: syntax ond export

(defun org-add-my-extra-markup ()
  "Add highlight emphasis."
  (add-to-list 'org-font-lock-extra-keywords
               '("[^\\w]\\(:\\[^\n\r\t]+:\\)[^\\w]"
                 (1 '(face highlight invisible nil)))))

(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-markup)

(defun my-html-mark-tag (text backend info)
  "Transcode :blah: into <mark>blah</mark> in body text."
  (when (org-export-derived-backend-p backend 'html)
    (let ((text (replace-regexp-in-string "[^\\w]\\(:\\)[^\n\t\r]+\\(:\\)[^\\w]" "<mark>"  text nil nil 1 nil)))
      (replace-regexp-in-string "[^\\w]\\(<mark>\\)[^\n\t\r]+\\(:\\)[^\\w]" "</mark>" text nil nil 2 nil))))

(add-to-list 'org-export-filter-plain-text-functions 'my-html-mark-tag)


;; Global Keybindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

;; Org Mode Keybindings
(add-hook 'org-mode
          (lambda () (local-set-key (kbd "C-c C-x C-r") #'org-clock-report)))
(add-hook 'org-agenda-mode-hook
          (lambda () (local-set-key (kbd "h") #'(lambda () (interactive)(Info-goto-node "(org) Agenda Views")))))

;; always save the damn file
(add-hook 'org-clock-in-hook  'save-buffer)
(add-hook 'org-clock-out-hook 'save-buffer)
(add-hook 'org-after-todo-state-change-hook 'save-buffer)
(add-hook 'org-after-refile-insert-hook 'save-buffer)

;;; ORG TEMPLATES
;; as of 9.2 the org easy templates have changed
;; (0) fix the behavior
(require 'org-tempo)
(add-hook 'org-mode-hook
          (lambda () (setq-local electric-pair-inhibit-predicate
                                `(lambda (c) (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))
;; (1) recove the old templates
(tempo-define-template "org-title-block"
                       '("#+title: " (p "Title: ") n
                         "#+author: Elsa Gonsiorowski" n
                         (concat "#+date: " (format-time-string "%B %e, %Y")) n))
(tempo-define-template "org-properties-block"
                       '(":PROPERTIES:" n
                         (p) n
                         ":END:" n%))
(tempo-define-template "org-title-options-block"
                       '("#+title: " (p "Title: ") n
                         "#+author: Elsa Gonsiorowski" n
                         (concat "#+date: " (format-time-string "%B %e, %Y")) n
                         n
                         "#+property: exported:nil" n
                         "#+options: toc:nil date:nil" n))
(tempo-define-template "org-name"
                       '("#+NAME: " n))
;(tempo-define-template "org-")

;; (2) get the <[TAB] shortcuts

;; (3) keep the old <[TAB] shortcuts
(when (string< org-version "9.2")
  (eval-after-load 'org
    '(progn
       (add-to-list 'org-structure-template-alist '("n" "#+NAME: "))
       (add-to-list 'org-structure-template-alist '("p" ":PROPERTIES:\n?\n:END:"))
       (add-to-list 'org-structure-template-alist '("t" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n"))
       (add-to-list 'org-structure-template-alist '("T" "#+title: ?\n#+author: Elsa Gonsiorowski\n#+date: \n\n#+property: exported:nil\n#+options: toc:nil date:nil\n"))
       (add-to-list 'org-structure-template-alist '("E" "#+END_EXAMPLE\n?\n#+BEGIN_EXAMPLE"))
       (add-to-list 'org-structure-template-alist '("b" "#+BEGIN_?\n\n#+END_")))))

;; Functions

;; my preferred plain-text timestamps
(defun my/current-timestamp nil
    (interactive)
  (format-time-string "%Y-%m-%d" (current-time)))

;; Capturing

;; blog post capturing
(defun capture-blog-post-file ()
  (let* ((title (read-string "Slug: "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title))))
    (expand-file-name
     (format "~/Projects/blorg/blog/drafts/%s.org" slug))))

(setq org-capture-templates
      '(("b" "Blog Post" plain (file capture-blog-post-file) (file "templates/blog-post.org"))
        ("j" "Journal" entry (file+datetree "~/ORG/journal.org") "* %?\nEntered on %U\n %i\n %a")
        ("n" "Notes" entry (file "~/ORG/notes.org") "* %(my/current-timestamp) %?\n")
        ("t" "TODO" entry (file "~/ORG/inbox.org") "* TODO %?\n%a\n")))

;; Time tracking
(setq org-time-stamp-rounding-minutes (quote (0 30)))


;; Archive
(setq org-archive-location "~/ORG/archive.org::* From %s")


;; Refile
;; refile targets to any ~/Projects/XXXX/notes.org location
(defun my/org-project-notes ()
  (let (proj-list)
    (dolist (dirl (directory-files "~/Projects" nil "^[^.]+.*"))
      (setq proj-list (append proj-list
                              (directory-files (concat "~/Projects/" dirl) t "^notes.org" nil))))
    (remove nil proj-list)))

(setq org-refile-targets
      `((nil :maxlevel . 9)
        (my/org-project-notes :maxlevel . 3)
        ("~/ORG/projects.org" :maxlevel . 9)))


;; Agenda
(setq org-agenda-files '("~/ORG/projects.org"))
(setq org-agenda-use-time-grid nil)
(setq org-agenda-window-setup (quote current-window))
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-todo-ignore-deadlines (quote all))
(setq org-agenda-todo-ignore-scheduled (quote all))
(setq org-agenda-remove-tags t)
(setq org-agenda-sorting-strategy
  (quote
   ((agenda deadline-up priority-down)
    (todo priority-down category-keep)
    (tags priority-down category-keep)
    (search category-keep))))
(setq org-agenda-custom-commands
      '(("x" "Agenda & TODOs"
         ((agenda "")
          (alltodo "")))))

;; Add Org files to the agenda when we save them
(defun to-agenda-on-save-org-mode-file()
  (when (string= (message "%s" major-mode) "org-mode")
    (org-agenda-file-to-front)))
;;(add-hook 'after-save-hook 'to-agenda-on-save-org-mode-file)

;; Copy Completed Items to Journal
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from
           (org-element-property :begin (org-element-at-point)))) "/DONE" 'tree))

(defun my/org-copy-done-tasks ()
  (interactive)
  (org-map-entries (org-copy nil "journal.org") "DONE" 'file 'comment))
