;(advice-add 'org-edit-src-code :after #'delete-other-windows)

;; Settings
(setq org-adapt-indentation nil)
(setq org-highlight-latex-and-related '(latex))
(setq org-catch-invisible-edits 'show-and-error)
(setq org-use-speed-commands t)

(setq org-ellipsis "⤵")

;; Packages
(use-package ob-applescript
  :ensure t)

(require 'ox-md)

(add-to-list 'load-path "~/.config/emacs/elisp/ox-jekyll-md/")
(require 'ox-jekyll-md)

(require 'ox-latex)
(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))

(require 'ox-reveal)

(use-package org-ref
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite)
  :config
  (setq org-ref-bibliography-notes "~/ORG/refs.org"))

;;(require 'org-tree-slide)
(use-package org-tree-slide
  :bind  (("<f8>" . org-tree-slide-mode)
          ("<f7>" . org-tree-slide-move-previous-tree)
          ("<f9>" . org-tree-slide-move-next-tree))
  :config
  (setq org-tree-slide-skip-outline-level 4)
  (org-tree-slide-narrowing-control-profile)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-header t))

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

;; My :highlighting: syntax and export

(defun org-add-my-extra-fonts ()
  "Add highlight emphasis."
  (add-to-list 'org-font-lock-extra-keywords
               '("[^\\w]\\(\\^\\[^\n\r\t]+\\^\\)[^\\w]"
                 (1 '(face highlight invisible nil)))))

;;(add-hook 'org-font-lock-set-keywords-hook #'org-add-my-extra-fonts)

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

(add-to-list 'org-tempo-keywords-alist '("N" . "name"))


;; (1) recover the old templates
(tempo-define-template "org-title-block"
                       '("#+title: " (p) n
                         "#+author: Elsa Gonsiorowski" n
                         (concat "#+date: " (format-time-string "%B %e, %Y")) n %))
(add-to-list 'org-tempo-tags '("<t" . tempo-template-org-title-block))
(tempo-define-template "org-properties-block"
                       '(":PROPERTIES:" n
                         (p) n
                         ":END:" n %))
(add-to-list 'org-tempo-tags '("<p" . tempo-template-org-properties-block))
(tempo-define-template "org-title-options-block"
                       '("#+title: " (p) n
                         "#+author: Elsa Gonsiorowski" n
                         (concat "#+date: " (format-time-string "%B %e, %Y")) n
                         n
                         "#+property: exported:nil" n
                         "#+options: toc:nil date:nil" n %))
(add-to-list 'org-tempo-tags '("<to" . tempo-template-org-title-options-block))
(tempo-define-template "org-begin-end-block"
                       '("#+begin_" (p) n
                         "#+end_" n %))
(add-to-list 'org-tempo-tags '("<b" . tempo-template-org-begin-end-block))

;; ;; ;; (tempo-define-template "org-name"
;; ;; ;;                        '("#+NAME: " n%))

;; ;; ;; (2) get the <[TAB] shortcuts by hooking to abbrev-mode
;; ;; ;; (defun expand-tempo-tag-interactive ()
;; ;; ;;   "Expand the tempo-tag before point by calling the template."
;; ;; ;;   (interactive)
;; ;; ;;   (let (match templ)
;; ;; ;;     (undo-boundary)
;; ;; ;;     (if (dolist (tags tempo-local-tags)
;; ;; ;;           (when (setq match (tempo-find-match-string (or (cdr tags)
;; ;; ;;                                                          tempo-match-finder)))
;; ;; ;;             (when (setq templ (assoc (car match) (symbol-value (car tags))))
;; ;; ;;               (delete-region (cdr match) (point))
;; ;; ;;               (funcall (cdr templ))
;; ;; ;;               (return t))))
;; ;; ;;         ;; Return a function with 'no-self-insert to stop input.
;; ;; ;;         'expand-tempo-tag-alias
;; ;; ;;       (funcall expand-function))))


;; ;; ;; (defun expand-tempo-tag (expand-function)
;; ;; ;;   "Expand the tempo-tag before point by calling the template."
;; ;; ;;   (let (match templ)
;; ;; ;;     (undo-boundary)
;; ;; ;;     (if (dolist (tags tempo-local-tags)
;; ;; ;;           (when (setq match (tempo-find-match-string (or (cdr tags)
;; ;; ;;                                                          tempo-match-finder)))
;; ;; ;;             (when (setq templ (assoc (car match) (symbol-value (car tags))))
;; ;; ;;               (delete-region (cdr match) (point))
;; ;; ;;               (funcall (cdr templ))
;; ;; ;;               (return t))))
;; ;; ;;         ;; Return a function with 'no-self-insert to stop input.
;; ;; ;;         'expand-tempo-tag-alias
;; ;; ;;       (funcall expand-function))))
;; ;; ;;(fset 'expand-tempo-tag-alias 'expand-tempo-tag)
;; ;; ;;(put 'expand-tempo-tag 'no-self-insert t)
;; ;; ;;(add-hook 'abbrev-expand-function 'expand-tempo-tag)
;; ;; ;; (add-hook 'org-mode
;; ;; ;;           (lambda ()
;; ;; ;;             (add-function :around (local 'abbrev-expand-function)
;; ;; ;;                           #'expand-tempo-tag)))

;; (3) keep the old <[TAB] shortcuts
(when (string< org-version "9.2")
  (eval-after-load 'org
    '(progn
       (add-to-list 'org-structure-template-alist '("N" "#+NAME: "))
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

;; blog post template
(defun capture-blog-post-file ()
  (let* ((title (read-string "Slug: "))
         (slug (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase title))))
    (expand-file-name
     (format "~/Projects/blorg/blog/drafts/%s.org" slug))))

(defun my/inc-parse ()
  (let* ((incstring (split-string (and kill-ring (current-kill 0)) "	"))
         (incnumber (nth 0 incstring))
         (incname   (nth 1 incstring))
         (inctitle  (nth 2 incstring))
         (incfname  (car (split-string incname "("))))
    (concat incnumber " - " incfname "
- " incname "
- " inctitle)))

(setq org-capture-templates
      '(("b" "Blog Post" plain (file capture-blog-post-file) (file "templates/blog-post.org"))
        ("j" "Journal" entry (file+datetree "~/ORG/journal.org") "* %?\nEntered on %U\n %i\n %a")
        ("n" "Notes" entry (file "~/ORG/notes.org") "* %(my/current-timestamp) %?\n")
        ("t" "TODO" entry (file "~/ORG/inbox.org") "* TODO %?\n")
        ("i" "Incident" entry (file+datetree "~/ORG/LLNL/inc.org") "* OPEN %(my/current-timestamp) %(my/inc-parse)" :tree-type month)
        ))

;; Time Tracking
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

;(setq org-refile-use-outline-path 'file)
(setq org-refile-targets
      `((nil :maxlevel . 9)
        (my/org-project-notes :maxlevel . 3)
        ("~/ORG/projects.org" :maxlevel . 9)
        ("~/ORG/LLNL/notes.org" :maxlevel . 1)))


;; Agenda
(setq org-agenda-files '("~/ORG/projects.org"))
(setq org-agenda-use-time-grid nil)
(setq org-agenda-window-setup (quote current-window))
(setq org-agenda-skip-scheduled-if-deadline-is-shown t)
(setq org-deadline-warning-days 7)
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
           (org-element-property :begin (org-element-at-point)))) "/DONE" 'file))

(defun my/org-copy-done-tasks ()
  (interactive)
  (org-map-entries (org-copy nil "journal.org") "DONE" 'file 'comment))
