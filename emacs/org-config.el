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
     (format "~/Projects/blorg/drafts/%s.org" slug))))

(setq org-capture-templates
      '(("b" "Blog Post" plain
         (file capture-blog-post-file)
         (file "templates/blog-post.org"))))
