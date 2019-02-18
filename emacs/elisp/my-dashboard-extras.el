(defun my/new-frame-dashboard ()
  "Create a new frame showing the dashboard"
  (interactive)
  (display-buffer "*dashboard*" '(display-buffer-pop-up-frame . nil)))
(global-set-key (kbd "C-c n") #'my/new-frame-dashboard)

(defun my/switch-to-dashboard ()
  "Switch to the dashboard buffer"
  (interactive)
  (switch-to-buffer "*dashboard*"))
(global-set-key (kbd "C-c d") #'my/switch-to-dashboard)

(defun dashboard-insert-project-list (list-display-name list)
  "Render LIST-DISPLAY-NAME title and items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
    (mapc (lambda (el)
            (setq el (concat "~/Projects/" (car el)))
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items from recently accessed projects."
  (let (proj-list)
    (dolist (dirl (directory-files-and-attributes "~/Projects" nil "^[^.]+.*"))
      (setq proj-list (append proj-list (list (cons (car dirl)
                                              (format-time-string "%s" (file-attribute-access-time (cdr dirl))))))))
    (setq proj-list (sort proj-list (lambda (a b) (string> (cdr a) (cdr b)))))
    (when (dashboard-insert-project-list
	   "Recent Projects:"
	   (dashboard-subseq proj-list 0 list-size))
      (dashboard-insert-shortcut "p" "Recent Projects:"))))

(add-to-list 'dashboard-item-generators  '(projects . dashboard-insert-projects))
(add-to-list 'dashboard-items '(projects . 10) t)
