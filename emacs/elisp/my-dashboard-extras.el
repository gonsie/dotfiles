;; Config
(setq dashboard-startup-banner 'nil)
(dashboard-setup-startup-hook)
;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Quick keys
(dashboard-insert-shortcut "f" "Recent Files:")
(dashboard-insert-shortcut "a" "Agenda for today:")

;; Key-bound functions for dashboards

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


;; Insert Project list

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


;; Insert *scratch* link

(defun dashboard-insert-scratch (list-size)
  (dashboard-insert-heading "Scratch:")
  (insert "\n    ")
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (switch-to-buffer (get-buffer-create "*scratch*")))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"
                 (princ "*scratch*"))
  (dashboard-insert-shortcut "s" "Scratch:"))
(add-to-list 'dashboard-item-generators  '(scratch . dashboard-insert-scratch))
(add-to-list 'dashboard-items '(scratch) t)

;; Define items to show
(setq dashboard-items '((recents  . 5)
                        (scratch . 1)
                        (projects . 10)))
