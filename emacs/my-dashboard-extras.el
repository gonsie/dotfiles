;; Config
(setq dashboard-startup-banner 'nil)
(dashboard-setup-startup-hook)
;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Quick keys
(dashboard-insert-shortcut "f" "Recent [F]iles:")
(dashboard-insert-shortcut "a" "Agenda for today:")
(define-key dashboard-mode-map (kbd "n") 'next-line)
(define-key dashboard-mode-map (kbd "p") 'previous-line)

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
	   "[R]ecent Projects:"
	   (dashboard-subseq proj-list 0 list-size))
      (dashboard-insert-shortcut "r" "[R]ecent Projects:"))))

(add-to-list 'dashboard-item-generators  '(projects . dashboard-insert-projects))
(add-to-list 'dashboard-items '(projects . 10) t)

;; helper function
(defun dashboard-widget-create-existing-file (path name)
  "Helper function to create a widget for existing files"
  (insert "\n    ")
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (find-file-existing ,path))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"
                 (princ name)))

(defun dashboard-insert-freqs (list-size)
  (dashboard-insert-heading "Frequent[s]:")
  (insert "\n    ")
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (progn
                                                   (switch-to-buffer (get-buffer-create "*eshell*"))
                                                   (eshell)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"
                 (princ "*eshell*"))
  (dashboard-widget-create-existing-file "~/ORG/" "~/ORG/")
  (dashboard-widget-create-existing-file "~/.config/emacs" ".config/emacs")
  (dashboard-widget-create-existing-file custom-file ".emacs.d/custom")
  (dashboard-widget-create-existing-file "~/Projects/dotfiles" "dotfiles")
  (dashboard-widget-create-existing-file "~/Projects" "Projects")
  (insert "\n    ")
  (widget-create 'push-button
                 :action `(lambda (&rest ignore) (switch-to-buffer (get-buffer-create "*scratch*")))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"
                 (princ "*scratch*")))

(dashboard-insert-shortcut "s" "Frequent[s]:")
(add-to-list 'dashboard-item-generators  '(freqs . dashboard-insert-freqs))
(add-to-list 'dashboard-items '(freqs) t)

;; Define items to show
(setq dashboard-items '((recents  . 5)
                        (freqs . 1)
                        (projects . 20)))
