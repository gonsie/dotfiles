;; Config
(setq dashboard-startup-banner 1)
                                        ;(setq dashboard-navigation-cycle nil)
(setq dashboard-show-shortcuts nil)
(setq dashboard-item-names '(("Recent Files:" . "Recent [F]iles:")))
(dashboard-setup-startup-hook)


;; Key Bindings
(define-key dashboard-mode-map (kbd "n") 'next-line)
(define-key dashboard-mode-map (kbd "p") 'previous-line)

;;; key-bound functions

(defun my/new-frame-dashboard ()
  "Create a new frame showing the dashboard"
  (interactive)
  (display-buffer "*dashboard*" '(display-buffer-pop-up-frame . nil)))
(global-set-key (kbd "C-c n") #'my/new-frame-dashboard)

(defun my/switch-to-dashboard ()
  "Switch to the dashboard buffer"
  (interactive)
  (switch-to-buffer "*dashboard*"))
;;(global-set-key (kbd "C-c d") #'my/switch-to-dashboard)

;;; Use follow-mode to get a 2 column view of the dashboard
;;; 1. make the 2 column / 2 window view appear when switching into the dashboard
;;; 2. make the 2 window view disappear when leaving the dashboard
;;;    a. via 'q'
;;;    b. by selecting a file
;;; 3. make the projects group start "in the next column" aka add some blank space

(defun my/display-2col-dashboard ()
  "Use follow-mode to display a 2 column / 2 window dashboard"
  (interactive)
  (switch-to-buffer "*dashboard*")
  (when (> (frame-width) 125)
    (when (< (window-body-height)
           (progn
             (goto-char (point-max))
             (string-to-number (cadr (split-string (what-line))))))
      (follow-mode 1)
      (split-window-right)))
  (beginning-of-buffer))
(global-set-key (kbd "C-c d") #'my/display-2col-dashboard)

(defun my/dashboard-quit-window ()
  "Close all extra dashboard windows, then quit dashboard"
  (interactive)
  (let* ((buf-list (get-buffer-window-list "*dashboard*"))
         (cur-buff (car buf-list))
         (othr-buf (cdr buf-list)))
    (mapcar (lambda (window)
              (delete-window window))
            othr-buf))
  (quit-window))
(define-key dashboard-mode-map (kbd "q") #'my/dashboard-quit-window)

;; Helper functions

;;; widget creation helper
(defun dashboard-widget-create-existing-file (path name)
  "Helper function to create a widget for existing files"
  (insert "\n    ")
  (widget-create 'item
                 :tag name
                 :action `(lambda (&rest ignore) (my/dashboard-quit-window)(find-file-existing ,path))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"))

;; Re-implement my list of recently accessed projects
(defun my/dashboard--get-projects ()
    "Get list of items in ~/Projects"
  (let (proj-list)
    (dolist (dirl (directory-files-and-attributes "~/Projects" nil "^[^.]+.*"))
      (setq proj-list (append proj-list (list (cons (car dirl)
                                                    (format-time-string "%s" (file-attribute-access-time (cdr dirl))))))))
    (setq proj-list (sort proj-list (lambda (a b) (string> (cdr a) (cdr b)))))
    (mapcar (lambda (el)
              (setq el (concat "~/Projects/" (car el))))
            proj-list)))

(defun my/dashboard-insert-projects (list-size)
  "Add the list of recently accessed items from ~/Projects"
  (dashboard-insert-section
   "[R]ecent Projects:"
   (my/dashboard--get-projects)
   list-size
   'my-projects
   (dashboard-get-shortcut 'my-projects)
   `(lambda (&rest _)
            (my/dashboard-quit-window)
            (find-file-existing ,el))
   (abbreviate-file-name el)))
(add-to-list 'dashboard-item-generators '(my-projects . my/dashboard-insert-projects))

;; Frequents Section
(defun dashboard-insert-freqs (list-size)
  "Insert list of frequent items. LIST-SIZE does not matter."
  (dashboard-insert-heading "Frequent[s]:" nil nil)
  (when (boundp 'my/dashboard-freqs)
    (mapcar (lambda (f)
             (dashboard-widget-create-existing-file (car f)(car (cdr f))))
          my/dashboard-freqs))
  (insert "\n    ")
  (widget-create 'item
                 :tag "*eshell*"
                 :action `(lambda (&rest ignore) (progn
                                                   (switch-to-buffer (get-buffer-create "*eshell*"))
                                                   (eshell)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]")
  (dashboard-widget-create-existing-file "~/ORG/" "~/ORG/")
  (dashboard-widget-create-existing-file "~/.config/emacs" ".config/emacs")
  (dashboard-widget-create-existing-file custom-file ".emacs.d/custom")
  (dashboard-widget-create-existing-file "~/Projects/dotfiles" "dotfiles")
  (dashboard-widget-create-existing-file "~/Projects" "Projects")
  (insert "\n    ")
  (widget-create 'item
                 :tag "*scratch*"
                 :action `(lambda (&rest ignore) (switch-to-buffer (get-buffer-create "*scratch*")))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]")
  (dashboard-insert-shortcut 'freqs "s" "Frequent[s]:"))
(add-to-list 'dashboard-item-generators  '(freqs . dashboard-insert-freqs))


(setq dashboard-items '((recents . 5)
                        (freqs . 1)
                        (my-projects . 20)))

(setq dashboard-item-shortcuts '((recents  . "f")
                                 (freqs    . "s")
                                 (my-projects . "r")))
