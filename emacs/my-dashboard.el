;; Config
(setq dashboard-startup-banner t)
(setq dashboard-show-shortcuts 'nil)
(setq dashboard-item-names '(("Recent Files:" . "Recent [F]iles:")
                             ("Agenda for today:" . "Today:")
                             ("Agenda for the coming week:" . "Agenda:")))
(dashboard-setup-startup-hook)
;(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

;; Quick keys
(dashboard-insert-shortcut (dashboard-get-shortcut 'recents) "f" "Recent Files:")
(dashboard-insert-shortcut (dashboard-get-shortcut 'agenda) "a" "Agenda for today:")
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
;;(global-set-key (kbd "C-c d") #'my/switch-to-dashboard)

;;; TODO:
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
  (follow-mode 1)
  (split-window-right))
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
  (quit-window)
  (follow-mode -1))
(define-key dashboard-mode-map (kbd "q") #'my/dashboard-quit-window)

;; widget creation helper
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

;; Project List Section
(defun dashboard-insert-project-list-item (list-display-name list)
  "Render LIST-DISPLAY-NAME title and items of LIST."
  (when (car list)
    (dashboard-insert-heading list-display-name)
    (mapc (lambda (el)
            (let ((el (concat "~/Projects/" (car el))))
              (dashboard-widget-create-existing-file el (abbreviate-file-name el))))
          list)))

(defun dashboard-insert-projects (list-size)
  "Add the list of LIST-SIZE items from recently accessed projects."
  (let (proj-list)
    (dolist (dirl (directory-files-and-attributes "~/Projects" nil "^[^.]+.*"))
      (setq proj-list (append proj-list (list (cons (car dirl)
                                              (format-time-string "%s" (file-attribute-access-time (cdr dirl))))))))
    (setq proj-list (sort proj-list (lambda (a b) (string> (cdr a) (cdr b)))))
    (when (dashboard-insert-project-list-item
	   "[R]ecent Projects:"
	   (dashboard-subseq proj-list list-size))
      (dashboard-insert-shortcut (dashboard-get-shortcut 'projects) "r" "[R]ecent Projects:")
      )))

(add-to-list 'dashboard-item-generators  '(projects . dashboard-insert-projects))
(add-to-list 'dashboard-items '(projects . 10) t)

;; Frequents Section

(defun dashboard-insert-freqs (list-size)
  (dashboard-insert-heading "Frequent[s]:")
  ;; TODO: machine-specific freq items
  (when (boundp 'my/dashboard-freqs)
    (mapcar '(lambda (f)
               (dashboard-widget-create-existing-file (car f)(car (cdr f))))
            my/dashboard-freqs))
  (insert "\n    ")
  (widget-create 'item
                 :tag "*eshell*"
                 :action `(lambda (&rest ignore) (progn
                                                   (my/dashboard-quit-window)
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
                 :action `(lambda (&rest ignore) (progn
                                                   (my/dashboard-quit-window)
                                                   (switch-to-buffer (get-buffer-create "*scratch*"))))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 :button-prefix ""
                 :button-suffix ""
                 :format "%[%t%]"))

(dashboard-insert-shortcut (dashboard-get-shortcut 'freqs) "s" "Frequent[s]:")
(add-to-list 'dashboard-item-generators  '(freqs . dashboard-insert-freqs))
(add-to-list 'dashboard-items '(freqs) t)

;; Extra Space Section
(defun dashboard-generate-extraspace (list-size)
  "Add some extra newlines. Used to improve the 2 column view."
  (dotimes (i list-size)
           (insert "\n")))

(add-to-list 'dashboard-item-generators '(extraspace . dashboard-generate-extraspace))
(add-to-list 'dashboard-items '(extraspace) t)

;; Define items to show
(setq dashboard-items '((recents . 5)
                        (freqs . 1)
                        (extraspace . 14)
                        (projects . 20)))
