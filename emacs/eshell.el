(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-error-if-no-glob t)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)
(setq eshell-prefer-lisp-functions nil)
(setq eshell-destroy-buffer-when-process-dies t)
(setq eshell-aliases-file "~/.config/emacs/eshell-aliases")

;; FUNCTIONS

;; open file and magit shortcut
(defun eshell/mg (&optional arg)
  "Change to file and magit window. Default show current directory."
  (if (= (length arg) 0)
      (find-file ".")
    (find-file arg))
  (magit))

;; implementation of bashmarks / fishmarks
;; only [c] / jump command is implemented
;; save, list, delete commands are not implemented
(defun eshell/c (&optional arg)
  "Change to bookmark'd directory"
  (if (= (length arg) 0)
      (eshell/cd)
    (eshell/cd
     (let ((sdirs (with-temp-buffer
                    (insert-file-contents "~/.sdirs")
                    (buffer-substring-no-properties (point-min) (point-max)))))
       (string-match (concat "DIR_" arg " \\(.*\\)") sdirs)
       (match-string 1 sdirs)))))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))


;; PROMPT

(defun my-prompt/git-branch-string (pwd)
  "Returns current git branch as string."
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git"))
    (let ((git-branches (shell-command-to-string "git branch -q --no-color --points-at HEAD --format '%(refname)'")))
      (eshell/basename git-branches))))

    ;; (let ((git-branches (shell-command-to-string "git branch --no-color 2> /dev/null")))
    ;;   (string-match "\\* \\(.*\\)" git-branches)
    ;;   (match-string 1 git-branches))))

(defun my-prompt/directory-name (&optional path)
  "Get last directory in a path"
  (let ((path (if path
                  path
                (concat (eshell/pwd) "/"))))
    (file-name-nondirectory (directory-file-name (file-name-directory path)))))

(defun my-prompt/jobid ()
  "Return job id if within an allocation."
  (cond ((getenv "SLURM_JOBID") (concat " [" (getenv "SLURM_JOBID") "]"))
        ((getenv "LSB_JOBID") (concat "[" (getenv "LSB_JOBID") "]"))
        ("")))

(defun my-prompt/hostname ()
  "Return hostname."
  (let ((hostname (split-string
                   (if (string-match-p "ssh:" (pwd))
                       (file-remote-p default-directory 'host)
                     (system-name))
                   "\\.")))
    (concat " " (car hostname) " ")))

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (my-prompt/hostname) 'face 'font-lock-constant-face)
         (propertize (my-prompt/directory-name) 'face 'font-lock-type-face)
         (propertize (my-prompt/jobid) 'face 'default)
         (propertize " $ " 'face 'default))))

(setq eshell-highlight-prompt nil)
