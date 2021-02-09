(setq eshell-scroll-to-bottom-on-input 'all)
(setq eshell-error-if-no-glob t)
(setq eshell-hist-ignoredups t)
(setq eshell-save-history-on-exit t)
(setq eshell-prefer-lisp-functions nil)
(setq eshell-destroy-buffer-when-process-dies t)


;; ALIASES
(add-hook
 'eshell-mode-hook (lambda ()
                     (eshell/alias "marco" "pwd")
                     (eshell/alias "em" "find-file $1")
                     (eshell/alias "eo" "find-file-other-window $1")
                     (eshell/alias "la" "ls -AFGhl $1")
                     (eshell/alias "d" "dired $1")))

;; alias ideas:
;;   - magit shortcut
;;   - dired shortcut


;; FUNCTIONS

;; implementation of bashmarks / fishmarks
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
  (cond ((getenv "SLURM_JOBID") (setq jobid (getenv "SLURM_JOBID")))
        ((getenv "LSB_JOBID") (setq jobid (getenv "LSB_JOBID")))
        (setq jobid nil))
  (when jobid
      (concat " [" jobid "]")))

(defun my-prompt/hostname ()
  "Return hostname.")

(setq eshell-prompt-function
      (lambda ()
        (concat
         (propertize (my-prompt/directory-name) 'face 'font-lock-type-face)
         (propertize (my-prompt/jobid) 'face 'default)
         (propertize " $ " 'face 'default))))

(setq eshell-highlight-prompt nil)
