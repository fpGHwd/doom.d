;;; ../../Sync/dotfiles/doom.d/lib/lib-misc.el -*- lexical-binding: t; -*-


(defun surround-quotes (&optional arg)
  "Enclose following ARG sexps in quotes.
Leave point after open-quote."
  (interactive "*P")
  (insert-pair arg ?\" ?\"))


;; open vscode in current line
(defun +wd/open-with-vscode ()
  "Open current file with vscode."
  (interactive)
  (let ((line (number-to-string (line-number-at-pos)))
        (column (number-to-string (current-column))))
    (apply 'call-process "code" nil nil nil (list (concat buffer-file-name ":" line ":" column) "--goto"))))


(defun +wd/update-bash-history (&optional args)
  "Update bash history hourly."
  (interactive)
  (let* ((dotdrop-cmd (executable-find "dotdrop"))
         (dotfiles-dir (file-truename "~/Sync/dotfiles/"))
         (bash-eternal-history (expand-file-name "~/.config/bash/bash_eternal_history"))
         (bash-eternal-history-dotdrop (expand-file-name (concat dotfiles-dir "config/bash/bash_eternal_history-ubuntu2004")))
         (bash-eternal-history-tmp (expand-file-name (concat dotfiles-dir "config/bash/bash_eternal_history-bak-ubuntu2004"))))
    (when dotdrop-cmd
      (let ((cmd (format "%s -c %s update --force -k f_bash_eternal_history" dotdrop-cmd (concat dotfiles-dir "dotdrop-config.yaml")))) ;; 更新的是 bak 文件
        ;; (message "%s" cmd)
        (shell-command-to-string cmd)
        (let* ((size1 (nth 7 (file-attributes bash-eternal-history-dotdrop)))
               (size2 (nth 7 (file-attributes bash-eternal-history-tmp))))
          (if (> size2 size1)
              (copy-file bash-eternal-history-tmp bash-eternal-history-dotdrop t)
            (copy-file bash-eternal-history-dotdrop bash-eternal-history t)))))))

(defun +wd/magit-push-to-gerrit (arg)
  "Push HEAD to remote branch. SAIC limited.
The `ARG` parameter is used to distinguish whether to use current branch or specify a remote branch.
1 to specify a remote branch, nil current branch to remote same branch."
  (interactive "p")
  (let* ((current-branch (magit-get-current-branch))
         (remote-name (magit-read-remote "select remote"))
         (gitlab-url (magit-get "remote" remote-name "url"))
         (gerrit-url (replace-regexp-in-string "\\(^https?://[^/]+/\\)" "\\1a/" gitlab-url))
         (remote-branch (pcase arg
                          (1 (replace-regexp-in-string ".*/" "" (magit-read-remote-branch "remote branch")))
                          (_ current-branch))))
    (magit-git-command
     (concat "git push " gerrit-url " HEAD:refs/for/" remote-branch))))


(defun +wd/add-book-to-calibre ()
    (interactive)
    (let* ((postfix (list ".pdf" ".epub" ".mobi" ".azw" ".azw3"))
           (library "calibre-lib")
           (path-list (mapcar #'file-truename '("~/Downloads"
                                                "~/Sync"
                                                "~/")))
           (bin-path (executable-find "calibredb"))
           (password (password-store-get "calibre-lib/wd"))
           (bash-path (executable-find "bash")))
      (dolist (pa path-list)
        (dolist (pf postfix)
          (when (file-directory-p pa)
            (mapcar (lambda (path)
                      (let* ((cmd (concat bin-path
                                          " --with-library=http://arch-nuc.lan:8080/\#" library
                                          " --username=wd"
                                          " --password=" password
                                          " --duplicates add " "'" path "'")))
                        ;; (message "cmd is [%s]" cmd)
                        (set-process-sentinel
                         (start-process "calibredb-add-book" nil bash-path "-c" cmd)
                         (lambda (proc event)
                           ;; (message "event: %s" event)
                           (when (string-equal event "finished\n")
                             (delete-file path)
                             (message "Add to calibre & delete origin: %s" path))))))
                    (directory-files pa t pf)))))))


;; for workspace
(defun +wd/update-current-workspaces-to-saved-ones ()
  (interactive)
  (let* ((+workspaces-data-file (concat (system-name) "_workspaces"))
         (current-ws (+workspace-list-names))
         (saved-ws (persp-list-persp-names-in-file
                    (expand-file-name +workspaces-data-file persp-save-dir)))
         (intersection (cl-intersection current-ws saved-ws :test 'equal)))
    (dolist (ws intersection)
      (+workspace-save ws))))

;; rime
(defun +wd/sync-emacs-rime-dict ()
    "Sync EMACS rime dictionary to git repository with default remote."
    (interactive)
    (let* ((rime-dir "~/.config/rime")
           (tmp-dir (concat rime-dir "/sync/tmp"))
           (tmp-file (concat tmp-dir "/rime_ice.userdb.txt"))
           (dict-file-relative "sync/rime-emacs/rime_ice.userdb.txt")
           (dict-file (concat rime-dir "/" dict-file-relative))
           (default-directory rime-dir)
           (commit-message (concat "update by elisp on " (format-time-string "%Y/%m/%d %H:%M:%S")))
           (upstream (magit-get-upstream-branch)))
      (mkdir tmp-dir t)
      (copy-file dict-file tmp-file t)
      (require 'magit)
      (magit-fetch-all-prune)           ; error handle
      (magit-reset-hard upstream)
      (rime-sync)
      (magit-stage-file dict-file-relative)
      (magit-commit-create `("--all" "-m" ,commit-message))
      (magit-push-current-to-upstream nil)))

(provide 'lib-misc)
