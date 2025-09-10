;;; ../../Sync/dotfiles/doom.d/lisp/init-work.el -*- lexical-binding: t; -*-


(defcustom +wd/tag-system-root "~/windows_share_dir/reference"
  "A customizable variable for setting tag system root path."
  :type 'string
  :group 'Melt)

;; TODO: 设置为 customizable item
(defvar +wd/git-repos-to-update
  (mapcar #'expand-file-name
       '(
         ;; "~/Sync/project/real-world-haskell"
         ;; "~/Sync/project/magic-haskell"
         "~/Sync/org"
         "~/Sync/dotfiles")))

;;;###autoload
(defun +wd/pull-repos ()
  (interactive)
  (require 'magit)
  (let ((connectable (check-http-server-status "https://www.baidu.com")))
    (dolist (repo +wd/git-repos-to-update)
      (let ((default-directory repo))
        (when connectable
          (progn
            (magit-run-git "fetch" "origin")
            (magit-run-git "rebase" "main")))))))

;;;###autoload
(defun +wd/push-repos ()
  (interactive)
  (require 'magit)
  (let ((connectable (check-http-server-status "https://www.baidu.com")))
    (dolist (repo +wd/git-repos-to-update)
      (let ((default-directory repo))
        (when connectable
          (progn
            (message "Updating %s" repo)
            (magit-staged-files)
            (magit-git-command "add" "./*")
            ;; (magit-run-git "commit" "-am" "auto update")
            ;; (magit-run-git "push" "origin" "main")
            ))))))

;;;###autoload
(defun +wd/print-directory-tree ()
  ;; (interactive)
  (write-region
   (shell-command-to-string (concat (executable-find "tree") " -d " +wd/tag-system-root
                                    " -L 4"
                                    " -I \"0000\""
                                    " -I \"Pictures\""
                                    " -I \"Download\""
                                    " -I \"202*\""
                                    " -I \"tools\""
                                    " -I \"tag-info.txt\""))
   nil (concat +wd/tag-system-root "/tag-info")))


;;;###autoload
(defun +wd/delete-40d-before-directory ()
  ;; read directory in a path and return a list, use elisp
  ;; get now time month and day in format "%02M%02D", use elisp
  ;; get now time %m%d before 60 day %m%d format, use format-time-string
  ;; elisp string compare function
  ;; rx match 4 numbers
  ;; delete some directory force , use elisp
  ;; (interactive)
  (let* ((path "~/windows_share_dir/reference/2025/")
         (dirs (directory-files path nil (rx (= 4 digit))))
         (b40d (format-time-string "%m%d" (time-add (current-time) (days-to-time -40)))))
    (dolist (d dirs)
      (when (string< d b40d)
        (delete-directory (concat path d) t)))))


;;;###autoload
(defun file-busy-p (filename)
  "检查 FILENAME 是否被其他进程打开."
  (let ((output (shell-command-to-string (format "lsof %s" filename))))
    (not (string-empty-p output))))

;;;###autoload
(defun +wd/share-org-roam-daily ()
 (interactive)
 (require 'cl-lib)
 (let* ((path-of-external-org-daily "/home/wd/windows_share_dir/SA/roam/")
        (path-of-org-daily "/home/wd/Sync/org/roam/daily/")
        (files-in-directory (let ((files (directory-files path-of-external-org-daily t "\[^.\]")))
                              (mapcar 'file-name-nondirectory files)))
        (name (format-time-string "%Y-%m-%d.org"))
        (source (concat path-of-org-daily name))
        (target (concat path-of-external-org-daily name)))
   (require 'org-roam)
   (save-excursion
     (let ((org-roam-directory "/home/wd/windows_share_dir/SA/roam/")
           (org-roam-dailies-directory ""))
       (org-roam-dailies-goto-today))
     (when (not (and (file-symlink-p source)
                     (string= (file-truename source) target)))
       (make-symbolic-link target source t)))
   (dolist (f (cl-set-difference files-in-directory (list name) :test #'equal))
     (let ((win-file (concat path-of-external-org-daily f))
           (linux-file (concat path-of-org-daily f)))
       (when (file-exists-p win-file)
         (when (file-symlink-p linux-file)
           (delete-file linux-file t))
         (copy-file win-file linux-file t)
         (when (not (file-busy-p win-file))
           (delete-file win-file)))))))


(defcustom +wd/reference-log-root "~/windows_share_dir/reference"
  "A customizable variable for project root when update Quectel SDK."
  :type 'string
  :group '+wd/reference-group)


;;;###autoload
(defun check-http-server-status (url)
  "Check if the specified HTTP URL is accessible."
  (let ((response-buffer (url-retrieve-synchronously url))
        status-code)
    (if response-buffer
        (with-current-buffer response-buffer
          ;; 提取 HTTP 响应状态码
          (goto-char (point-min))
          (if (re-search-forward "HTTP/[^ ]+ \\([0-9]+\\)" nil t)
              (setq status-code (string-to-number (match-string 1))))
          ;; 清理临时缓冲区
          (kill-buffer response-buffer))
      (setq status-code nil))
    ;; 返回检测结果
    (if (and status-code (>= status-code 200) (< status-code 300))
        t
      ;; (message "Website is accessible, status code: %s" status-code)
      ;; (message "Website is not accessible, status code: %s" status-code)
      nil)))


(let ((vector `(("08:00" #'+wd/update-bash-history (* 10 60))
                    ;;("18:00" #'+wd/push-repos (* 24 60 60))
                    ("08:00" #'+wd/print-directory-tree (* 24 60 60))
                    ("08:00" #'+wd/delete-40d-before-directory (* 24 60 60))
                    ;; ("08:00" #'+wd/share-org-roam-daily (* 24 60 60))
                    )))
    (dolist (i vector)
      (let ((start (nth 0 i))
            (func (nth 1 i))
            (interval (nth 2 i)))
        (cancel-function-timers func)
        (run-at-time start interval func))))

;;;###autoload
(defun +wd/tag-add-file (file category tag date-flag)
  "A function used to add file to tag system."
  (let ((file-path (concat +wd/tag-system-root
                           "/" category
                           "/" tag
                           (if date-flag
                               (format-time-string "/%Y-%m-%d")))))
    (when (not (file-exists-p file-path))
      (make-directory file-path t))
    (if (file-directory-p file)
        (copy-directory file (concat file-path "/" (file-name-nondirectory file)) t t)
      (copy-file file (concat file-path "/" (file-name-nondirectory file)) t))))
;; 如何对 dired 打开的某个文件写一个自定义函数进行操作，将其复制到另外的目录中

;; add kernel tags
;; (setq tags-file-name "~/datb/ag590/kernel/qualcomm/ag590/TAGS")

(provide 'init-work)
