;;; ../../Sync/dotfiles/doom.d/lib/lib-init-roam.el -*- lexical-binding: t; -*-

(require 'vulpea)

;; dynamic agenda https://github.com/brianmcgillion/doomd/blob/master/config.org
;; https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
;; The 'roam-agenda' tag is used to tell vulpea that there is a todo item in this file
(defun vulpea-buffer-p ()
  "Return non-nil if the currently visited buffer is a note."
  (and buffer-file-name
       (or (string-prefix-p
            (expand-file-name (file-name-as-directory "~/Sync/org/roam"))
            (file-name-directory buffer-file-name))
           (string-prefix-p
            (expand-file-name (file-name-as-directory "~/Sync/org/work/zone/roam"))
            (file-name-directory buffer-file-name)))))

(defun vulpea-project-p ()
  "Return non-nil if current buffer has any todo entry.

TODO entries marked as done are ignored, meaning the this
function returns nil if current buffer contains only completed
tasks.

Return t if active SCHEDULED/DEADLINE in property within a headline."
  (or (seq-find                       ; (3)
       (lambda (type)
         (eq type 'todo))
       (org-element-map                         ; (2)
           (org-element-parse-buffer 'headline) ; (1)
           'headline
         (lambda (h)
           (org-element-property :todo-type h))))
      (seq-find
       (lambda (type)
         (and (or (eq (org-element-property :type (nth 0 type)) 'active)
                  (eq (org-element-property :type (nth 0 type)) 'active-range)
                  (eq (org-element-property :type (nth 1 type)) 'active)
                  (eq (org-element-property :type (nth 1 type)) 'active-range))
              (not (nth 2 type))))
       (org-element-map
           (org-element-parse-buffer 'headline)
           'headline
         (lambda (h)
           (list (org-element-property :scheduled h)
                 (org-element-property :deadline h)
                 (org-element-property :closed h)))))))

(defun vulpea-project-update-tag (&optional arg)
  "Update PROJECT tag in the current buffer."
  (interactive "P")
  (when (and (not (active-minibuffer-window))
             (vulpea-buffer-p))
    (save-excursion
      (goto-char (point-min))
      (let* ((tags (vulpea-buffer-tags-get))
             (original-tags tags))
        (if (vulpea-project-p)
            (setq tags (cons "roam-agenda" tags))
          (setq tags (remove "roam-agenda" tags)))

        ;; cleanup duplicates
        (setq tags (seq-uniq tags))

        ;; update tags if changed
        (when (or (seq-difference tags original-tags)
                  (seq-difference original-tags tags))
          (apply #'vulpea-buffer-tags-set tags))))))

;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(defun my/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun my/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (my/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun dynamic-agenda-files-advice (orig-val)
  (let ((roam-agenda-files (delete-dups (my/org-roam-list-notes-by-tag "roam-agenda"))))
    (cl-union orig-val roam-agenda-files :test #'equal)))

(defun blog-post (title)
  (interactive "sEnter title: ")
  (let ((post-file (concat "~/Sync/org/blog/jekyll/_posts/"
                           (format-time-string "%Y-%m-%d")
                           "-"
                           title
                           ".org")))
    (progn
      (switch-to-buffer (find-file-noselect post-file))
      (insert (concat "#+startup: showall\n"
                      "#+options: toc:nil\n"
                      "#+begin_export html\n"
                      "---\n"
                      "layout     : post\n"
                      "title      : \n"
                      "categories : \n"
                      "tags       : \n"
                      "date       : \"" (format-time-string "%Y-%m-%d %H:%M:%S") "\"\n"
                      "---\n"
                      "#+end_export\n"
                      "#+TOC: headlines 2\n")))))

;; publish (project for blog)
(defun publish-project (project no-cache)
  (interactive "sName of project: \nsNo-cache?[y/n] ")
  (if (or (string= no-cache "y")
          (string= no-cache "Y"))
      (setq org-publish-use-timestamps-flag nil))
  (org-publish-project project)
  (setq org-publish-use-timestamps-flag t))

(defun +wd/handle-image-in-html (origin-name html-name)
  (with-temp-buffer
    (insert-file-contents html-name)
    (goto-char (point-min))
    (let ((match-content ""))
      (while (re-search-forward "\"\\(file:///.*?\\)\"" nil t)
        (setq match-content (match-string 1 nil))
        (let* ((image-name (file-name-nondirectory match-content))
               (image-extension (file-name-extension image-name))
               (image-src-path (substring match-content (length "file://")))
               (image-deploy-prefix (pcase system-name
                                      ("arch-nuc" "/home/wd/Sync/blog/jekyll/images/")
                                      ("macbook-m1-pro" "/Users/wd/Sync/blog/jekyll/images/")))
               (home-prefix (pcase system-name
                              ("arch-nuc" "/home/wd/")
                              ("macbook-m1-pro" "/Users/wd/")))
               (image-src-prefix (pcase system-name
                                   ("arch-nuc" "/home/wd/Sync/org/.")
                                   ("macbook-m1-pro" "/Users/wd/Sync/org/.")))
               (image-from-org-p (or (string-prefix-p "/home/wd/Sync/org/." image-src-path)
                                     (string-prefix-p "/Users/wd/Sync/org/." image-src-path)))
               (image-deploy-path (concat image-deploy-prefix
                                          (substring (file-name-directory image-src-path)
                                                     (if image-from-org-p (length image-src-prefix)
                                                       (length home-prefix)))))
               (image-deploy-name (if (string-prefix-p "_" image-name)
                                      (substring image-name 1)
                                    image-name))
               (image-src-link (concat "/images/"
                                       (substring (file-name-directory image-src-path)
                                                  (if image-from-org-p (length image-src-prefix)
                                                    (length home-prefix))) ;; only keep file in home directory ;; can rewrite to only trim /home/wd prefix
                                       image-deploy-name))
               (image-handle-cmd (concat (executable-find "magick")
                                         " "
                                         image-src-path
                                         " -strip -resize 60% -quality 85% "
                                         (concat image-deploy-path image-deploy-name))))
          (make-directory image-deploy-path t)
          ;; (message "cmd is %s" image-handle-cmd)
          ;; replace first, then execute command
          (replace-match image-src-link nil nil nil 1)
          ;; (copy-file image-src-path (concat image-deploy-path image-deploy-name) t)
          (when  (string= image-extension "png")
            ;; (string= image-extension "gif")
            (shell-command image-handle-cmd "*org-publish-image-handle*")))))
    (write-region (point-min) (point-max) html-name)))

(defun my/org-insert-updated-timestamp (backend)
  "Insert/update #+LAST_MODIFIED line before export."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^updated    :.*$" nil t)
        (replace-match (concat "updated    : \"" (format-time-string "%Y-%m-%d %H:%M:%S") "\""))
      (when (re-search-forward "^---\n#\\+end_export$" nil t)
        (beginning-of-line)
        (forward-line -1)
        (open-line 1)
        (insert (concat "updated    : \"" (format-time-string "%Y-%m-%d %H:%M:%S") "\""))))))


(defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
  (if year
      (let* ((d-date (diary-make-date lunar-month lunar-day year))
             (a-date (calendar-absolute-from-gregorian d-date))
             (c-date (calendar-chinese-from-absolute a-date))
             (cycle (car c-date))
             (yy (cadr c-date))
             (y (+ (* 100 cycle) yy)))
        (diary-chinese-anniversary lunar-month lunar-day y mark))
    (diary-chinese-anniversary lunar-month lunar-day year mark)))

(defun +wd/org-split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.

SEPARATORS is a regular expression.  When nil, it defaults to
\"[ \f\t\n\r\v]+\".

Unlike `split-string', matching SEPARATORS at the beginning and
end of string are ignored."
  (let ((separators (or separators "[ \f\t\n\r\v]+")))
    (if (not (string-match separators string)) (list string)
      (let ((i (match-end 0))
            (results
             (and (/= 0 (match-beginning 0)) ;skip leading separator
                  (list (substring string 0 (match-beginning 0))))))
        (while (string-match separators string i)
          (push (substring string (- i 1) i) results) ; 将 seperator 添加进去
          (push (substring string i (match-beginning 0))
                results)
          (setq i (match-end 0)))
        (push (substring string (- i 1) i) results) ; 增加最后的 seperator
        (nreverse (if (= i (length string))
                      results         ;skip trailing separator
                    (cons (substring string i) results)))))))

;; copy link from org-link
;; https ://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
(defun +wd/org-link-copy (&optional arg)
  "Extract URL from org-mode link and add it to kill ring."
  (interactive "P")
  (let* ((link (org-element-lineage (org-element-context) '(link) t))
         (type (org-element-property :type link))
         (url (org-element-property :path link))
         (url (concat type ":" url)))
    (kill-new url)
    (message (concat "Copied URL: " url))))

(defun +wd/org-search-by-tags (org-match-string)
  "Use org-tags-view within org and roam files, via `org-search-view`."
  (let* ((tag-operator-list (+wd/org-split-string org-match-string "[+|-]"))
         (search-string "-{^\\\*+ \\(KILL\\|DONE\\|\\[X\\]\\).*}")
         (last-operator nil)
         (or-regex-str "+{\\(\\)}")
         (or-str ""))
    (dolist (elt tag-operator-list org-match-string)
      (if (or (string= elt "+") (string= elt "|") (string= elt "-"))
          (setq last-operator elt)
        (if (not (null last-operator))
            (if (or (string= last-operator "+") (string= last-operator "-"))
                (setq search-string (concat last-operator ":" elt ":"
                                            " " search-string))
              (if (string= or-str "")
                  (setq or-str (concat or-str ":" elt ":"))
                (setq or-str (concat or-str "\\|" ":" elt ":"))))
          (if (string= or-str "")
              (setq or-str (concat or-str ":" elt ":"))
            (setq or-str (concat or-str "\\|" ":" elt ":"))))))
    (setq or-regex-str (concat (substring or-regex-str 0 (- (length or-regex-str) (length "\\)}")))
                               or-str
                               (substring or-regex-str (- (length or-regex-str) (length "\\)}")))))
    (org-search-view nil (concat or-regex-str " " search-string) nil)))


;;Sunrise and Sunset
;;日出而作, 日落而息
(defun diary-sunrise ()
  (let ((dss (diary-sunrise-sunset)))
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ",")
      (buffer-substring (point-min) (match-beginning 0)))))

(defun diary-sunset ()
  (let ((dss (diary-sunrise-sunset))
        start end)
    (with-temp-buffer
      (insert dss)
      (goto-char (point-min))
      (while (re-search-forward " ([^)]*)" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward ", ")
      (setq start (match-end 0))
      (search-forward " at")
      (setq end (match-beginning 0))
      (goto-char start)
      (capitalize-word 1)
      (buffer-substring start end))))

(provide 'lib-org)
