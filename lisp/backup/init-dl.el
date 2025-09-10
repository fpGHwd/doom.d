;;; ../../Sync/dotfiles/doom.d/lisp/init-dl.el -*- lexical-binding: t; -*-


(defun my-download-video (url)
  "Download video from a URL."
  (interactive
   (list (if (string-match "\\`https?://" (current-kill 0))
             (current-kill 0)
           (read-string "URL: "))))

  (if (not (string-match "\\`https?://" url))
      (error "Invalid URL: %s" url)
    (let* ((buffer (url-retrieve-synchronously url))
           (html (with-current-buffer buffer (buffer-string)))
           (m3u8-links (cl-remove-if-not
                        (lambda (s) (string-match "\\.m3u8" s))
                        (split-string html "https?://[^\"']+"))))
      (if (null m3u8-links)
          (message "No .m3u8 links found.")
        (let ((selected-link (completing-read "Select a link: " m3u8-links nil t)))
          (let* ((title (read-string "Enter video title (or blank to auto-detect): "))
                 (title (if (string= title "") (my-get-title html) title))
                 (filename (format-time-string "%Y-%m-%d-%s.mp4" (current-time) (replace-regexp-in-string "[^a-zA-Z0-9]+" "-" title))))
            (async-shell-command
             (format "youtube-dl '%s' -o '%s'" selected-link filename))))))))


(defun download-youtube-video (url filename)
  "Download a YouTube video using youtube-dl."
  (let* ((default-directory (concat (getenv "HOME") "/Downloads/"))
         (date-str (format-time-string "%Y-%m-%d"))
         (final-filename (concat date-str "-" filename ".mp4")))
    (shell-command
     (concat "yt-dlp --write-sub --embed-subs --sub-lang en_US,en-US,en -o '"
             final-filename "' '" url "'")
     "*YouTube Download Output*")
    (message "Video downloaded to %s" final-filename)))

(defun download-youtube-video-prompt ()
  "Prompt for YouTube video URL and filename, then download the video."
  (interactive)
  (let ((url (read-string "Enter YouTube video URL: "))
        (filename (read-string "Enter filename (without extension): ")))
    (download-youtube-video url filename)))


;; (defun my-get-title (html)
;;   "Extract title from HTML."
;;   (if (string-match "<h1 class=\"panel-title\">\\([^<]+\\)</h1>" html)
;;       (match-string 1 html)
;;     "Untitled"))
;; #+end_src

;; 这个函数可以通过 =M-x my-download-video= 来调用。它会首先检查剪贴板中是否有一个有效的 URL，如果没有就会提示输入。然后它会获取网页内容，匹配所有 =.m3u8= 链接，让用户选择一个。接着它会提示输入视频标题，或者自动匹配网页中的标题。最后它会使用 =youtube-dl= 命令来下载选中的链接，保存为指定的文件名。

;; 注意，这个实现依赖于 =url-retrieve-synchronously=、=async-shell-command= 和 =youtube-dl= 命令的可用性。另外，它使用了 =cl-remove-if-not= 函数，因此需要加载 =cl-lib=。
