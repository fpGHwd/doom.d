;;; ../../Sync/dotfiles/doom.d/lisp/init-misc.el -*- lexical-binding: t; -*-

(setq! +workspaces-data-file (concat (system-name) "_workspaces"))


(use-package! bing-dict
  :hook (doom-first-file-hook . (lambda () (require 'bing-dict)))
  :custom
  (bing-dict-vocabulary-save t)
  (bing-dict-vocabulary-file "~/.config/doom/etc/bing-dict/vocabulary.org")
  :config
  (map! :leader :desc "bing-dict-brief" "y" #'bing-dict-brief))


;; keyfreq
(use-package! keyfreq
  :hook (doom-first-buffer-hook . (lambda () (keyfreq-mode 1)))
  :custom
  (keyfreq-autosave-mode 1)
  (keyfreq-file (expand-file-name "emacs.keyfreq" doom-local-dir))
  (keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock" doom-local-dir))
  (keyfreq-excluded-commands '(self-insert-command
                               forward-char
                               backward-char
                               previous-line
                               next-line
                               evil-next-line
                               evil-previous-line
                               evil-forward-char
                               evil-backward-char
                               evil-next-visual-line
                               evil-previous-visual-line
                               evil-forward-WORD-begin
                               evil-backward-WORD-begin)))

;; leetcode
(use-package! leetcode
  :defer t
  :custom
  (leetcode-save-solutions t)
  (leetcode-directory (concat  (file-truename "~/Sync/leetcode/") (format-time-string "%Y")))
  (leetcode-prefer-language "python3"))


;; org-mobile
(use-package! org-mobile
  :defer t
  :custom
  (org-mobile-encryption-password (password-store-get "org/org-mobile"))
  (org-mobile-directory "/srv/http/dav/org")
  (org-mobile-files '("~/Sync/org/2025/todo.org")))


(use-package! auth-source
  :defer t
  :custom
  (auth-source-save-behavior 'ask)
  (auth-sources '("~/.config/doom/etc/authinfo.gpg")))


(use-package! ispell
  :defer t
  :custom
  (ispell-extra-args '("--sug-mode=ultra"))
  (ispell-dictionary "english")
  (ispell-personal-dictionary (concat doom-user-dir "etc/ispell-personal-dictionary"))

  :config
  (pushnew! ispell-skip-region-alist
            '("#\\+begin_src" . "#\\+end_src"))

  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))


(use-package! doom-ui
  :init
  (setenv "SSH_AUTH_SOCK" (pcase system-name
                            ("macbook-m1-pro" "/Users/wd/.gnupg/S.gpg-agent.ssh")
                            (_ "/run/user/1000/gnupg/S.gpg-agent.ssh")))
  (setenv "MPD_HOST" "arch-nuc.lan")
  (setenv "MPD_PORT" "6600")

  (setq initial-scratch-message ";; Happy hacking,  - Emacs loves Melt!\12\12")

  :custom
  ;; (doom-theme 'doom-one)
  (doom-theme 'doom-one-light)
  (initial-scratch-message (concat ";; Happy hacking, " user-full-name " - Emacs ♥ you!\n\n"))
  (fancy-splash-image (file-truename (concat doom-user-dir "assets/2025/bitmap_resized_2.png")))

  (imenu-auto-rescan t)

  (user-full-name "Wang Ding")
  (user-mail-address "ggwdwhu@gmail.com"))


(use-package! wakatime-mode
  :hook (doom-first-file-hook . global-wakatime-mode)
  :custom
  (wakatime-cli-path (executable-find "wakatime"))
  (wakatime-api-key "dd9ed811-5f7c-4e42-9224-8c93005c00bd")
  (wakatime-disable-on-error t))


;; gif-screencast
;; https://github.com/Ambrevar/emacs-gif-screencast
;; https://blog.3vyd.com/blog/posts-output/2019-12-15-emacs-gif/
(use-package! gif-screencast
  :defer t
  :custom
  (gif-screencast-convert-program (executable-find "magick"))
  (gif-screencast-convert-args '("convert" "-delay" "10" "-loop" "0"))
  (gif-screencast-args '("-x")) ;; To shut up the shutter sound of `screencapture' (see `gif-screencast-command').
  (gif-screencast-cropping-program "mogrify") ;; Optional: Used to crop the capture to the Emacs frame.
  (gif-screencast-capture-format "ppm")
  :config
  (with-eval-after-load 'gif-screencast
    (define-key gif-screencast-mode-map (kbd "<f8>") 'gif-screencast-toggle-pause)
    (define-key gif-screencast-mode-map (kbd "<f9>") 'gif-screencast-stop))

  (when (string= system-name "macbook-m1-pro")
    (advice-add ;; 适配mac自带的视网膜屏幕
     #'gif-screencast--cropping-region
     :around
     (lambda (oldfun &rest r)
       (apply #'format "%dx%d+%d+%d"
              (mapcar
               (lambda (x) (* 2 (string-to-number x)))
               (split-string (apply oldfun r) "[+x]")))))))


(use-package! vterm
  :defer t
  :custom
  (vterm-shell (let ((zsh-path (executable-find "zsh")))
                 (if zsh-path
                     zsh-path
                   (executable-find "bash")))))


(use-package! magit-clone
  :defer t
  :custom
  (magit-clone-default-directory (concat "~/github/" (format-time-string "%Y") "/")))


(use-package! recentf
  :hook (doom-first-file-hook . recentf-mode)
  :config
  (setq recentf-max-saved-items 2000))


(use-package! eldoc
  :defer t
  :custom
  (eldoc-idle-delay 2))


;; (toggle-debug-on-error)  ;; you can do this everywhere
(when (string= (system-name) "ubuntu2204")
  (after! doom
    (add-to-list '+lookup-provider-url-alist
                 '("Bing" "https://www.bing.com/search?q=%s"))))

(setup haskell-mode
  (:hook org-mode (lambda ()
                    (org-babel-do-load-languages 'org-babel-load-languages '((haskell . t))))))


(after! spell-fu
  (setq spell-fu-idle-delay 30))  ; default is 0.25

(after! flyspell
  (setq flyspell-lazy-idle-seconds 30))

;; (after! ein
;;   (let* ((urls '("https://jupyter.autove.dev"))
;;          (urls-new (when (not (string= "ubuntu2204" (system-name)))
;;                  (append '("http://arch-nuc.lan:8888") urls))))
;;     (setq ein:urls urls-new
;;           ein:jupyter-default-notebook-directory "/home/wd/Sync/projects/2025/hikyuu"
;;           ein:jupyter-server-use-subcommand "server")))

;; after! 本质就是 eval-after-load
(after! gdb-mi
  (setq! gdb-debuginfod-enable nil))


(provide 'init-misc)
