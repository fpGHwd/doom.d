;;; ../../Sync/dotfiles/doom.d/lisp/init-ui.el -*- lexical-binding: t; -*-

; set fonts
(add-hook! 'after-setting-font-hook
  (lambda ()
    (when window-system
      (setup faces
        (:also-load lib-face)
        (configure-ligatures)
        (:hooks window-setup-hook +setup-fonts
                server-after-make-frame-hook +setup-fonts
                default-text-scale-mode-hook +setup-fonts)
        (when doom--system-macos-p
          (:with-mode (vterm-mode eshell-mode) (:set-font *term-default-font*))
          (:with-mode (latex-mode prog-mode nxml-mode magit-status-mode magit-diff-mode diff-mode) (:set-font *prog-font*))
          (:with-mode nov-mode (:set-font (replace-regexp-in-string "14" "16" *default-font*)))
          (:with-mode dired-mode (:set-font *org-font*))
          (:with-mode (org-mode ebib-index-mode ebib-entry-mode) (:set-font *org-font*)))
        (:advice face-at-point :around #'+suggest-other-faces)))))


;; override doom font setting
(setq doom-font (font-spec :family "Fira Code" :weight 'regular :size (if (string= (system-name) "ubuntu2204") 16 15)))
(setq doom-variable-pitch-font (font-spec :family "Sarasa Gothic SC" :weight 'regular))
(when (not IS-MAC)
  (setq doom-serif-font (font-spec :family "Noto Serif CJK SC" :weight 'regular)))


(add-hook!
 'after-setting-font-hook
 #'(lambda ()
     ;; 如果不把这玩意设置为 nil, 会默认去用 fontset-default 来展示, 配置无效
     (setq use-default-font-for-symbols nil)
     (dolist (charset '(kana han cjk-misc bopomofo))
       (set-fontset-font t charset (font-spec :family "Sarasa Gothic SC")))))


(add-hook! 'doom-load-theme-hook
 (set-face-attribute 'font-lock-comment-face t :slant 'italic)
 (set-face-attribute 'font-lock-keyword-face t :slant 'italic))


(require 'lib-misc)
(add-to-list 'default-frame-alist '(fullscreen . fullboth))

;; (setup vertico
;;   (:hooks window-size-change-functions (lambda () (setq! vertico-posframe-width (floor (* *golden-ratio* (frame-width)))
;;                                                          vertico-posframe-height (floor (* *golden-ratio* (frame-height)))))))
(after! vertico
  (setq! vertico-posframe-width 130
         vertico-posframe-height 25
         vertico-count 25))

;; auto save saved workspaces
(add-hook! 'doom-after-init-hook #'(lambda () (run-with-idle-timer 600 nil #'+wd/update-current-workspaces-to-saved-ones)))

(provide 'init-ui)
