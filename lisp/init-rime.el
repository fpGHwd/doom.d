;;; ../Sync/dotfiles/doom.d/lisp/init-rime.el -*- lexical-binding: t; -*-

(setq default-input-method "rime")

(setup rime
  (:global "M-\\" rime-force-enable)
  (:when-loaded
    (:also-load lib-rime)
    (:option
     rime-posframe-properties (list :background-color "#666699"
                                    :foreground-color "#dcdccc"
                                    :font (format "Sarasa Gothic SC-%d" (1+ (font-get doom-font :size))))
     rime-show-candidate 'minibuffer
     ;; (rime-show-candidate 'posframe)
     rime-disable-predicates '(rime-predicate-auto-english-p
                               ;; rime-predicate-space-after-cc-p
                               rime-predicate-current-uppercase-letter-p
                               +pyim-probe-telega-msg)
     rime-inline-ascii-trigger 'shift-l
     rime-emacs-module-header-root (pcase (system-name)
                                     ("macbook-m1-pro" "/opt/homebrew/include")
                                     ("ubuntu2204" nil)
                                     ("arch-nuc" "/usr/include"))
     rime-user-data-dir "~/.config/rime"
     rime-librime-root (pcase system-name
                         ("macbook-m1-pro" (file-truename "~/github/2025/rime-1.14.0-osx/dist"))))
    (:after evil-mode
      (:global "M-\\" rime-force-enable))))

(provide 'init-rime)
