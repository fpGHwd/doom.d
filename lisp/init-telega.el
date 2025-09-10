;;; ../Sync/dotfiles/doom.d/lisp/init-telega.el -*- lexical-binding: t; -*-

;; https://github.com/zevlg/telega.el
(setup telega
  (:only-if (string= (system-name) "arch-nuc"))
  (:defer (telega t))
  (:when-loaded
    (:also-load lib-telega)
    (:option
     telega-cache-dir (file-truename "~/.config/telega/cache")
     telega-directory (file-truename "~/.config/telega/")
     telega-server-logfile (file-truename "~/.config/telega/telega-server.log")
     telega-temp-dir (file-truename "~/.config/telega/temp")
     telega-server-libs-prefix "~/.local")
    (:hooks telega-chat-mode-hook (lambda () (company-mode -1)))

    (when (string= system-name "arch-nuc")
      (add-hook 'telega-chat-update-hook #'+wd/telega-chat-update-function))
    ;; telega font
    (when (member "Sarasa Mono SC" (font-family-list))
      (make-face 'telega-align-by-sarasa)
      (set-face-font 'telega-align-by-sarasa (font-spec :family "Sarasa Mono SC"))
      (add-hook! '(telega-chat-mode-hook telega-root-mode-hook)
        (buffer-face-set 'telega-align-by-sarasa)))))

(provide 'init-telega)
