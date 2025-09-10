;;; ../../Sync/dotfiles/doom.d/lisp/init-mail.el -*- lexical-binding: t; -*-

(use-package mu4e
  :defer t
  :load-path
  ("/opt/homebrew/opt/mu/share/emacs/site-lisp/mu/mu4e/"
   "/usr/share/emacs/site-lisp/mu4e"
   "/home/wd/.local/share/emacs/site-lisp/mu4e")
  :init
  (defvar +wd/mu4e-index-timer nil "Timer for auto-updating mu4e index.")
  ;; (setq mu4e-maildir "/home/data/mail")
  :hook
  (mu4e-main-mode . (lambda ()
                           (unless +wd/mu4e-index-timer
                             (setq +wd/mu4e-index-timer
                                   (run-at-time nil (* 5 60) #'mu4e-update-index)))))
  :config                               ; doom use config
  (setq
   mu4e-mu-binary (pcase (system-name)
                    ("macbook-m1-pro" "/opt/homebrew/bin/mu")
                    ("ubuntu2204" "/home/wd/.local/bin/mu")
                    (_ "/usr/bin/mu"))
   sendmail-program (executable-find "msmtp")
   send-mail-function #'smtpmail-send-it
   message-sendmail-f-is-evil t
   message-sendmail-extra-arguments '("--read-envelope-from")
   message-send-mail-function #'message-send-mail-with-sendmail
   mu4e-get-mail-command "true"))

(provide 'init-mail)
