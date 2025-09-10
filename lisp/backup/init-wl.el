;; -*- no-byte-compile: t; -*-
;;; ../../Sync/dotfiles/doom.d/lisp/test/init-wl.el

;; (use-package! wl
;;   :defer t
;;   :custom
;;   (wl-smtp-connection-type 'starttls)
;;   (wl-smtp-posting-port 587)
;;   (wl-smtp-authenticate-type "plain")
;;   (wl-smtp-posting-user "Wang Ding")
;;   (wl-smtp-posting-server "smtp.gmail.com")
;;   (wl-local-domain "gmail.com")

;;   (wl-default-folder "%INBOX")
;;   (wl-default-spec "%")
;;   (wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
;;   (wl-trash-folder "%[Gmail]/Trash")

;;   (wl-summary-sort-specs '(number !date subject from list-info size))

;;   (wl-folders-file "~/.config/doom/wl/folders")
;;   (wl-summary-order 'descending)
;;   (wl-temporary-file-directory "~/.config/wanderlust/tmp")

;;   :config
;;   ;; wanderlust
;;   (autoload 'wl "wl" "Wanderlust" t)
;;   (autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
;;   (autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;;   ;; SMTP
;;   (autoload 'wl-user-agent-compose "wl-draft" nil t)
;;   (if (boundp 'mail-user-agent)
;;       (setq mail-user-agent 'wl-user-agent))
;;   (if (fboundp 'define-mail-user-agent)
;;       (define-mail-user-agent
;;         'wl-user-agent
;;         'wl-user-agent-compose
;;         'wl-draft-send
;;         'wl-draft-kill
;;         'mail-send-hook)))


;; (provide 'init-wl)