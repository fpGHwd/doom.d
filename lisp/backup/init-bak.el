;; -*- no-byte-compile: t; -*-
;;; ../../Sync/dotfiles/doom.d/lisp/test/init-bak.el


;;;###autoload
(defun +wd/ivy-modem-compile ()
  (interactive)
  (ivy-read "Choose: "
            (list "ES33 5G Modem" "S11L 4G Modem" "S11L 5G Modem")
            :action (lambda (compile-type)
                      (cond ((string= compile-type "ES33 5G Modem") (modem-compile-macro "ES33 5G Modem"))
                            ((string= compile-type "S11L 4G Modem") (modem-compile-macro "S11L 4G Modem"))
                            ((string= compile-type "S11L 5G Modem") (modem-compile-macro "S11L 5G Modem"))))
            :update-fn (lambda () (message "updating..."))))

;;;###autoload
;; notify-tools
(defun +wd/notify-send (info-str)
  (interactive "sEnter message: ")
  (shell-command (concat (executable-find  "notify-send") " \"" info-str "\"")))