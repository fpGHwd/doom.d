;;; ../../Sync/dotfiles/doom.d/lisp/init-mode.el -*- lexical-binding: t; -*-
;;; hello-mode.el --- A major mode for Hello World files

;; major mode
(define-derived-mode hello-mode fundamental-mode "Hello"
  "Major mode for Hello World files."
  (setq font-lock-defaults '(hello-font-lock-keywords)))

;; font-lock-defaults 是如何使用的


(defvar hello-font-lock-keywords
  '(("\\<\\(Hello\\|World\\)\\>" . font-lock-keyword-face))
  "Highlight Hello and World keywords.")

;; binding hello-mode to .hello file
(add-to-list 'auto-mode-alist '("\\.hello\\'" . hello-mode))

(provide 'hello-mode)
;;; hello-mode.el ends here
