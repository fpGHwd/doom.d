;;; ../../Sync/dotfiles/doom.d/lisp/backup/init-misc.el -*- lexical-binding: t; -*-


;;;###autoload
;; automatically delete redundant workspaces on arch-nuc
(defun +wd/iter-and-delete-workspaces ()
  "Delete useless workspace."
  (interactive)
  (+workspace:switch-next)
  (let ((init-workspace (+workspace-current-name)))
    (+workspace:switch-next)
    (while (not (string= (+workspace-current-name) init-workspace))
      (when
          (string-match-p "#[[:digit:]]" (+workspace-current-name))
        (+workspace/delete (+workspace-current-name)))
      (+workspace:switch-next))))


;;;###autoload
;; advice for +default/org-notes-search
(defun +wd-advice/notes-search ()
  "Advice for notes-search and replace its key binding."
  (progn
    (message "This is function for notes-search...")))
;; find place where notes in my files


;;;###autoload
(defun cmp-date-property (prop)
  "Compare two `org-mode' agenda entries, `A' and `B', by some date property.
     If a is before b, return -1. If a is after b, return 1. If they
     are equal return t."
  (eval-when-compile (require 'cl)) ;; fixing not lexical-let
  (lexical-let ((prop prop))
    #'(lambda (a b)
        (let* ((a-pos (get-text-property 0 'org-marker a))
               (b-pos (get-text-property 0 'org-marker b))
               (a-date (or (org-entry-get a-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (b-date (or (org-entry-get b-pos prop)
                           (format "<%s>" (org-read-date t nil "now"))))
               (cmp (compare-strings a-date nil nil b-date nil nil))
               )
          (if (eq cmp t) nil (signum cmp))))))


;;;###autoload
(defmacro +wd/save-org-tag-view-macro (tag path name &optional args)
  "Save org-tag-view with TAG in PATH, with optional arguments ARGS."
  `(save-excursion
     (org-batch-agenda ,tag)
     (make-directory ,path 'parents)
     (write-file (concat ,path "/" ,name))))


;; recentf
(use-package! recentf
  :defer t
  :custom
  (recentf-max-menu-items 20)
  (recentf-max-saved-items 2000))


;; javascript
(use-package! indium
  :defer t
  :config
  (add-hook 'js-mode-hook #'indium-interaction-mode)
  (add-hook 'js-mode-hook 'js2-minor-mode))
