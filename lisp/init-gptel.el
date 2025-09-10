;;; ../../Sync/dotfiles/doom.d/lisp/init-gpt.el -*- lexical-binding: t; -*-

;; gptel
(use-package! gptel
  :defer t
  :custom
  (gptel-use-curl t)
  (gptel-default-mode 'org-mode)
  (gptel-log-level nil)
  (gptel-api-key (password-store-get "openai/chatgpt"))
  (gptel-crowdsourced-prompts-file (expand-file-name "etc/gptel/gptel-crowdsourced-prompts.csv" doom-user-dir))
  :config
  ;; Anthropic/claude
  ;; (gptel-make-anthropic "Claude" :stream t :key (password-store-get "anthropic.com/claude-api-key"))
  ;; Google/gemini
  ;; (gptel-make-gemini "Gemini" :key (password-store-get "google/gemini") :stream t)
  ;; Github Models offers an OpenAI compatible API
  ;; (gptel-make-openai "Github"
  ;;   :host "models.inference.ai.azure.com"
  ;;   :endpoint "/chat/completions?api-version=2024-05-01-preview"
  ;;   :stream t
  ;;   :key (password-store-get "github/gptel-backend-github")
  ;;   :models '(gpt-4o))
  ;; Github DeepSeek-R1
  ;; (gptel-make-openai "Github/DeepSeek-R1"
  ;;   :host "models.inference.ai.azure.com"
  ;;   :key (password-store-get "github/gptel-backend-github")
  ;;   :endpoint "/chat/completions"
  ;;   :stream t
  ;;   :models '(DeepSeek-R1))
  )

(use-package! aidermacs
  :init
  (setenv "OPENAI_API_KEY" (password-store-get "openai/chatgpt"))
  :bind (("C-c a" . aidermacs-transient-menu))
  :custom
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "gpt-3.5-turbo")
  (aidermacs-backend 'vterm))

(provide 'init-gptel)
