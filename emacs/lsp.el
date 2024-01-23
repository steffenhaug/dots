(require 'eglot)

;; Tree-sitter

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


;; Completion

(use-package corfu
  :ensure t
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0.0)
  (setq corfu-preview-current nil)
  :init
  (global-corfu-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

;; Eglot (LSP Client)

(use-package eldoc
  :ensure t
  :config
  (setq eldoc-idle-delay 0.0)
  (setq eldoc-echo-area-use-multiline-p 2)
  (setq eldoc-echo-area-display-truncation-message nil)
  (setq eldoc-echo-area-prefer-doc-buffer t)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly))


;; Julia

(use-package julia-ts-mode
  :ensure t)

(use-package eglot-jl
  :ensure t
  :after eglot
  :config
  (eglot-jl-init))

(use-package julia-repl
  :ensure t
  :after no-littering julia-mode
  :hook (julia-ts-mode . julia-repl-mode)
  :init
  ;; You can detect Julia running in emacs:
  ;; ENV["INSIDE_EMACS"] is set to indicate the backend.
  (setenv "JULIA_NUM_THREADS" "auto")
  (setenv "JULIA_PROJECT"     "@.")
  (let* ((img-prefix (no-littering-expand-var-file-name "jl_inline_image")))
    (setenv "JULIA_INLINE_FILE" img-prefix))
  :config
  (add-hook 'julia-repl-hook #'inline-image-mode)
  (julia-repl-set-terminal-backend 'vterm))


;; Haskell

(use-package haskell-mode
  :ensure t
  :config
  (setq haskell-process-auto-import-loaded-modules t
        haskell-process-log t
        haskell-process-suggest-remove-import-lines t))


;; OCaml

(use-package ocp-indent
  :ensure t)

(use-package merlin
  :ensure t)

(use-package utop
  :ensure t
  :config
  (setq utop-command "opam exec -- dune utop . -- -emacs"))

(use-package tuareg
  :ensure t
  :config
  (setq tuareg-interactive-mode 'utop)
  :hook (tuareg-mode-hook . utop-minor-mode)
  :hook (tuareg-mode-hook . merlin-mode)
  :hook (tuareg-mode-hook . ocp-setup-indent))

(use-package dune
  :ensure t)
