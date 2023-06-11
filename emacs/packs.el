;; Packages
;; Org-related packages live in `org.el', because
;; they have to be configured after the Org-related
;; directories and files are known.

;; Use MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Set up `use-package` to automatically install requested packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-always-ensure t)

(use-package no-littering
  :init
  (setq no-littering-var-directory "~/.local/share/emacs")
  :config
  ;; Other littering-related settings:
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
  (setq auto-save-default nil))

(use-package project
  :ensure t)

;; EVIL Keybindings
;; ----------------

;; Become EVIL
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :init
  :config
  (evil-collection-init))

(use-package hydra
  :config
  (setq hydra-is-helpful t)
  (setq hydra-hint-display-type 'lv)
  (setq lv-use-padding t)
  (add-hook 'lv-window-hook
            ;; EVIL mode makes the cursor visible
            (lambda () (evil-local-mode -1)))

  ;; Hack: lv calculates padding incorrectly for
  ;; hydras with multiple columns, so fix it.
  (defun lv--pad-to-center (str width)
    "Pad STR with spaces on the left to be centered to WIDTH."
    (let* ((strs (split-string str "\n"))
           (w (apply 'max (mapcar 'length strs)))
           (pad (max 0 (/ (- width w) 2)))
           (padding (make-string pad ?\ )))
      (mapconcat (lambda (s) (concat padding s)) strs "\n"))))

;; General.el to set up keybindings with a unified interface
;; The keybindings themselves live in a separate file.
(use-package general
  :after evil hydra
  :config
  (load (locate-user-emacs-file "keybind.el")))

;; VTERM
;; -----

(use-package vterm)

(use-package multi-vterm
  :after vterm evil)


(use-package corfu
  :config
  (setq corfu-cycle t)
  (setq corfu-auto t)
  (setq corfu-auto-prefix 2)
  (setq corfu-auto-delay 0.0)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ("RET" . nil))
  :init
  (global-corfu-mode))

;; Search with Ivy @ Co
;; --------------------
;;   (`ivy', `amx', `counsel', and `swiper')

(use-package ivy
  :config
  (setq enable-recursive-minibuffers nil)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "")
  (define-key ivy-minibuffer-map
    (kbd "C-<return>") #'ivy-immediate-done)
  (ivy-mode 1))

(use-package counsel)

(use-package swiper)

(use-package amx
  :config
  (amx-mode 1))


;; Julia programming language
;; --------------------------

(use-package julia-mode)

(use-package julia-repl
  :after no-littering julia-mode
  :hook (julia-mode . julia-repl-mode)
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

;; Haskell programming language
;; ----------------------------
(use-package haskell-mode
  :ensure t)

;; TeX
;; ---

(use-package pdf-tools
  :ensure t
  :config
  (pdf-loader-install))

(use-package tex
  :ensure auctex
  :after pdf-tools
  :config
  (load (locate-user-emacs-file "tex.el")))

;; Useless Stuff
;; -------------
;;   (For some definition of useless)

(use-package ligature
  :config
  ;; Enable selected ligatures:
  (let ((ligs '("->"   "-->"  "->>"
		">->"  "|->"  "<->" 
		"<-"   "<--"  "<<-"
		"<-<"  "<-|"  "<!--"
		"=>"   "==>"  "<=>"
		"<==>" ">=>"  "|=>"
		"=>>"  ":>"   ":<"
		":::"  "::"   ":="
		"::="  "<:"   "|>"
		"<|")))
	  (ligature-set-ligatures 'prog-mode ligs)
	  (ligature-set-ligatures 'org-mode ligs))
  ;; Enables ligature checks globally in all buffers.
  (global-ligature-mode t))

(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil))
