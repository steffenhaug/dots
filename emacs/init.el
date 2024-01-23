;; Make all the auto-generated junk go to its own file.
(setq custom-file (locate-user-emacs-file "customize.el"))
(load custom-file)

;; Use MELPA
(require 'use-package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Anti-littering

(progn
  (use-package no-littering
    :init
    (setq no-littering-var-directory "~/.local/share/emacs"))
  (setq auto-save-default nil)
  (setq make-backup-files nil)
  (setq create-lockfiles nil))

;; GUI stuff

(load-theme 'phoebe t)

(set-face-attribute
 'default nil
 :family "JetBrains Mono Slashed"
 :height 130)

(progn
  (winner-mode 1)             ; Ability to undo window layout changes
  (scroll-bar-mode -1)        ; Scrollbar
  (tool-bar-mode -1)          ; Toolbar
  (tooltip-mode -1)           ; Tooltips (hovering with mouse)
  (set-fringe-mode 15)        ; Padding on the sides of the text
  (menu-bar-mode -1)          ; Disable the menu bar (File, Edit, ...)
  (setq frame-resize-pixelwise t)
  (pixel-scroll-mode 1)
  (pixel-scroll-precision-mode 1)
  (save-place-mode 1)         ; Save position in files
  (setq-default truncate-lines 1))


;; Show line-numbers in selected modes.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'org-mode-hook 'display-line-numbers-mode)

(use-package rainbow-mode
  :config
  (setq rainbow-x-colors nil))

;; Never tabs
(setq-default indent-tabs-mode nil)

;; No bell
(setq visible-bell nil)

;; Auto-revert buffers.
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

;; Show the keystrokes in the echo area immediately.
(setq echo-keystrokes 0.01)

;; Evil

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'dired-mode   'emacs)
  (evil-set-initial-state 'vterm-mode   'emacs)
  (evil-set-initial-state 'special-mode 'emacs)
  (evil-set-initial-state 'utop-mode    'emacs))

;; Terminal

(use-package vterm
  :ensure t)

(use-package multi-vterm
  :ensure t
  :after  vterm)

(use-package ivy
  :ensure t
  :config
  (setq enable-recursive-minibuffers nil)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "")
  (ivy-mode 1))

(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t)

(use-package amx
  :ensure t
  :config
  (amx-mode 1))

(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(load (locate-user-emacs-file "lsp.el"))
(load (locate-user-emacs-file "modeline.el"))
(load (locate-user-emacs-file "cells.el"))
(load (locate-user-emacs-file "images.el"))
(load (locate-user-emacs-file "org.el"))
(load (locate-user-emacs-file "wm.el"))
(load (locate-user-emacs-file "fixes.el"))
(load (locate-user-emacs-file "keybind.el"))
