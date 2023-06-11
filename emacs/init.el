;; Make all the auto-generated junk go to its own file.
(setq custom-file (locate-user-emacs-file "customize.el"))
(load custom-file)

;; Load other config files with grouped settings.
(load-theme 'phoebe t)
(window-divider-mode 1)
(load (locate-user-emacs-file "packs.el"))
(load (locate-user-emacs-file "modeline.el"))
(load (locate-user-emacs-file "cells.el"))
(load (locate-user-emacs-file "images.el"))
(load (locate-user-emacs-file "org.el"))
(load (locate-user-emacs-file "wm.el"))

(setq-default indent-tabs-mode nil)

(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

(setq echo-keystrokes 0.01)

;; Font stuff
(set-frame-font "JetBrains Mono Slashed" t t)

(defmacro oaty/font-height (h)
  "Make a function that sets the buffers default face height."
  `(lambda ()
     (setq-local buffer-face-mode-face `(:height ,,h))
     (buffer-face-mode 1)))

(add-hook 'help-mode-hook     (oaty/font-height 70))


(setq frame-resize-pixelwise t)
(scroll-bar-mode -1)        ; Scrollbar
(tool-bar-mode -1)          ; Toolbar
(tooltip-mode -1)           ; Tooltips (hovering with mouse)
(set-fringe-mode 15)        ; Padding on the sides of the text
(menu-bar-mode -1)          ; Disable the menu bar (File, Edit, ...)
(line-number-mode -1)       ; Line number in modeline
(save-place-mode 1)

(setq-default fill-column 60)


;; Show line-numbers in selected modes.
(dolist (mode '(prog-mode-hook))
  (add-hook mode
            (lambda () (display-line-numbers-mode 1))))

;; Don't wrap lines in selected modes.
(dolist (mode '(prog-mode-hook))
  (add-hook mode
            (lambda () (setq truncate-lines t))))

