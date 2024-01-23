(require 'org)

(use-package ess
  :ensure t)

;; Org file organization
(setq org-directory (expand-file-name "Org" "~")
      org-agenda-files `("thesis.org"
                         "notes.org")
      org-default-notes-file (expand-file-name "notes.org" org-directory))

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale 1.5))

;; Org Mode Packages
;; -----------------
(use-package org-roam
  :init
  (setq org-roam-directory (expand-file-name "Roam" org-directory))
  (setq org-roam-completion-everywhere t)
  :config
  (org-roam-setup))

;; Org minor-modes
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Org Agenda layout
(setq org-agenda-window-setup 'current-window)
(setq org-agenda-tags-column 60
      org-agenda-start-with-log-mode t
      org-agenda-span 3 ; (3 days)
      org-agenda-time-grid '((daily today require-timed)
                             (600 800 1000 1200 1400 1600)
                             "" "")
      org-agenda-current-time-string "Now")


;; Code editing in Org cells
(setq org-src-fontify-natively  t
      org-src-tab-acts-natively t)

;; Log the time stamp when a task was completed.
(setq org-log-done 'time
      org-log-into-drawer t)


;; Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((julia . t)))

(setq org-confirm-babel-evaluate nil)
