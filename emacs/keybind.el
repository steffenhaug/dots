;; These keybindings use the general.el package.
;; This file is loaded from its `:config'-section
;; in `pack.el'.
(require 'general)
(require 'hydra)
(general-auto-unbind-keys)

(general-unbind
  :modes '(org-mode)
  "C-,"
  "C-'")

(general-unbind
  "C-w"
  "C-k"
  "C-j"
  "C-x C-z"
  "ESC ESC ESC")

(general-create-definer my/leader
  :states '(emacs normal motion)
  :prefix        ","
  :global-prefix "C-,")

(my/leader
  "f"  'find-file
  "b"  'ivy-switch-buffer
  "p"  'counsel-git
  "rg" 'counsel-rg)

(my/leader
  "ol" 'org-roam-buffer-toggle
  "of" 'org-roam-node-find)

(my/leader
  :mode 'org-mode
  "oi" 'org-roam-node-insert)

(defhydra hydra-win (:columns 5)
  "Window Management"
  ;; Basic window navigation
  ("h" evil-window-left "Left")
  ;; Trick: Make a command that runs an alternative
  ;; command if one fails. The best part: The prefix
  ;; argument works in both branches!
  ("j" (ignore) "Down/Up"
   :exit (condition-case nil
             (evil-window-down current-prefix-arg)
           (error (evil-next-line current-prefix-arg))))
  ("k" (ignore) "Down/Up"
   :exit (condition-case nil
             (evil-window-up current-prefix-arg)
           (error (evil-previous-line current-prefix-arg))))
  ("l" evil-window-right "Right")
  ;; Splits
  ("s" evil-window-split "Horizontal Split")
  ("v" evil-window-vsplit "Vertical Split")
  ("q" evil-quit "Close")
  ;;
  ("=" (balance-windows (window-main-window)) "Balance")
  ;; Cycle trough windows:w
  ("w" evil-window-next "Next")
  ("W" evil-window-prev "Prev")
  ("t" window-toggle-side-windows "Toggle Side Windows")
  ("T" oaty/make-help-tall "Move Help to Side")
  ("ESC" nil "Quit"))

(hydra-set-property 'hydra-win :verbosity nil)

(general-def global-map
  :states '(normal motion)
  "C-w" 'hydra-win/body)

(general-define-key
 :modes 'pdf-view-mode
 "C-w" 'hydra-win/body)

(general-unbind
 :modes 'julia-mode
 "C-c C-c")

(general-def cell-mode-map
  :states 'visual
  "ac" 'my/select-cell-with-header
  "ic" 'my/select-cell)

(general-def cell-mode-map
  "C-c C-c" 'my/send-cell)

(my/leader
  :modes  'cell-mode
  :states 'normal
  "yc"    'my/yank-cell)

(general-def org-mode-map
  "M-h" 'org-shiftmetaleft
  "M-l" 'org-shiftmetaright
  "M-j" 'org-move-item-down
  "M-k" 'org-move-item-up)

