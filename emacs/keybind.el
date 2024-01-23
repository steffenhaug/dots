;; Key binding stuff.
(require 'org)
(require 'evil)
(require 'vterm)
(require 'ivy)
(require 'utop)
(require 'corfu)

;; Tab vs C-i
;; GUI emacs dinstinguishes between Tab ("<tab>") and C-i ("TAB"),
;; but if Tab is not bound, it is translated to C-i automatically.
;; Globally binding Tab stops this.
(keymap-global-set "<tab>" 'ignore)


;; Fix <tab> vs. C-i in places where it is wrong:
(keymap-set ivy-minibuffer-map "<tab>" 'ivy-partial-or-done)
(keymap-set utop-mode-map "<tab>" 'utop-complete)
(setq org-cycle-emulate-tab nil)

;; Completion
(keymap-unset corfu-map "RET")
(keymap-set corfu-map "<tab>" 'corfu-complete)

(keymap-unset evil-insert-state-map "C-p")
(keymap-set corfu-map "C-p" 'corfu-previous)

;; Because emulation maps take precedence, Evil keys take priority
;; even when the corfu map is active, so to have C-n pop up completions
;; like in vim, and also go to the next completion, we can remap
;; completion-at-point itself in corfu mode...
(keymap-set evil-insert-state-map "C-n" 'completion-at-point)
(keymap-set corfu-map "<remap> <completion-at-point>" 'corfu-next)

;; Make ESC also stop completion.
(keymap-set evil-insert-state-map "<escape>"
            (lambda ()
              (interactive)
              (corfu-quit)
              (evil-normal-state)))


;; Keys that are bad to press out of VIM habit:
(keymap-global-unset "C-w")
(keymap-global-unset "C-j")
(keymap-global-unset "C-i")
(keymap-global-unset "C-k")
(keymap-global-unset "C-l")
(keymap-global-unset "C-o")


;; Disable conflicting keybindings for Evil motions.
(keymap-unset vterm-mode-map "C-w")
(keymap-unset vterm-mode-map "C-o")
(keymap-unset vterm-mode-map "C-i")
(keymap-unset help-mode-map "C-i")
(keymap-unset dired-mode-map "C-o")
(keymap-unset org-mode-map "C-i")
(keymap-unset utop-mode-map "C-i")
(keymap-unset button-map "C-i")
(keymap-unset splash-screen-keymap "C-i")
(keymap-unset backtrace-mode-map "C-i")


;; Evil window motions globally:
(keymap-global-set "C-w" 'evil-window-map)


;; Evils implementation of the jump stack doesn't really work
;; because a ton of jumps from other packages don't register.
;; Better to just use some inferior built-in Emacs functionality.
(keymap-unset evil-motion-state-map "C-o")
(keymap-unset evil-motion-state-map "C-i")
(keymap-global-set "C-o" 'previous-buffer)
(keymap-global-set "C-i" 'next-buffer)

;; VIM "leader key":
(define-prefix-command 'vi-leader)
(keymap-set evil-motion-state-map "," 'vi-leader)

(keymap-set 'vi-leader "f"   'find-file)
(keymap-set 'vi-leader "b"   'ivy-switch-buffer)
(keymap-set 'vi-leader "p"   'counsel-git)
(keymap-set 'vi-leader "r g" 'counsel-rg)
