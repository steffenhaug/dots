;; Modeline Customizations

;; Active Window Tracking
(defun oaty/is-active ()
  (window-parameter (selected-window) 'oaty/active))

(defun oaty/walk-track-active ()
  (let* ((track (lambda (w)
                  (set-window-parameter w
                   'oaty/active
                   ;; If the minibuffer is active, the "active window'
                   ;; is actually the one the minbuffer will fall back to.
                   (eq w (if (minibufferp)
                             (minibuffer-selected-window)
                           (selected-window)))))))
    (walk-windows track nil t)))

(add-hook 'window-state-change-hook
          #'oaty/walk-track-active)

(defmacro oaty/fallback (face)
  `(if (oaty/is-active)
       ,face
     (intern (format "%s-inactive" ,face))))

;; Custom faces

(defface oaty/modeline-vi-state
  '((t :inherit mode-line-active))
  "Face for the modeline EVIL state.")

(defface oaty/modeline-vi-state-inactive
  '((t :inherit oaty/modeline-vi-state))
  "Face for the modeline EVIL state in inactive windows.")

(dolist (st '(normal insert visual replace motion emacs))
  (let* ((vi-state-inactive (intern (format "oaty/modeline-vi-%s-inactive" st)))
         (vi-state          (intern (format "oaty/modeline-vi-%s" st))))
    (custom-declare-face
     vi-state '((t :inherit oaty/modeline-vi-state))
     (format "Face for EVIL motion state %s on the modeline." st))
    (custom-declare-face
     vi-state-inactive '((t :inherit oaty/modeline-vi-state-inactive))
     (format "Face for EVIL motion state %s on the modeline in inactive windows." st))))

(defface oaty/modeline-vi-cursor
  '((t :inherit mode-line-active))
  "Face for a VIM-like cursor position on the modeline.")

(defface oaty/modeline-vi-cursor-inactive
  '((t :inherit oaty/mode-line-vi-cursor))
  "Face for a VIM-like cursor position on the modeline in inactive windows.")

(defface oaty/modeline-buffer-id
  '((t :inherit mode-line-active))
  "Face for a mode-line construct with the buffer name.")

(defface oaty/modeline-buffer-id-inactive
  '((t :inherit oaty/modeline-buffer-id))
  "Face for a mode-line construct with the buffer name in inactive windows.")

(defface oaty/modeline-buffer-id-roam
  '((t :inherit oaty/modeline-buffer-id))
  "Face for the mode-line buffer identification when the buffer is a Roam-node.")

(defface oaty/modeline-buffer-id-roam-inactive
  '((t :inherit oaty/modeline-buffer-id-inactive))
  "Face for the mode-line buffer identification when the buffer is a Roam-node and the window is inactive.")

(defface oaty/modeline-major-mode
  '((t :inherit mode-line-active))
  "Face for a mode-line construct with the buffers major mode.")

(defface oaty/modeline-major-mode-inactive
  '((t :inherit oaty/modeline-major-mode))
  "Face for a mode-line construct with the buffers major mode in inactive windows.")


;; Mode-line constructs

(defun oaty/pp-evil-state ()
  "Print the current EVIL-state in a manner suitable for the mode-line."
  (if evil-mode
      (let* ((name (symbol-name evil-state))
             (abbr (substring name 0 1)))
        (format " %s " (capitalize abbr)))))

(defconst oaty/vi-state
  '(:eval
    (let* ((name (format "oaty/modeline-vi-%s" evil-state))
           (face (oaty/fallback (intern-soft name))))
      (propertize (oaty/pp-evil-state) 'face face)))
  "Mode line construct for displaying the EVIL state.

This uses a special face for each mode, enabling a visual
indication of what the current mode is, based on the color.")

(defconst oaty/vi-cursor
  '(:eval
    (propertize " %l:%c " 'face (oaty/fallback 'oaty/modeline-vi-cursor)))
  "Mode line construct for displaying the position of the point.")

(defconst oaty/buffer-id
  '(:eval
    (if (org-roam-buffer-p)
        (propertize (format " %s " (org-roam-node-title
                                          (org-roam-node-at-point)))
                    'face (oaty/fallback 'oaty/modeline-buffer-id-roam))
      (propertize " %b " 'face (oaty/fallback 'oaty/modeline-buffer-id))))
  "Mode line construct for identifying the buffer.")

(defconst oaty/major-mode
  '(:eval
    (propertize (format " %s " (format-mode-line mode-name))
                'face (oaty/fallback 'oaty/modeline-major-mode)))
  "Mode line construct showing the buffers major mode.")

;; Set up the modeline

(defconst oaty/mode-line-format
  '((evil-mode oaty/vi-state)
    "%e"
    " "
    mode-line-front-space
    mode-line-mule-info
    mode-line-client
    mode-line-modified
    mode-line-remote
    mode-line-frame-identification
    oaty/buffer-id
    ;; (vc-mode vc-mode) gotta make something better
    oaty/vi-cursor
    oaty/major-mode
    mode-line-misc-info
    mode-line-end-spaces))

;; (setq mode-line-format oaty/mode-line-format)
(setq-default mode-line-format oaty/mode-line-format)
