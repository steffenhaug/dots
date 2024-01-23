;; Emacs window management
;;   (Rules for side-windows, etc.)

;; (setq fit-window-to-buffer-horizontally t)
;; (window-toggle-side-windows)
(setq window-sides-vertical t)

(defun oaty/make-help-tall ()
  "Moves the `*Help*' buffer to the side, so it is tall."
  (interactive)
  (let* ((buf (get-buffer "*Help*")))
    (dolist (w (get-buffer-window-list buf))
      (quit-window nil w))
    (display-buffer-in-side-window
     buf
     '((side . right)
       (slot . 0)
       (window-width . 0.3)))))

(defun oaty/pdf-side-win ()
  (interactive)
  (let* ((pdfs (match-buffers ".+\\.pdf"))
         (buf  (car pdfs)))
    (dolist (w (get-buffer-window-list buf))
      (quit-window nil w))
    (display-buffer-in-side-window
     buf
     '((side . right)
       (slot . 0)
       (window-width . 0.3)))))

(defun oaty/julia-side-win ()
  (interactive)
  (let* ((repl (match-buffers "\\*julia\\*"))
         (buf  (car repl)))
    (dolist (w (get-buffer-window-list buf))
      (quit-window nil w))
    (display-buffer-in-side-window
     buf
     '((side . right)
       (slot . 1)
       (window-width . 0.3)))))

(defun oaty/send-to-side ()
  (interactive)
  (display-buffer-in-side-window
   (current-buffer)
   '((side . right)
     (slot . 1))))
                                 
  

(setq
 display-buffer-alist
 `(("\\*\\(Backtrace\\|Warnings\\)\\*"
    display-buffer-in-side-window
    (side . right)
    (slot . 1)
   ("\\*compilation\\*"
    display-buffer-in-side-window
    (side . right)
    (slot . 2)
   ("\\*[eE]ldoc.*\\*"
    display-buffer-in-side-window
    (side . bottom)
    (slot . 1)
   )))))
