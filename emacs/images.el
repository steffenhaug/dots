;; Minor mode to display inline images
;; `inline-image-mode` will render certain formatted
;; file paths as inline images.
(rx-define link-open  "[[")
(rx-define link-close "]]")
(rx-define img-file-ext
  (or "png" "jpg"))
(rx-define img-file-name
  (seq (group-n 1 (+ (any "/.+_-" alpha digit))
       "."
       (group-n 2 img-file-ext))))
(rx-define img-link
  (seq link-open img-file-name link-close))

;; Regex matching lines with image links.
(defconst my/img-regex
  (rx bol (group-n 3 img-link) ?\n))

(defvar my/img-min-width 400
  "Lower bound for inline image width.")

(defun my/img-width (buf)
  (let* (;; All windows showing the buffer in question
	 (wl (get-buffer-window-list buf nil t))
	 ;; List of pixel-wise window widths
	 (widths (mapcar (lambda (win) (window-body-width win t)) wl)))
    (apply #'max (cons my/img-min-width widths))))

(defface my/img-face
  '((t :extend t
       :foreground "red"
       :background "white"))
  "Face for links to images in `inline-image-mode`.")

(defun my/apply-display-img ()
  (let* ((beg (match-beginning 3))
         (end (match-end 3))
         (file (match-string 1))
         (ext (match-string 2))
         (img `(image :type ,(intern-soft ext)
                      :file ,file
                      :max-width ,(my/img-width (current-buffer))
                      :ascent center
                      :pointer arrow)))
    (put-text-property beg end 'display img)))

(define-minor-mode inline-image-mode
  "Toggle minor mode for showing pictures in Julia REPLs."
  :lighter nil
  (when-let ((repl (process-filter
		     (get-buffer-process
		      ;; Could use (current-buffer) to make it work in other terminals.
		      (julia-repl-inferior-buffer)))))
    (if inline-image-mode
	(progn
	  (font-lock-add-keywords
	   nil
	   `((,my/img-regex (0 (progn (my/apply-display-img) 'my/img-face) t))))
	  (message "Julia inline images enabled in %S!" (current-buffer))
	  (font-lock-flush))
      (progn
	;; Remove the fontification behind images
	(font-lock-remove-keywords
	 nil
	 `((,my/img-regex (0 ,my/fontification t))))
	;; Go throguh the buffer and remove the images
	(save-excursion
	  (let* ((inhibit-read-only t))
	    (goto-char (point-min))
	    (while (re-search-forward my/img-regex nil t)
	      (remove-text-properties (match-beginning 3) (match-end 3) '(display)))))
	(message "Julia inline image disabled in %S!" (current-buffer))
	(font-lock-flush)))))
