;; Minor mode for adding code cells to a buffer
;; via formatted comments.
(require 'evil)

(defface oaty/cell-header-face
  '((t :inherit font-lock-comment-face
       :extend t
       :overline t))
  "Face for headers of code cells.")

(defface oaty/cell-header-title-face
  '((t :inherit (bold
                 oaty/cell-header-face)))
  "Face for the title in header of code cells.")

(defface oaty/active-cell-face
  '((t :extend t
       :foreground unspecified))
  "Face for the body of the active cell.")

(defface oaty/active-cell-header-face
  '((t :extend t
       :inherit oaty/cell-header-face))
  "Face for the header of the active cell.")

(defface oaty/active-cell-header-title-face
  '((t :inherit (oaty/cell-header-title-face
                 oaty/active-cell-header-face)))
  "Face for the header of the active cell.")

(rx-define prefix (seq (syntax comment-start) (or " %%" " |>") (* blank)))
(rx-define header
  ;; Match a full cell header and capture the title.
  (seq bol prefix (group-n 1 (* nonl)) ?\n))

(defconst oaty/cell-header-regex
  (rx header))

(defconst oaty/cell-beginning-regex
  (rx (or header (group-n 1 buffer-start))))

(defconst oaty/cell-end-regex
  (rx (or header (group-n 1 buffer-end))))

(defun oaty/search-cell-lims ()
  (save-excursion
    ;; Move +1 so search is exclusive of current point.
    (goto-char (+ 1 (point)))
    ;; Find the next lim forward.
    (re-search-forward oaty/cell-end-regex nil 1)
    (setq-local oaty/active-cell-end (- (match-beginning 0) 1))
    (if (= (point) (point-max))
	(goto-char (- (match-beginning 0) 1))
      (goto-char (match-beginning 0)))
    ;; Find the next lim back.
    (re-search-backward oaty/cell-beginning-regex nil 1)
    ;; Set the start of the cell, and the lims of the header title.
    (setq-local oaty/active-cell-start (match-end 0))
    (setq-local oaty/active-cell-header-start (match-beginning 0))
    (setq-local oaty/active-cell-header-title-start (match-beginning 1))
    (setq-local oaty/active-cell-header-title-end (match-end 1))))

(defun oaty/move-active-cell-overlays ()
  (if cell-mode
      (progn (oaty/search-cell-lims)
	     (move-overlay oaty/active-cell-overlay
			   oaty/active-cell-start
			   (+ 1 oaty/active-cell-end))
	     (move-overlay oaty/active-cell-header-overlay
			   oaty/active-cell-header-start
			   oaty/active-cell-start)
	     (move-overlay oaty/active-cell-header-title-overlay
			   oaty/active-cell-header-title-start
			   oaty/active-cell-header-title-end)
	     (font-lock-flush))))

(defun oaty/yank-cell ()
  (interactive)
  (if cell-mode
      (evil-yank-line oaty/active-cell-start
		      oaty/active-cell-end
		      'line)))

(defun oaty/goto-cell-beginning ()
  (interactive)
  (if cell-mode
      (evil-goto-char oaty/active-cell-start)))

(defun oaty/goto-cell-end ()
  (interactive)
  (if cell-mode
      (evil-goto-char oaty/active-cell-end)))

(defun oaty/select-cell ()
  (interactive)
  (if cell-mode
      (progn
	(evil-visual-line)
	(oaty/goto-cell-end)
	(push-mark oaty/active-cell-start nil t))))

(defun oaty/select-cell-with-header ()
  (interactive)
  (if cell-mode
      (progn
	(evil-visual-line)
	(oaty/goto-cell-end)
	(push-mark oaty/active-cell-header-start nil t))))

(defun oaty/send-cell ()
  (interactive)
  (if cell-mode
      (if (derived-mode-p 'julia-mode)
	  (save-excursion
	    (oaty/select-cell)
	    (julia-repl-send-region-or-line)))))

(defconst oaty/font-lock-keywords
  `((,oaty/cell-header-regex (0 'oaty/cell-header-face t))
    (,oaty/cell-header-regex (1 'oaty/cell-header-title-face t))))

(define-minor-mode cell-mode
  "Toggle minor mode for code cells."
  :lighter nil
  (if cell-mode
      (progn
	;; Apply face to cell headers
	(font-lock-add-keywords
	 nil oaty/font-lock-keywords)
	;; Set up cell overlay
	(oaty/search-cell-lims)
	(setq-local oaty/active-cell-overlay
		    (make-overlay oaty/active-cell-start
				  oaty/active-cell-end))
	(setq-local oaty/active-cell-header-overlay
		    (make-overlay oaty/active-cell-header-start
				  oaty/active-cell-start))
	(setq-local oaty/active-cell-header-title-overlay
		    (make-overlay oaty/active-cell-header-title-start
				  oaty/active-cell-header-title-end))
	(overlay-put oaty/active-cell-overlay
		     'face 'oaty/active-cell-face)
	(overlay-put oaty/active-cell-header-overlay
		     'face 'oaty/active-cell-header-face)
	(overlay-put oaty/active-cell-header-title-overlay
		     'face 'oaty/active-cell-header-title-face)
	(add-hook 'post-command-hook #'oaty/move-active-cell-overlays)
	(font-lock-flush))
    (progn
      ;; Remove face from cell headers
      (font-lock-remove-keywords
       nil oaty/font-lock-keywords)
      ;; Tear down cell overlay
      (delete-overlay oaty/active-cell-overlay)
      (delete-overlay oaty/active-cell-header-overlay)
      (delete-overlay oaty/active-cell-header-title-overlay)
      (remove-hook 'post-command-hook #'oaty/move-active-cell-overlay)
      (font-lock-flush))))
