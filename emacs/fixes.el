;; This file contains redefinitions of functions that fixes bugs.

(require 'eglot)

(defface oaty/eglot-function-name-face
  `((t :inherit font-lock-function-name-face))
  "Face for LSP function names.")

(defface oaty/eglot-function-argument-face
  `((t :inherit eldoc-highlight-function-argument))
  "Face for the hovered over LSP function argument.")

;; This regex fixes the function signature highlighting for ocaml.
;; If oyu are working in another language, update the variable
;; with a hook.
;; Ideally you should be able to define a regex per-language,
;; but i haven't got around to that yet.
(defvar oaty/eglot-sig-info-rx "\\([^:]*\\):\\(.+\\)"
  "Regex to parse LSP function signature.")


;; Redefinition of the function signature information fontification that uses
;; the customizable variables.
(defun eglot--sig-info (sigs active-sig sig-help-active-param)
  (cl-loop
   for (sig . moresigs) on (append sigs nil) for i from 0
   concat
   (eglot--dbind ((SignatureInformation) label documentation parameters activeParameter) sig
     (with-temp-buffer
       (save-excursion (insert label))
       (let ((active-param (or activeParameter sig-help-active-param))
             params-start params-end)
         ;; Ad-hoc attempt to parse label using a regex
         (when (looking-at oaty/eglot-sig-info-rx)
           (setq params-start (match-beginning 2) params-end (match-end 2))
           (add-face-text-property (match-beginning 1) (match-end 1)
                                   'oaty/eglot-function-name-face))
         (when (eql i active-sig)
           ;; Decide whether to add one-line-summary to signature line
           (when (and (stringp documentation)
                      (string-match "[[:space:]]*\\([^.\r\n]+[.]?\\)"
                                    documentation))
             (setq documentation (match-string 1 documentation))
             (unless (string-prefix-p (string-trim documentation) label)
               (goto-char (point-max))
               (insert ": " (eglot--format-markup documentation))))
           ;; Decide what to do with the active parameter...
           (when (and (eql i active-sig) active-param
                      (< -1 active-param (length parameters)))
             (eglot--dbind ((ParameterInformation) label documentation)
                 (aref parameters active-param)
               ;; ...perhaps highlight it in the formals list
               (when params-start
                 (goto-char params-start)
                 (pcase-let
                     ((`(,beg ,end)
                       (if (stringp label)
                           (let ((case-fold-search nil))
                             (and (re-search-forward
                                   (concat "\\<" (regexp-quote label) "\\>")
                                   params-end t)
                                  (list (match-beginning 0) (match-end 0))))
                         (mapcar #'1+ (append label nil)))))
                   (if (and beg end)
                       (add-face-text-property
                        beg end
                        'oaty/eglot-function-argument-face))))
               ;; ...and/or maybe add its doc on a line by its own.
               (when documentation
                 (goto-char (point-max))
                 (insert "\n"
                         (propertize
                          (if (stringp label)
                              label
                            (apply #'buffer-substring (mapcar #'1+ label)))
                          'face 'eldoc-highlight-function-argument)
                         ": " (eglot--format-markup documentation))))))
         (buffer-string))))
   when moresigs concat "\n"))

;; Fixed to send <tab> instead of TAB, so it doens't conflict
;; with C-i.
(defun org-indent-line ()
  (interactive)
  (let* ((element (save-excursion (beginning-of-line) (org-element-at-point-no-context)))
	 (type (org-element-type element)))
    (unless (or (org-at-heading-p)
                (and (eq org-adapt-indentation 'headline-data)
                     (not (org--at-headline-data-p nil element))
                     (save-excursion
                       (goto-char (1- (org-element-property :begin element)))
                       (or (org-at-heading-p)
                           (org--at-headline-data-p)))))
      (cond ((and (memq type '(plain-list item))
		  (= (line-beginning-position)
		     (org-element-property :post-affiliated element)))
	     nil)
	    ((and (eq type 'latex-environment)
		  (>= (point) (org-element-property :post-affiliated element))
		  (< (point)
		     (org-with-point-at (org-element-property :end element)
		       (skip-chars-backward " \t\n")
		       (line-beginning-position 2))))
	     nil)
	    ((and (eq type 'src-block)
		  org-src-tab-acts-natively
		  (> (line-beginning-position)
		     (org-element-property :post-affiliated element))
		  (< (line-beginning-position)
		     (org-with-point-at (org-element-property :end element)
		       (skip-chars-backward " \t\n")
		       (line-beginning-position))))
             ;; At the beginning of a blank line, do some preindentation.  This
             ;; signals org-src--edit-element to preserve the indentation on exit
             (when (and (looking-at-p "^[[:space:]]*$")
                        (not org-src-preserve-indentation))
               (let ((element (org-element-at-point))
                     block-content-ind some-ind)
                 (org-with-point-at (org-element-property :begin element)
                   (setq block-content-ind (+ (org-current-text-indentation)
                                              org-edit-src-content-indentation))
                   (forward-line)
		   (save-match-data (re-search-forward "^[ \t]*\\S-" nil t))
                   (backward-char)
                   (setq some-ind (if (looking-at-p "#\\+end_src")
                                      block-content-ind (org-current-text-indentation))))
                 (indent-line-to (min block-content-ind some-ind))))
	     (org-babel-do-key-sequence-in-edit-buffer (kbd "<tab>")))
	    (t
	     (let ((column (org--get-expected-indentation element nil)))
	       ;; Preserve current column.
	       (if (<= (current-column) (current-indentation))
		   (indent-line-to column)
		 (save-excursion (indent-line-to column))))
	     ;; Align node property.  Also preserve current column.
	     (when (eq type 'node-property)
	       (let ((column (current-column)))
		 (org--align-node-property)
		 (org-move-to-column column))))))))
