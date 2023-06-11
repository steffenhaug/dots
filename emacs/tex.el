;; TeX setup
(require 'tex)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)


(defun oaty/guess-master ()
  "Try to guess which file is the 'master' TeX file."
  (let* ((root (project-root (project-current)))
         (main (expand-file-name "main.tex" root)))
    (cond ((file-exists-p main) main)
          (t                    nil))))


;; Configure TeX compiler
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (let* ((master (oaty/guess-master)))
              (setq TeX-master master)
              (setq TeX-engine 'luatex)
              (setq TeX-command-extra-options
                    "-shell-escape --synctex=1"))))
         

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
