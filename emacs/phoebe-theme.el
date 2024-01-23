(deftheme phoebe
  "A dark theme with moderate contrast.

Inspired by vitesse, gruvbox and modus-operandi.")

(defface phoebe/key-binding nil
  "Face for key bindings.")

(defface phoebe/prompt nil
  "Face for interactive prompts.")

(defface phoebe/comp-match nil
  "Face for completion matches.

Note: Ivy uses additional faces to distinguish between
parts of a search. The `-1', `-2', and `-3' variants
of this face takes care of that.")

(defface phoebe/comp-match-1 nil
  "Face for completion matches numbered 1 mod 3.")

(defface phoebe/comp-match-2 nil
  "Face for completion matches numbered 2 mod 3.")

(defface phoebe/comp-match-3 nil
  "Face for coimpletion matches numbered 0 (3) mod 3.")

(defface phoebe/comp-selected nil
  "Face for highlighting the current match.")

(defface phoebe/comp-next nil
  "Face for the next character to disambiguate a match.")

(defface phoebe/comp-annotation nil
  "Face for completion annotations.")

(defface phoebe/block-header nil
  "Face for block headers.")

(defface phoebe/block nil
  "Face for block bodies.")

(dotimes (n 9)
  (let* ((heading-N (intern (format "phoebe/heading-%d" n))))
    (custom-declare-face 
     heading-N nil
     (format "Face for level-%d headings." n))))

(defface phoebe/prose-verb nil
  "Face for verbatim text in prose.")

(defface phoebe/prose-code nil
  "Face for code in prose.")

(let* ((green1      "#4d9375")
       (green2      "#65A66B")
       (green3      "#80a665")
       (green4      "#7db583")

       (yellow1     "#e6cc77")
       (yellow2     "#debd52")
       (yellow3     "#e6d6a1")
       (yellow4     "#FABD2F")

       (orange1     "#f2ab55")
       (orange2     "#ffb130")

       (red1        "#cb7676")
       (red2        "#c98a7d")
       (red3        "#c99076")
       (red4        "#db4934")
       (red5        "#cc241d")
       (red6        "#e06552")

       (cyan1       "#5da9a7")
       (cyan2       "#5eaab5")
       (cyan3       "#73b5bf")

       (blue1       "#6394bf")
       (blue2       "#88a7bb")
       (blue3       "#83a598")

       (magenta1    "#d9739f")
       (magenta2    "#de87ad")


       (purple1     "#6d4fa3")
       (purple6     "#5a32a3")

       (ivory2      "#eee8d5")
       (off-white   "#ded6bd")

       (brown1      "#6c4f3a")
       (brown2      "#80614a")
       (brown3      "#d4976c")

       (melon       "#FDBCB4")
       (apricot     "#FBCEB1")
       (tangerine   "#df9774")

       (pitch-black           "#000000")
       (pitch-black2          "#0C0C0C")
       (black                 "#111111")
       (black1                "#252525")
       (black2-dim            "#2c2c2c")
       (black2                "#393939")
       (black2-bright         "#434343")
       (black3                "#4d4d4d")
       (black4                "#5d5d5d")
       (white3                "#c3c3c3")
       (white2                "#d7d7d7")
       (white1                "#ebebeb")
       (white                 "#ffffff")

       (bg0                   "#282828")
       (bg0_h                 "#1d2021")
       (bg1                   "#3c3836")

       (bg                    black)
       (bg-active             "#151515")

       (bg-inactive           black1)
       (bg-highlight          black3)
       (bg-dim                black2)

       (fg          white1)
       (fg-dim      black3)
       (fg-alt      blue1)

       (fg0                   "#A89984")
       (fg1                   "#BDAE84")
       (fg2                   "#D5C4A1")
       (fg3                   "#EBDBB2")
       (fg4                   "#FBF1C7")

       (border      pitch-black)

       (bg-completion    black1)

       (bg-link          bg)
       (link             blue1)
       (bg-link-visited  bg-link)
       (link-visited     magenta1)

       ;; Status colors
       (err                   red1)
       (warning               yellow1)
       (info                  green1)
       (bg-err                red2)
       (bg-warning            yellow1)
       (bg-info               green1)
       (underline-err         red2)
       (underline-warning     yellow1)
       (underline-info        green1)

       (bg-region             black1)
       (bg-sel                black2)

       (fg-linum-active       white3)
       (bg-linum-active       white3)
       (fg-linum-active       white3)

       (bg-modeline           white3)
       (fg-modeline           bg)
       (bg-modeline-inactive  bg-dim)
       (fg-modeline-inactive  fg)
       (bg-modeline-vi        white3)
       (bg-modeline-vi-N      orange1)
       (bg-modeline-vi-I      blue1)
       (bg-modeline-vi-V      yellow1)
       (bg-modeline-vi-R      red1)
       (bg-modeline-vi-E      white3)
       (bg-modeline-vi-M      white3)

       ;; Markup
       (date-common           blue1)
       (date-deadline         red1)
       (date-event            fg-alt)
       (date-holiday          red1)
       (date-now              fg)
       (date-range            fg-alt)
       (date-scheduled        yellow2)
       (date-weekday          cyan1)
       (date-weekend          red2)

       (heading-0             green1)
       (heading-1             fg)
       (heading-2             white3)
       (heading-3             white3)
       (heading-4             white3)
       (heading-5             heading-1)
       (heading-6             heading-2)
       (heading-7             heading-3)
       (heading-8             heading-4)

       (prose-done            green1)
       (prose-todo            red1)

       (prose-block           fg-dim)
       (prose-code            green1)
       (prose-macro           magenta1)
       (prose-metadata        fg-dim)
       (prose-metadata-value  fg-alt)
       (prose-table           fg-alt)
       (prose-tag             blue1)
       (prose-verbatim        magenta1)

       ;; Programming language stuff
       (variable              orange1)
       (builtin               green4)
       (comment               black2)
       (constant              tangerine)
       (docstring             yellow1)
       (docmarkup             yellow2)
       (function              yellow1)
       (keyword               green1)
       (macro                 red2)
       (string                blue3)
       (type                  green2)
       (regex                 blue1))

  (custom-theme-set-variables
   'phoebe
   ;; Variables
   '(window-divider-default-right-width 1)
   '(org-fontify-quote-and-verse-blocks nil))
  (custom-theme-set-faces
   'phoebe

   ;; Completion highlighting
   `(phoebe/key-binding       ((t :inherit     bold
                                  :foreground ,green1)))
   `(phoebe/prompt            ((t :foreground ,yellow1)))

   `(phoebe/comp-match        ((t :inherit     bold)))
   `(phoebe/comp-match-1      ((t :inherit     phoebe/comp-match
                                  :foreground ,green1)))
   `(phoebe/comp-match-2      ((t :inherit     phoebe/comp-match
                                  :foreground ,green2)))
   `(phoebe/comp-match-3      ((t :inherit     phoebe/comp-match
                                  :foreground ,green3)))
   `(phoebe/comp-next         ((t :inherit     bold)))
   `(phoebe/comp-annotation   ((t :foreground ,black3)))

   ;; Foreground is unspecified to preserve `-match-*' colors!
   `(phoebe/comp-selected     ((t :background ,black4)))


   ;; Language notes
   `(phoebe/lang-error
     ((t :underline (:style wave :color ,underline-err))))
   `(phoebe/lang-info
     ((t :underline (:style wave :color ,underline-info))))
   `(phoebe/lang-warning
     ((t :underline (:style wave :color ,underline-warning))))

   ;; Prominent semantic notes
   `(phoebe/prominent-error   ((t :foreground ,fg
                                  :background ,bg-err)))
   `(phoebe/prominent-info    ((t :foreground ,fg
                                  :background ,bg-info)))
   `(phoebe/prominent-warning ((t :foreground ,fg
                                  :background ,bg-warning)))

   ;; Org, etc
   `(phoebe/heading-0 ((t :inherit     bold
                          :foreground ,heading-0)))
   `(phoebe/heading-1 ((t :inherit     phoebe/heading-0
                          :foreground ,heading-1)))
   `(phoebe/heading-2 ((t :inherit     phoebe/heading-1
                          :foreground ,heading-2)))
   `(phoebe/heading-3 ((t :inherit     phoebe/heading-2
                          :foreground ,heading-3)))
   `(phoebe/heading-4 ((t :inherit     phoebe/heading-3
                          :foreground ,heading-4)))
   `(phoebe/heading-5 ((t :inherit     phoebe/heading-4
                          :foreground ,heading-5)))
   `(phoebe/heading-6 ((t :inherit     phoebe/heading-5
                          :foreground ,heading-6)))
   `(phoebe/heading-7 ((t :inherit     phoebe/heading-6
                          :foreground ,heading-7)))
   `(phoebe/heading-8 ((t :inherit     phoebe/heading-7
                          :foreground ,heading-8)))

   `(phoebe/block         ((t :inherit     default)))
   `(phoebe/block-header  ((t :inherit     font-lock-comment-face)))
   
   `(phoebe/prose-code ((t :foreground ,green3)))
   `(phoebe/prose-verb ((t :foreground ,magenta1)))

   ;; Essentials
   `(default             ((t :background ,bg
                             :foreground ,fg)))
   `(cursor              ((t :background ,fg)))
   `(fringe              ((t :inherit default)))
   `(menu                ((t :inherit default)))
   `(scroll-bar          ((t :inherit default)))
   `(tool-bar            ((t :inherit default)))
   `(vertical-border     ((t :foreground ,border
                             :background ,border)))

   `(window-divider-first-pixel ((t :foreground ,black2-dim)))
   `(window-divider             ((t :foreground ,border)))
   `(window-divider-last-pixel  ((t :foreground ,black2-dim)))

   ;; Basic styles
   `(bold                ((t :weight      bold)))
   `(italic              ((t :slant       italic)))
   `(bold-italic         ((t :inherit    (bold italic))))
   `(underline           ((t :underline  ,fg-dim)))
   `(shadow              ((t :foreground ,fg-dim)))

   ;; "Ungrouped" styles
   `(child-frame-border  ((t :background ,border)))
   `(file-name-shadow    ((t :inherit     shadow)))
   `(highlight           ((t :background ,bg-highlight
                             :foreground ,fg)))
   `(next-error          ((t :inherit phoebe/prominent-error
                             :extend t)))

   ;; Search and selection
   `(region
     ((t :background ,bg-region)))

   `(rectangle-preview
     ((t :background ,bg-sel
         :foreground ,yellow1)))

   ;; Status styles
   `(error               ((t :inherit     bold
                             :foreground ,err)))
   `(success             ((t :inherit     bold
                             :foreground ,info)))
   `(warning             ((t :inherit     bold
                             :foreground ,warning)))

   ;; Special characters/whitespace
   `(homoglyph           ((t :foreground ,warning)))
   `(nobreak-hyphen      ((t :foreground ,err)))
   `(nobreak-space       ((t :foreground ,err
                             :underline t)))
   `(escape-glyph        ((t :foreground ,err)))
   `(trailing-whitespace ((t :background ,bg-active)))

   ;; Emacs manual, help pages, ...
   `(help-argument-name  ((t :inherit     italic 
                             :foreground ,variable)))
   `(help-key-binding    ((t :inherit     phoebe/key-binding)))

   ;; Emacs "UI elements"
   `(button              ((t :background ,bg-link
                             :foreground ,link
                             :underline   t)))
   `(link                ((t :inherit     button)))
   `(link-visited        ((t :background ,bg-link-visited
                             :foreground ,link-visited
                             :underline   t)))
   `(tooltip             ((t :background ,bg-active
                             :foreground ,fg)))
   `(menu                ((t :background ,bg-active
                             :foreground ,fg)))
   `(separator-line      ((t :underline  ,bg-active)))
   `(minibuffer-prompt   ((t :inherit     phoebe/prompt)))

   ;; Line Numbers
   ;; Comment from Prot:
   ;; We need to fall back to `default' otherwise line numbers
   ;; do not scale when using `text-scale-adjust'.
   `(line-number              ((t :inherit default
                                  :background ,bg
                                  :foreground ,black3)))
   `(line-number-current-line ((t :inherit default
                                  :background ,bg
                                  :foreground ,yellow2)))
   `(line-number-major-tick   ((t :inherit     line-number
                                  :foreground ,green1)))
   `(line-number-minor-tick   ((t :inherit     line-number
                                  :foreground ,yellow1)))

   ;; Buffer list
   `(buffer-menu-buffer  ((t :inherit     bold
                             :foreground ,yellow1)))

   ;; auto-dim-other-buffers
   `(auto-dim-other-buffers-face
     ((t :background ,bg-inactive)))

   ;; font-lock
   `(font-lock-builtin-face              ((t :inherit     bold
                                             :foreground ,builtin)))
   `(font-lock-comment-delimiter-face    ((t :inherit     font-lock-comment-face)))
   `(font-lock-comment-face              ((t :inherit     italic
                                             :foreground ,comment)))
   `(font-lock-constant-face             ((t :foreground ,constant)))
   `(font-lock-doc-face                  ((t :foreground ,docstring)))
   `(font-lock-doc-markup-face           ((t :foreground ,docmarkup)))
   `(font-lock-function-name-face        ((t :foreground ,function)))
   `(font-lock-keyword-face              ((t :inherit     bold
                                             :foreground ,keyword)))
   `(font-lock-negation-char-face        ((t :inherit     error)))
   `(font-lock-preprocessor-face         ((t :foreground ,macro)))
   `(font-lock-regexp-grouping-backslash ((t :foreground ,regex)))
   `(font-lock-regexp-grouping-construct ((t :inherit     font-lock-regexp-grouping-backslash)))
   `(font-lock-string-face               ((t :foreground ,string)))
   `(font-lock-type-face                 ((t :inherit     bold
                                             :foreground ,type)))
   `(font-lock-variable-name-face        ((t :foreground ,variable)))
   `(font-lock-warning-face              ((t :inherit     bold
                                             :foreground ,warning)))

   ;; Julia
   `(julia-macro-face         ((t :inherit font-lock-builtin-face)))
   `(julia-quoted-symbol-face ((t :inherit font-lock-constant-face)))


   `(completions-annotations      ((t :inherit phoebe/comp-annotation)))
   `(completions-common-part      ((t :inherit phoebe/comp-match)))
   `(completions-first-difference ((t :inherit phoebe/comp-next)))

   ;; Ivy
   `(ivy-action               ((t :inherit phoebe/key-binding)))
   `(ivy-confirm-face         ((t :inherit success)))
   `(ivy-match-required-face  ((t :inherit error)))

   `(ivy-minibuffer-match-face-1 ((t :inherit phoebe/comp-match)))
   `(ivy-minibuffer-match-face-2 ((t :inherit phoebe/comp-match-1)))
   `(ivy-minibuffer-match-face-3 ((t :inherit phoebe/comp-match-2)))
   `(ivy-minibuffer-match-face-4 ((t :inherit phoebe/comp-match-3)))
   `(ivy-current-match        ((t :inherit phoebe/comp-selected)))

   `(ivy-remote               ((t :inherit italic)))
   `(ivy-separator            ((t :inherit shadow)))
   `(ivy-subdir               ((t :foreground ,green2)))
   `(ivy-virtual              ((t :foreground ,cyan1)))

   ;; Corfu
   `(corfu-current            ((t :inherit phoebe/comp-selected)))
   `(corfu-bar                ((t :background ,white3)))
   `(corfu-border             ((t :background ,border)))
   `(corfu-default            ((t :background ,bg-dim)))
   `(corfu-annotations        ((t :foreground ,green1)))
   `(corfu-deprecated         ((t :foreground ,black2)))

   `(oaty/eglot-function-name-face
     ((t :foreground ,blue1)))

   ;; Org basics
   `(org-verbatim ((t :inherit phoebe/prose-verb)))
   `(org-code     ((t :inherit phoebe/prose-code)))

   `(org-link     ((t :inherit link)))
   `(org-footnote ((t :inherit link)))

   `(org-latex-and-related ((t :foreground ,yellow1)))

   `(org-document-info ((t :foreground ,prose-metadata-value)))
   `(org-document-info-keyword ((t :foreground ,prose-metadata)))
   `(org-document-title ((t :inherit phoebe/heading-0)))

   `(org-level-1       ((t :inherit phoebe/heading-1)))
   `(org-level-2       ((t :inherit phoebe/heading-2)))
   `(org-level-3       ((t :inherit phoebe/heading-3)))
   `(org-level-4       ((t :inherit phoebe/heading-4)))
   `(org-level-5       ((t :inherit phoebe/heading-5)))
   `(org-level-6       ((t :inherit phoebe/heading-6)))
   `(org-level-7       ((t :inherit phoebe/heading-7)))
   `(org-level-8       ((t :inherit phoebe/heading-8)))
   `(org-headline-done ((t :inherit org-done
                           :foreground ,green3)))
   `(org-headline-todo ((t :inherit org-todo)))
   ;; Use to hide things by making the foreground equal to the background
   `(org-hide          ((t :foreground ,bg)))

   `(org-checkbox ((t :inherit     bold
                      :foreground ,blue3)))

   ;; Org blocks
   `(org-block                ((t :inherit  phoebe/block)))
   `(org-block-end-line       ((t :inherit (font-lock-comment-face
                                            phoebe/block))))
   `(org-block-begin-line     ((t :inherit  phoebe/block-header)))

   `(org-quote                ((t :inherit org-block)))
   `(org-verse                ((t :inherit org-block)))

   ;; Org tables
   `(org-table        ((t :foreground ,prose-table)))
   `(org-table-header ((t :inherit (bold org-table))))

   ;; Org metadata
   `(org-drawer          ((t :foreground ,prose-metadata)))
   `(org-special-keyword ((t :inherit     org-drawer)))
   `(org-date            ((t :foreground ,date-common)))
   `(org-date-selected   ((t :foreground ,date-common
                             :inverse-video t)))
   `(org-clock-overlay   ((t :inherit secondary-selection)))
   `(org-done            ((t :foreground ,prose-done)))
   `(org-todo            ((t :foreground ,prose-todo)))
   `(org-priority        ((t :foreground ,prose-tag)))
   `(org-tag             ((t :foreground ,prose-tag)))
   `(org-tag-group       ((t :inherit    (bold org-tag))))
   `(org-property-value  ((t :foreground ,prose-metadata-value)))

   ;; Org Agenda
   `(org-imminent-deadline ((t :inherit     bold
                               :foreground ,date-deadline)))

   ;; Org (uncategorized)
   `(org-warning ((t :inherit warning)))
   `(org-sexp-date ((t :foreground ,date-common)))
   `(org-macro ((t :foreground ,prose-macro)))
   `(org-time-grid ((t :foreground ,fg-dim)))

   ;; `(org-scheduled ((t :foreground ,date-scheduled)))
   ;; `(org-scheduled-previously ((t :inherit org-scheduled)))
   ;; `(org-scheduled-today ((t :inherit (modus-themes-bold org-scheduled))))
   ;; `(org-upcoming-deadline ((t :foreground ,date-deadline)))
   ;; `(org-upcoming-distant-deadline ((t :inherit org-upcoming-deadline)))

   ;; `(org-agenda-calendar-daterange ((t :foreground ,date-range)))
   ;; `(org-agenda-calendar-event ((t :foreground ,date-event)))
   ;; `(org-agenda-calendar-sexp ((t :inherit (modus-themes-slant org-agenda-calendar-event))))
   ;; `(org-agenda-clocking ((t :inherit modus-themes-mark-alt)))
   ;; `(org-agenda-column-dateline ((t :background ,bg-inactive)))
   ;; `(org-agenda-current-time ((t :foreground ,date-now)))
   ;; `(org-agenda-date ((t ,@(modus-themes--heading 'agenda-date date-weekday))))
   ;; `(org-agenda-date-today ((t :inherit org-agenda-date :underline t)))
   ;; `(org-agenda-date-weekend ((t :inherit org-agenda-date :foreground ,date-weekend)))
   ;; `(org-agenda-date-weekend-today ((t :inherit org-agenda-date-today :foreground ,date-weekend)))
   ;; `(org-agenda-diary ((t :inherit org-agenda-calendar-sexp)))
   ;; `(org-agenda-dimmed-todo-face ((t :inherit shadow)))
   ;; `(org-agenda-done ((t :inherit org-done)))
   ;; `(org-agenda-filter-category ((t :inherit bold :foreground ,modeline-err)))
   ;; `(org-agenda-filter-effort ((t :inherit bold :foreground ,modeline-err)))
   ;; `(org-agenda-filter-regexp ((t :inherit bold :foreground ,modeline-err)))
   ;; `(org-agenda-filter-tags ((t :inherit bold :foreground ,modeline-err)))
   ;; `(org-agenda-restriction-lock ((t :background ,bg-dim :foreground ,fg-dim)))
   ;; `(org-agenda-structure ((t ,@(modus-themes--heading 'agenda-structure fg-alt))))
   ;; `(org-agenda-structure-filter ((t :inherit org-agenda-structure :foreground ,warning)))
   ;; `(org-agenda-structure-secondary ((t :inherit font-lock-doc-face)))

   ;; `(org-mode-line-clock (( )))
   ;; `(org-mode-line-clock-overrun ((t :inherit bold :foreground ,modeline-err)))

   ;; Modeline
   `(mode-line          ((t :background ,white3
                            :foreground ,bg)))
   `(mode-line-active   ((t :inherit mode-line)))
   `(mode-line-inactive ((t :inherit mode-line
                            :background ,black3
                            :foreground ,fg)))

   ;; Oatys Modeline
   `(oaty/modeline-vi-state          ((t :inherit mode-line
                                         :background ,white1)))
   `(oaty/modeline-vi-state-inactive ((t :inherit oaty/modeline-vi-state
                                         :background ,black4)))

   `(oaty/modeline-buffer-id          ((t :inherit mode-line
                                          :background ,white2)))
   `(oaty/modeline-buffer-id-inactive ((t :inherit oaty/modeline-buffer-id
                                          :background ,black2
                                          :foreground ,fg)))

   `(oaty/modeline-vi-cursor          ((t :inherit mode-line)))
   `(oaty/modeline-vi-cursor-inactive ((t )))

   `(oaty/modeline-major-mode          ((t :inherit mode-line)))
   `(oaty/modeline-major-mode-inactive ((t )))

   `(oaty/modeline-vi-normal   ((t :inherit oaty/modeline-vi-state
                                   :background ,green3)))
   `(oaty/modeline-vi-insert   ((t :inherit oaty/modeline-vi-state
                                   :background ,blue1)))
   `(oaty/modeline-vi-visual   ((t :inherit oaty/modeline-vi-state
                                   :background ,magenta1)))
   `(oaty/modeline-vi-replace  ((t :inherit oaty/modeline-vi-state
                                   :background ,red4)))
   `(oaty/modeline-vi-emacs    ((t :inherit oaty/modeline-vi-state
                                   :background ,purple1)))

   `(oaty/modeline-buffer-id-roam          ((t :inherit oaty/modeline-buffer-id
                                               :background ,yellow3)))
   `(oaty/modeline-buffer-id-roam-inactive ((t :inherit oaty/modeline-buffer-id-inactive)))

   ;; `(mode-line-buffer-id ((t :inherit bold)))
   ;; `(mode-line-emphasis ((t :inherit bold :foreground ,modeline-info)))
   ;; `(mode-line-highlight ((t :background ,bg-hover :foreground ,fg-main :box ,fg-main)))
   ;; `(mode-line-inactive ((t :inherit modus-themes-ui-variable-pitch
   ;;                           :box ,border-mode-line-inactive
   ;;                           :background ,bg-mode-line-inactive
   ;;                           :foreground ,fg-mode-line-inactive)))


   ;; Hydra
   `(hydra-face-red      ((t :foreground ,red4)))
   `(hydra-face-blue     ((t :foreground ,blue1)))

   ;; ANSI Terminal colors
   ;; Bizarrely, vterm ignores the *-bright-* colors, and uses the :background
   ;; color to draw bright colors.
   ;; Bright colors are thus a little fucked, even in modus operandi.
   `(ansi-color-black ((t :background ,black3 :foreground ,pitch-black)))
   `(ansi-color-blue ((t :background ,blue2 :foreground ,blue1)))
   `(ansi-color-cyan ((t :background ,cyan3 :foreground ,cyan2)))
   `(ansi-color-green ((t :background ,green4 :foreground ,green2)))
   `(ansi-color-magenta ((t :background ,magenta2 :foreground ,magenta1)))
   `(ansi-color-red ((t :background ,red6 :foreground ,red4)))
   `(ansi-color-white ((t :background ,white1 :foreground ,white2)))
   `(ansi-color-yellow ((t :background ,yellow3 :foreground ,yellow2)))
   ;; These are ignored by vterm
   `(ansi-color-bright-black ((t :background ,black3 :foreground ,black3)))
   `(ansi-color-bright-blue ((t :background ,blue2 :foreground ,blue2)))
   `(ansi-color-bright-cyan ((t :background ,cyan3 :foreground ,cyan3)))
   `(ansi-color-bright-green ((t :background ,green4 :foreground ,green4)))
   `(ansi-color-bright-magenta ((t :background ,magenta2 :foreground ,magenta2)))
   `(ansi-color-bright-red ((t :background ,red6 :foreground ,red6)))
   `(ansi-color-bright-white ((t :background ,white1 :foreground ,white1)))
   `(ansi-color-bright-yellow ((t :background ,yellow3 :foreground ,yellow3)))

   `(ansi-color-bold ((t :inherit bold)))

   ;; Eldoc
   `(eldoc-highlight-function-argument ((t :inherit bold :underline t)))
   ))


(provide-theme 'phoebe)
