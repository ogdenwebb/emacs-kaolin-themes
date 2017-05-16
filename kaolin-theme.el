;;; kaolin-theme.el --- A dark jade theme inspired by Sierra.vim

;; Copyright (C) 2017 0rdy

;; Author: 0rdy <mail@0rdy.com>
;; URL: https://github.com/0rdy/kaolin-theme
;; Package-Requires: ((emacs "24"))
;; Version: 0.7.4

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; Kaolin is a dark jade, eye pleasing theme for Emacs with support
;; a large number of specific modes and external packages.
;;
;;; Code:

(deftheme kaolin "A dark jade theme")

(defgroup kaolin-theme nil
  "Kaolin theme properties"
  :prefix "kaolin-"
  :group 'faces)

(defcustom kaolin-bold t
  "If nil, disable the bold style."
  :group 'kaolin-theme)

(defcustom kaolin-italic t
  "If nil, disable the italic style."
  :group 'kaolin-theme)

(defcustom kaolin-underline t
  "If nil, disable the underline style."
  :group 'kaolin-theme)

(defface kaolin-boolean nil
  "Face to highlight boolean values"
  :group 'kaolin-theme)

;; Kaolin color palette
(let ((c '((class color) (min-colors 89)))
      (black           "#1b1b1b")
      (alt-black       "#181818")
      (dark-gray       "#2a2a2a")
      (dim-gray        "#353535")
      (gray            "#545c5e")
      (alt-gray        "#60696b")
      ;; (light-gray      "#859092")
      (light-gray      "#9191a2")
      ;; (white           "#c5c8c6")
      (white           "#c8c8d0")

      (brown           "#7d6360")
      (light-brown     "#ae9895")
      (alt-brown       "#52413f")

      (dark-red        "#832729")
      (red             "#d75f5f")
      ;; (faded-red       "#ac4040")
      (faded-red       "#a94d53")
      (alt-red         "#c93232")
      (light-red       "#d66e75")
      ;; (deep-pink    "#d75f91")
      (deep-pink       "#d24b83")

      (alt-orange      "#d9a76f")
      (orange          "#dbac66")
      (light-orange    "#ddc085")
      ;; (pure-orange     "#cc5900")
      (pure-orange     "#cc6a00")

      (dark-yellow     "#555a2f")
      (yellow          "#acb370")
      (alt-yellow      "#be9266")
      ;; (light-yellow "#c1b175")
      (light-yellow    "#c9bb87")
      (wheat           "#b9c791")

      (dark-jade       "#2e4039")
      (jade            "#597a6e")
      (light-jade      "#709688")
      (midnight-green  "#152628")
      ;; (deep-green   "#30555a")
      (deep-green      "#39656b")
      (green           "#4a858c")
      (dark-green      "#39855f")
      (light-green     "#54b685")
      (lime            "#85b654")
      (teal            "#80b6bc")
      (teal-blue       "#91b9c7")
      ;; (teal-blue      "#91c7c7")
      ;; (teal-green      "#80bea0")
      (teal-green      "#6fb593")

      (dark-blue       "#2a4661")
      ;; TODO: Change blue color
      ;; (blue            "#5485b6")
      (blue            "#5077a5")
      ;; (alt-blue        "#6666be")
      (alt-blue        "#267fb5")
      (cyan            "#54b6b6")
      ;; (faded-blue      "#857f96")
      (faded-blue      "#817f96")

      (midnight-purple "#1a121a")
      (dark-purple     "#563d56")
      (purple          "#835d83")
      (magenta         "#5454b6")
      (light-purple    "#cea2ca")
      ;; (alt-purple      "#8c4a64")
      (alt-purple      "#915c83")

      (violet          "#ab98b5")

      ;; Face options
      (bold         kaolin-bold)
      (italic       kaolin-italic)
      (underline    kaolin-underline))

  ;; Theme colors
  (let* ((fg1  white)
         (fg2  "#babac4")
         (fg3  "#adadb9")
         (fg4  "#9f9fad")
         (bg1  black)
         (bg2  "#282828")
         (bg3  "#353535")
         (bg4  "#414141")
         ;; TODO: move this part
         (key2 "#5f9298")
         (key3 "#41757b")

         (dim-buffer alt-black)
         (hl-line    bg2)
         (tooltip-bg bg2)
         (tooltip-fg light-gray)
         (tooltip-hl alt-brown)

         (rb1 teal)
         (rb2 violet)
         (rb3 jade)
         (rb4 faded-blue)
         (rb5 green)
         (rb6 light-brown)
         (rb7 light-green)
         (rb8 wheat)

         (diff-add    light-green)
         (diff-change violet)
         (diff-del    red)

         (line-fg           fg4)
         (line-bg           bg2)
         (line-border       bg3)
         (segment-active    gray)
         (segment-inactive  gray)
         (evil-normal       green)
         (evil-insert       light-green)
         (evil-visual       orange)
         (evil-replace      red)
         (evil-motion       yellow)
         (evil-operator     evil-normal)
         (evil-emacs        light-yellow)

         (cursor     light-gray)
         (keyword    green)
         (hl         cyan)
         (hl-indent  gray)
         (builtin    teal)
         (comment    gray)
         ;; Light
         ;; (comment    "#869a90")
         ;; (comment    "#8f9ca7")
         (win-border dark-gray)
         (functions  teal)
         (str        teal-green)
         (str-alt    jade)
         (doc        str-alt)
         (type       alt-orange)
         (const      violet)
         (var        faded-blue)
         ;; TODO: change number color ?? light-green
         (num        faded-red)
         (bool       num)
         (prep       alt-purple)
         (warning    orange)
         (err        red))

    (custom-theme-set-faces
     'kaolin
     ;; Font-lock
     `(font-lock-builtin-face ((,c (:foreground ,builtin))))
     `(font-lock-comment-face ((,c (:foreground ,comment))))
     `(font-lock-comment-delimiter-face ((,c (:foreground ,comment))))
     `(font-lock-constant-face ((,c (:foreground ,const))))
     `(font-lock-reference-face ((,c (:foreground ,const))))
     `(font-lock-string-face ((,c (:foreground ,str))))
     `(font-lock-doc-face ((,c (:foreground ,doc))))
     `(font-lock-function-name-face ((,c (:foreground ,functions :bold ,bold))))
     `(font-lock-keyword-face ((,c (:foreground ,keyword :bold ,bold))))
     `(font-lock-negation-char-face ((,c (:foreground ,const))))
     `(font-lock-type-face ((,c (:foreground ,type))))
     `(font-lock-variable-name-face ((,c (:foreground ,var))))
     `(font-lock-warning-face ((,c (:background nil :foreground ,warning))))
     `(font-lock-preprocessor-face ((,c (:foreground ,prep :bold nil))))
     `(font-lock-negation-char-face ((,c (:foreground ,cyan :bold nil))))

     ;; Kaolin faces
     `(kaolin-boolean ((,c (:foreground ,bool))))

     ;; General
     `(default ((,c (:background ,bg1 :foreground ,fg1))))
     `(warning ((,c (:foreground ,warning))))
     `(error ((,c (:foreground ,err))))
     `(shadow ((,c (:foreground ,alt-gray))))
     `(file-name-shadow ((,c (:inherit shadow))))
     `(region ((,c (:background ,bg3))))
     `(secondary-selection ((,c (:background ,green :foreground ,bg1))))
     `(fringe ((,c (:background ,bg1 :foreground ,fg1))))
     `(cursor ((,c (:background ,cursor))))
     `(vertical-border ((,c (:foreground ,win-border))))
     `(minibuffer-prompt ((,c (:foreground ,keyword :bold ,bold))))
     `(default-italic ((,c (:italic ,italic))))
     `(link ((,c (:foreground ,cyan :underline ,underline))))
     `(success ((,c (:background nil :foreground ,light-green))))
     `(escape-glyph ((,c (:background nil :foreground ,cyan))))

     `(menu ((,c (:background ,bg2 :foreground ,fg2))))
     `(header-line ((,c (:background ,bg2 :foreground ,jade))))
     `(tooltip ((,c (:foreground ,tooltip-bg :foreground ,tooltip-fg))))

     `(match ((,c (:background nil :foreground ,cyan))))
     `(isearch ((,c (:background nil :foreground ,light-green :bold ,bold :underline ,underline))))
     `(isearch-fail ((,c (:background nil :foreground ,red))))

     ;; Interface
     `(custom-button ((,c (:background ,bg4 :foreground ,teal :box (:line-width 3 :color ,bg3 :style nil)))))
     `(custom-button-mouse ((,c (:background ,bg3 :foreground ,cyan :box (:line-width 3 :color ,bg2 :style nil)))))
     `(custom-button-pressed ((,c (:background ,bg3 :foreground ,cyan :box (:line-width 3 :color ,bg2 :style pressed-button)))))
     `(custom-visibility ((,c (:background nil :foreground ,cyan :height 0.9 :underline ,underline))))
     `(custom-state ((,c (:background nil :foreground ,light-green))))
     `(custom-changed ((,c (:background nil :foreground ,orange))))
     `(custom-invalid ((,c (:background nil :foreground ,red))))
     `(custom-face-tag ((,c (:background nil :foreground ,purple :bold ,bold))))
     `(custom-link ((,c (:background nil :foreground ,teal :bold ,bold))))
     `(widget-button ((,c (:background nil :foreground ,green :bold ,bold))))
     `(widget-field ((,c (:background ,bg3 :foreground ,fg1 :box (:line-width 1 :color ,bg2 :style nil)))))


     ;; Additional highlighting
     `(highlight ((,c (:background ,bg2 :foreground ,hl))))
     `(lazy-highlight ((,c (:background ,bg3 :foreground ,fg2))))
     `(hl-line ((,c (:background ,bg2))))
     `(highlight-numbers-number ((,c (:foreground ,num))))
     `(highlight-quoted-quote ((t (:foreground ,teal)))) ; Face to highlight Lisp quotes
     `(highlight-quoted-symbol ((t (:foreground ,green)))) ; Face to highlight quoted Lisp symbols

     ;; Eldoc
     `(eldoc-highlight-function-argument ((t (:foreground ,violet :bold ,bold))))

     ;; Highlight indent guides
     `(highlight-indent-guides-odd-face  ((t (:background ,hl-indent))))
     `(highlight-indent-guides-even-face  ((t (:background ,hl-indent))))
     `(highlight-indent-guides-character-face  ((t (:foreground ,hl-indent))))

     ;; Highlighting indentation
     `(highlight-indentation-face  ((t (:background ,bg2))))
     `(highlight-indentation-current-column-face  ((t (:background ,bg3))))

     ;; Linum & nlinum
     `(linum ((t (:background ,bg1 :foreground ,gray :bold nil))))
     `(nlinum-current-line ((t (:background ,bg1 :foreground ,green))))
     `(linum-relative-current-line ((t (:background ,bg1 :foreground ,green))))
     `(linum-highlight-face ((t (:inherit linum))))

     ;; Auto-dim-other-buffers
     `(auto-dim-other-buffers-face  ((t (:background ,dim-buffer))))

     ;; Fic-mode
     `(fic-face  ((t (:background nil :foreground ,red :bold ,bold))))
     `(fic-author-face  ((t (:background nil :foreground ,red :bold ,bold))))

     ;; Modeline
     ;; `(mode-line ((,c (:box (:line-width 1 :color ,line-border) :bold ,bold :background ,line-bg :foreground ,line-fg))))
     `(mode-line ((,c (:box (:line-width 2 :color ,dim-gray) :background ,line-bg :foreground ,faded-blue :bold ,bold))))
     `(mode-line-buffer-id ((,c (:background nil :foreground ,teal :bold ,bold))))
     `(mode-line-highlight ((,c (:foreground ,keyword :box nil :bold ,bold))))
     ;; `(mode-line-inactive ((,c (:box (:line-width 1 :color ,bg2 :style pressed-button) :background ,bg2 :foreground ,light-gray :weight normal))))
     `(mode-line-inactive ((,c (:box (:line-width 2 :color ,line-bg) :background ,line-bg :foreground ,light-gray :bold ,bold))))
     `(mode-line-emphasis ((,c (:foreground ,fg1))))

     ;; Telephone-line
     `(telephone-line-accent-active ((t (:inherit mode-line :background ,dim-gray :foreground ,line-fg))))
     `(telephone-line-accent-inactive ((t (:background ,line-bg :foreground ,light-gray :inherit mode-line-inactive))))
     `(telephone-line-evil ((t (:inherit mode-line))))
     `(telephone-line-evil-normal ((t (:background ,dim-gray :foreground ,evil-normal :inherit telephone-line-evil))))
     `(telephone-line-evil-insert ((t (:background ,dim-gray :foreground ,evil-insert :inherit telephone-line-evil))))
     `(telephone-line-evil-visual ((t (:background ,dim-gray :foreground ,evil-visual :inherit telephone-line-evil))))
     `(telephone-line-evil-replace ((t (:background ,dim-gray :foreground ,evil-replace :inherit telephone-line-evil))))
     `(telephone-line-evil-motion ((t (:background ,dim-gray :foreground ,evil-motion :inherit telephone-line-evil))))
     `(telephone-line-evil-operator ((t (:background ,dim-gray :foreground ,evil-operator :inherit telephone-line-evil))))
     `(telephone-line-evil-emacs ((t (:background ,dim-gray :foreground ,evil-emacs :inherit telephone-line-evil))))

     ;; Powerline
     `(powerline-active1 ((,c (:inherit mode-line))))
     `(powerline-active2 ((,c (:inherit mode-line))))
     `(powerline-inactive1 ((,c (:inherit mode-line-inactive))))
     `(powerline-inactive2 ((,c (:inherit mode-line-inactive))))

     ;; Spaceline
     `(spaceline-highlight-face ((,c (:foreground ,teal))))

     ;; Smart-mode-line
     `(sml/line-number ((t (:foreground ,lime))))
     `(sml/modes ((t (:foreground ,purple))))
     `(sml/global ((t (:foreground ,teal))))
     `(sml/filename ((t (:foreground ,green))))
     `(sml/charging ((t (:foreground ,green))))
     `(sml/discharging ((t (:foreground ,red))))
     `(sml/modified ((t (:foreground ,light-green :bold ,bold))))
     `(sml/outside-modified ((t (:background ,alt-red :foreground ,fg1))))
     `(sml/prefix ((t (:foreground ,line-fg))))
     `(sml/read-only ((t (:foreground ,orange))))

     ;; Flycheck
     `(flycheck-info ((,c (:foreground ,teal-blue))))
     `(flycheck-warning ((,c (:underline (:style line :color ,warning)))))
     `(flycheck-error ((,c (:underline (:style line :color ,err)))))
     `(flycheck-fringe-error ((,c (:foreground ,err))))
     `(flycheck-fringe-warning ((,c (:foreground ,warning))))
     `(flycheck-fringe-info ((,c (:foreground ,teal-blue))))

     ;; Hydra
     `(hydra-face-red ((,c (:foreground ,red))))
     `(hydra-face-teal ((,c (:foreground ,teal))))
     `(hydra-face-blue ((,c (:foreground ,blue))))
     `(hydra-face-pink ((,c (:foreground ,deep-pink))))
     `(hydra-face-amaranth ((,c (:foreground ,purple))))

     ;; Org-mode
     `(org-level-1 ((,c (:foreground ,teal-green :bold ,bold :height 1.1))))
     `(org-level-2 ((,c (:foreground ,teal-blue :bold nil))))
     `(org-level-3 ((,c (:inherit org-level-2))))
     `(org-level-4 ((,c (:inherit org-level-2))))
     `(org-tag ((,c (:foreground ,orange :bold ,bold))))
     `(org-checkbox ((,c (:foreground ,faded-blue :bold ,bold))))
     `(org-todo ((,c (:foreground ,red :bold ,bold))))
     `(org-done ((,c (:foreground ,lime  :bold ,bold))))
     `(org-headline-done ((,c (:foreground ,teal-blue  :bold nil))))
     `(org-checkbox-statistics-todo ((,c (:foreground ,faded-blue :bold ,bold))))
     `(org-checkbox-statistics-done ((,c (:foreground ,lime :bold ,bold))))
     `(org-code ((,c (:foreground ,green))))
     `(org-verbatim ((,c (:foreground ,light-yellow))))
     `(org-hide ((,c (:foreground ,bg2))))
     `(org-date ((,c (:foreground ,light-yellow :underline ,underline))))
     `(org-document-title ((,c (:foreground ,teal :bold ,bold))))
     `(org-document-info-keyword ((,c (:foreground ,deep-green))))
     `(org-meta-line ((,c (:inherit org-document-info-keyword))))
     `(org-document-info ((,c (:foreground ,teal))))
     `(org-footnote  ((,c (:foreground ,fg4 :underline ,underline))))
     `(org-link ((,c (:foreground ,cyan :underline ,underline))))
     `(org-special-keyword ((,c (:foreground ,functions))))
     `(org-block ((,c (:foreground ,fg3))))
     `(org-block-begin-line ((,c (:foreground ,deep-green))))
     `(org-block-end-line ((,c (:inherit org-block-begin-line))))
     `(org-table ((,c (:foreground ,faded-blue :bold ,bold))))
     `(org-formula ((,c (:foreground ,orange))))
     `(org-quote ((,c (:inherit org-block :slant italic))))
     `(org-verse ((,c (:inherit org-block :slant italic))))
     `(org-warning ((,c (:foreground ,warning :underline ,underline))))
     `(org-agenda-structure ((,c (:background ,bg3 :foreground ,fg3 :bold ,bold))))
     `(org-agenda-date ((,c (:foreground ,light-yellow :height 1.1))))
     `(org-agenda-date-weekend ((,c (:weight normal :foreground ,fg4))))
     `(org-agenda-date-today ((,c (:foreground ,purple :height 1.2 :bold ,bold))))
     `(org-agenda-done ((,c (:foreground ,bg4))))
     `(org-scheduled ((,c (:foreground ,type))))
     `(org-scheduled-today ((,c (:foreground ,functions :height 1.2 :bold ,bold))))
     `(org-ellipsis ((,c (:foreground ,builtin))))
     `(org-sexp-date ((,c (:foreground ,fg4))))

     ;; Latex
     `(font-latex-bold-face ((,c (:foreground ,type))))
     `(font-latex-italic-face ((,c (:foreground ,key3 :italic ,italic))))
     `(font-latex-string-face ((,c (:foreground ,str))))
     `(font-latex-match-reference-keywords ((,c (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,c (:foreground ,var))))

     ;; Ido
     `(ido-only-match ((,c (:foreground ,hl))))
     `(ido-first-match ((,c (:foreground ,keyword :bold ,bold))))

     ;; Gnus
     `(gnus-header-content ((,c (:foreground ,keyword))))
     `(gnus-header-from ((,c (:foreground ,var))))
     `(gnus-header-name ((,c (:foreground ,type))))
     `(gnus-header-subject ((,c (:foreground ,functions :bold ,bold))))

     ;; Mu4e
     `(mu4e-header-marks-face ((,c (:foreground ,type))))
     `(mu4e-view-url-number-face ((,c (:foreground ,type))))
     `(mu4e-cited-1-face ((,c (:foreground ,fg2))))
     `(mu4e-cited-7-face ((,c (:foreground ,fg3))))

     `(ffap ((,c (:foreground ,fg4))))

     ;; Js-mode
     `(js2-private-function-call ((,c (:foreground ,const))))
     `(js2-jsdoc-html-tag-delimiter ((,c (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,c (:foreground ,key2))))
     `(js2-external-variable ((,c (:foreground ,type))))
     `(js2-function-param ((,c (:foreground ,const))))
     `(js2-error ((,c (:underline (:color ,alt-red :style wave)))))
     `(js2-function-call ((,c (:foreground ,functions))))
     `(js2-object-property ((,c (:foreground ,light-brown))))
     `(js2-jsdoc-value ((,c (:foreground ,str))))
     `(js2-private-member ((,c (:foreground ,fg3))))
     `(js3-function-param-face ((,c (:foreground ,key3))))
     `(js3-instance-member-face ((,c (:foreground ,const))))
     `(js3-external-variable-face ((,c (:foreground ,var))))
     `(js3-jsdoc-tag-face ((,c (:foreground ,keyword))))
     `(js3-warning-face ((,c (:underline ,keyword))))
     `(js3-error-face ((,c (:underline ,err))))

     `(ac-completion-face ((,c (:foreground ,keyword :underline ,underline))))
     `(info-quoted-name ((,c (:foreground ,builtin))))
     `(info-string ((,c (:foreground ,str))))
     `(icompletep-determined ((,c :foreground ,builtin)))

     ;; Undo-tree
     `(undo-tree-visualizer-current-face ((,c :foreground ,builtin)))
     `(undo-tree-visualizer-default-face ((,c :foreground ,fg2)))
     `(undo-tree-visualizer-unmodified-face ((,c :foreground ,var)))
     `(undo-tree-visualizer-register-face ((,c :foreground ,type)))

     ;; Slime
     `(slime-repl-inputed-output-face ((,c (:foreground ,type))))

     ;; Rainbow delimeters
     `(show-paren-match-face ((,c (:background ,jade :foreground ,bg2))))
     `(show-paren-mismatch-face ((,c (:background ,red :foreground ,bg2))))
     `(rainbow-delimiters-unmatched-face ((,c :foreground ,warning)))
     `(rainbow-delimiters-depth-1-face ((,c (:foreground ,rb1))))
     `(rainbow-delimiters-depth-2-face ((,c :foreground ,rb2)))
     `(rainbow-delimiters-depth-3-face ((,c :foreground ,rb3)))
     `(rainbow-delimiters-depth-4-face ((,c :foreground ,rb4)))
     `(rainbow-delimiters-depth-5-face ((,c :foreground ,rb5)))
     `(rainbow-delimiters-depth-6-face ((,c :foreground ,rb6)))
     `(rainbow-delimiters-depth-7-face ((,c :foreground ,rb7)))
     `(rainbow-delimiters-depth-8-face ((,c :foreground ,rb8)))

     ;; Diff
     `(diff-header ((,c (:background ,bg2))))
     `(diff-file-header ((,c (:background ,bg2 :foreground ,green))))
     `(diff-added ((,c (:background ,dark-green :foreground ,fg1))))
     `(diff-changed ((,c (:background ,diff-change :foreground ,fg1))))
     `(diff-removed ((,c (:background ,dark-red :foreground ,fg1))))

     ;; Ediff
     `(ediff-current-diff-A ((,c (:background ,dark-red :foreground ,red))))
     `(ediff-current-diff-B ((,c (:background ,dark-green :foreground ,light-green))))
     `(ediff-current-diff-C ((,c (:background ,dark-blue :foreground ,teal-blue))))

     `(ediff-even-diff-A ((,c (:background ,bg2))))
     `(ediff-even-diff-B ((,c (:background ,bg2))))
     `(ediff-even-diff-C ((,c (:background ,bg2))))

     `(ediff-fine-diff-A ((,c (:background nil :bold ,bold))))
     `(ediff-fine-diff-B ((,c (:background nil :bold ,bold))))
     `(ediff-fine-diff-C ((,c (:background nil :bold ,bold))))

     `(ediff-odd-diff-A ((,c (:background ,bg3))))
     `(ediff-odd-diff-B ((,c (:background ,bg3))))
     `(ediff-odd-diff-C ((,c (:background ,bg3))))

     ;; Magit
     `(magit-section-highlight ((,c (:background ,bg2))))
     `(magit-diff-file-header ((,c (:background ,bg3 :foreground ,fg2))))
     `(magit-item-highlight ((,c :background ,bg3)))
     `(magit-section-heading ((,c (:foreground ,keyword :bold ,bold))))
     `(magit-hunk-heading ((,c (:background ,bg3))))
     `(magit-hunk-heading-highlight ((,c (:background ,bg3))))
     `(magit-diff-context-highlight ((,c (:background ,bg3 :foreground ,fg3))))
     `(magit-diffstat-added   ((,c (:foreground ,type))))
     `(magit-diffstat-removed ((,c (:foreground ,var))))
     `(magit-process-ok ((,c (:foreground ,functions :bold ,bold))))
     `(magit-process-ng ((,c (:foreground ,warning :bold ,bold))))
     `(magit-branch ((,c (:foreground ,const :bold ,bold))))
     `(magit-log-author ((,c (:foreground ,fg3))))
     `(magit-hash ((,c (:foreground ,fg2))))

     ;; Git gutter
     `(git-gutter:unchanged ((,c (:background ,bg1 :foreground nil))))
     `(git-gutter:added ((,c (:background ,bg1 :foreground ,diff-add :bold ,bold))))
     `(git-gutter:modified ((,c (:background ,bg1 :foreground ,diff-change :bold ,bold))))
     `(git-gutter:deleted ((,c (:background ,bg1 :foreground ,diff-del :bold ,bold))))

     ;; Diff-hl
     `(diff-hl-insert ((,c (:foreground ,light-green))))
     `(diff-hl-change ((,c (:foreground ,yellow))))
     `(diff-hl-delete ((,c (:foreground ,red))))

     ;; Popup
     `(popup-face ((,c (:background ,tooltip-bg :foreground ,tooltip-fg :bold ,bold))))
     `(popup-menu-selection-face ((,c (:background ,tooltip-hl :foreground ,light-orange :bold ,bold))))

     ;; Terminal
     `(term ((t (:foreground ,fg1))))
     `(term-color-black ((t (:foreground ,bg1))))
     `(term-color-blue ((t (:foreground ,blue))))
     `(term-color-red ((t (:foreground ,red))))
     `(term-color-green ((t (:foreground ,green))))
     `(term-color-yellow ((t (:foreground ,yellow))))
     `(term-color-magenta ((,t (:foreground ,purple))))
     `(term-color-cyan ((t (:foreground ,cyan))))
     `(term-color-white ((t (:foreground ,fg2))))

     ;; EShell
     `(eshell-prompt ((t (:foreground ,green :bold ,bold))))
     `(eshell-ls-directory ((t (:foreground ,magenta :bold ,bold))))
     `(eshell-ls-symlink ((t (:foreground ,blue :bold ,bold))))
     `(eshell-ls-executable ((t (:foreground ,lime :bold ,bold))))
     `(eshell-ls-archive ((t (:foreground ,red))))
     `(eshell-ls-backup ((t (:foreground ,purple))))
     `(eshell-ls-clutter ((t (:foreground ,deep-pink))))
     `(eshell-ls-missing ((t (:background ,bg3 :foreground ,red))))
     `(eshell-ls-product ((t (:foreground ,yellow))))
     `(eshell-ls-readonly ((t (:foreground ,fg2))))
     `(eshell-ls-special ((t (:foreground ,light-green))))
     `(eshell-ls-unreadable ((t (:foreground ,faded-blue))))

     ;; Whitespace
     `(whitespace-empty            ((t (:foreground ,red))))
     `(whitespace-line             ((t (:background ,bg2))))
     `(whitespace-space            ((t (:background ,bg2))))
     `(whitespace-tab              ((t (:foreground ,gray))))
     `(whitespace-newline          ((t (:foreground ,gray))))
     `(whitespace-hspace           ((t (:foreground ,orange))))
     `(whitespace-trailing         ((t (:background ,bg1))))

     ;; Smartparens
     `(sp-pair-overlay-face ((t (:foreground nil))))

     ;; Helm
     `(helm-header ((,c (:background ,bg1 :foreground ,fg2 :underline nil :box nil))))
     `(helm-source-header ((,c (:background ,bg1 :foreground ,keyword :underline nil :bold ,bold))))
     `(helm-match ((,c (:inherit default :foreground ,orange :bold ,bold))))
     `(helm-header-line-left-margin ((t (:background ,blue :foreground ,bg1))))
     `(helm-selection ((,c (:background ,bg2 :foreground ,orange :bold ,bold))))
     `(helm-selection-line ((,c (:background ,bg2 :foreground ,orange :bold ,bold))))
     `(helm-visible-mark ((,c (:background ,bg1 :foreground ,blue))))
     `(helm-candidate-number ((,c (:foreground ,lime))))
     `(helm-separator ((,c (:background ,bg1 :foreground ,type))))
     `(helm-time-zone-current ((,c (:background ,bg1 :foreground ,builtin))))
     `(helm-time-zone-home ((,c (:background ,bg1 :foreground ,type))))
     `(helm-buffer-not-saved ((,c (:background ,bg1 :foreground ,type))))
     `(helm-buffer-process ((,c (:background ,bg1 :foreground ,builtin))))
     `(helm-buffer-saved-out ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-buffer-size ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-ff-directory ((,c (:background ,bg1 :foreground ,functions :bold ,bold))))
     `(helm-buffer-directory ((,c (:background ,bg1 :foreground ,purple))))
     `(helm-ff-dotted-directory ((,c (:background ,bg1 :foreground ,functions :bold ,bold))))
     `(helm-ff-dotted-symlink-directory ((,c (:background ,bg1 :foreground ,blue :bold ,bold))))
     `(helm-ff-file ((,c (:background ,bg1 :foreground ,fg1 :weight normal))))
     `(helm-ff-executable ((,c (:background ,bg1 :foreground ,key2 :weight normal))))
     `(helm-ff-invalid-symlink ((,c (:background ,bg1 :foreground ,warning :bold ,bold))))
     `(helm-resume-need-update ((,c (:background ,alt-red :foreground nil))))
     `(helm-ff-symlink ((,c (:background ,bg1 :foreground ,keyword :bold ,bold))))
     `(helm-ff-prefix ((,c (:background ,keyword :foreground ,bg1 :weight normal))))
     `(helm-grep-cmd-line ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-grep-file ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-grep-finish ((,c (:background ,bg1 :foreground ,fg2))))
     `(helm-grep-lineno ((,c (:background ,bg1 :foreground ,fg1))))
     `(helm-grep-match ((,c (:background nil :foreground nil :inherit helm-match))))
     `(helm-grep-running ((,c (:background ,bg1 :foreground ,functions))))
     `(helm-moccur-buffer ((,c (:background ,bg1 :foreground ,functions))))
     `(helm-source-go-package-godoc-description ((,c (:foreground ,str))))
     `(helm-bookmark-w3m ((,c (:foreground ,type))))

     ;; Company
     `(company-tooltip ((,c (:background ,tooltip-bg :foreground ,fg3 :bold ,bold))))
     `(company-tooltip-common ((,c (:foreground ,light-green))))
     `(company-tooltip-common-selection ((,c (:foreground ,light-orange))))
     `(company-tooltip-selection ((,c (:background ,tooltip-hl :foreground ,light-yellow))))
     `(company-tooltip-annotation ((,c (:foreground ,faded-blue))))
     `(company-scrollbar-bg ((,c (:background ,bg1))))
     `(company-scrollbar-fg ((,c (:foreground ,keyword))))
     `(company-template-field ((,c (:inherit region))))
     `(company-echo-common ((,c (:background ,bg1 :foreground ,light-yellow))))
     `(company-preview ((,c (:background ,bg1 :foreground ,key2))))
     `(company-preview-common ((,c (:foreground ,bg2 :foreground ,fg3))))
     `(company-preview-search ((,c (:background ,bg1 :foreground ,blue))))
     `(company-tooltip-mouse ((,c (:background ,bg3 :foreground ,fg3))))

     ;; Web
     `(css-selector ((,c (:foreground ,teal))))
     `(web-mode-type-face ((,c (:inherit ,font-lock-type-face))))
     `(web-mode-html-tag-face ((,c (:inherit font-lock-keyword-face))))
     `(web-mode-html-tag-bracket-face ((,c (:inherit web-mode-html-tag-face))))
     `(web-mode-html-attr-name-face ((,c (:inherit ,font-lock-function-name-face))))
     `(web-mode-html-attr-value-face ((,c (:inherit ,font-lock-string-face))))
     `(web-mode-builtin-face ((,c (:inherit ,font-lock-builtin-face))))
     `(web-mode-keyword-face ((,c (:foreground ,keyword))))
     `(web-mode-constant-face ((,c (:inherit ,font-lock-constant-face))))
     `(web-mode-comment-face ((,c (:inherit ,font-lock-comment-face))))
     `(web-mode-doctype-face ((,c (:foreground ,purple :bold ,bold))))
     `(web-mode-function-name-face ((,c (:inherit ,font-lock-function-name-face))))
     `(web-mode-string-face ((,c (:foreground ,str))))
     `(web-mode-warning-face ((,c (:inherit ,font-lock-warning-face))))

     ;; Speedbar
     `(speedbar-separator-face ((,c (:background ,blue))))
     `(speedbar-directory-face ((,c (:foreground ,teal))))
     `(speedbar-file-face ((,c (:foreground ,green))))
     `(speedbar-tag-face ((,c (:foreground ,faded-blue))))
     `(speedbar-selected-face ((,c (:foreground ,teal-green))))
     `(speedbar-highlight-face ((,c (:foreground ,hl))))
     `(speedbar-button-face ((,c (:foreground ,jade))))

     ;; Haskell mode
     ;; `(haskell-operator-face ((,c (:foreground ,lime))))
     ;; `(haskell-type-face ((,c (:foreground ,light-yellow))))
     ;; `(haskell-constructor-face ((,c (:foreground ,orange))))

     ;; Perl6
     ;; `(perl6-identifier ((,c (:foreground ,cyan))))
     `(perl6-phaser ((,c (:foreground ,cyan))))
     `(perl6-type-constraint ((,c (:inherit font-lock-keyword-face))))

     ;; Shell
     `(sh-quoted-exec ((,c (:foreground ,light-yellow))))

     ;; Flx
     `(flx-highlight-face ((,c (:foreground ,light-green :underline ,underline))))

     ;; Emmet
     `(emmet-preview-input ((t (:foreground nil :background nil))))
     `(emmet-preview-output ((t (:foreground nil :background nil))))

     ;; Clojure
     `(clojure-keyword-face ((,c (:inherit ,font-lock-variable-name-face))))

     ;; OCaml
     `(tuareg-font-lock-governing-face ((,c (:foreground ,green :bold ,bold))))
     `(tuareg-font-double-colon-face ((,c (:foreground ,yellow))))
     `(tuareg-font-lock-error-face ((,c (:foreground ,alt-red))))
     ;; FIXME: find code with following face
     ;; `(tuareg-font-lock-multistage-face ((,c (:foreground ,alt-red))))

     ;; Nim
     `(nim-font-lock-export-face ((,c (:inherit font-lock-function-name-face :italic nil))))

     ;; Ace-window
     `(aw-leading-char-face ((,c (:foreground ,deep-pink :bold ,bold))))
     `(aw-background-face ((,c (:foreground ,bg4 :bold ,bold))))

     ;; Latex/Auctex
     ;; `(font-latex-bold-face ((,c (:inherit bold))))
     ;; `(font-latex-italic-face ((,c (:inherit italic))))
     `(font-latex-warning-face ((,c (:inherit warning))))

     `(font-latex-string-face ((,c (:inherit font-lock-string-face))))
     `(font-latex-math-face ((,c (:foreground ,violet))))
     `(font-latex-sedate-face ((,c (:foreground ,teal-blue))))
     `(font-latex-script-char-face ((,c (:foreground ,violet))))
     `(font-latex-sectioning-0-face ((,c (:foreground ,wheat :bold ,bold))))
     `(font-latex-sectioning-1-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-2-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-3-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-4-face ((,c (:inherit font-latex-sectioning-0-face))))
     `(font-latex-sectioning-5-face ((,c (:inherit font-latex-sectioning-0-face))))

     ;; which-function-mode
     `(which-func ((,c (:foreground ,orange))))

     ;; Evil ex
     `(evil-ex-info ((,c (:foreground ,orange))))
     `(evil-ex-substitute-matches ((,c (:background nil :foreground ,red :underline ,underline))))
     `(evil-ex-substitute-replacement ((,c (:background nil :foreground ,light-green))))
     '(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))

    ;; Avy
     `(avy-lead-face ((,c (:background ,dark-red :foreground ,white))))
     `(avy-lead-face-0 ((,c (:background ,jade :foreground ,white))))
     `(avy-lead-face-1 ((,c (:background ,dark-blue :foreground ,white))))
     `(avy-lead-face-2 ((,c (:background ,dark-purple :foreground ,white))))

     ;; Ivy & Swiper
     `(ivy-current-match ((,c (:background ,hl-line :foreground ,light-green :bold t))))
     `(ivy-match-required-face ((,c (:background nil :foreground ,alt-red :bold nil))))
     `(ivy-confirm-face ((,c (:background nil :foreground ,teal-green))))
     `(ivy-action ((,c (:background nil :foreground ,teal-green :bold ,bold))))
     `(ivy-minibuffer-match-face-1 ((,c (:background ,bg3 :foreground ,fg1))))
     `(ivy-minibuffer-match-face-2 ((,c (:background ,dark-blue :foreground ,teal-blue :bold ,bold))))
     `(ivy-minibuffer-match-face-3 ((,c (:background ,dark-yellow :foreground ,light-orange :bold ,bold))))
     `(ivy-minibuffer-match-face-4 ((,c (:background ,dark-purple :foreground ,light-purple :bold ,bold))))
     ;; `(ivy-current-match ((,c (:inherit hl-line :foreground ,hl))))
     `(swiper-match-face-1 ((,c (:background ,bg3 :foreground ,fg1))))
     `(swiper-match-face-2 ((,c (:background ,dark-blue :foreground ,teal-blue :bold ,bold))))
     `(swiper-match-face-3 ((,c (:background ,dark-yellow :foreground ,light-orange :bold ,bold))))
     `(swiper-match-face-4 ((,c (:background ,dark-purple :foreground ,light-purple :bold ,bold))))
     `(swiper-line-face ((,c (:inherit hl-line)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kaolin)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; kaolin-theme.el ends here
