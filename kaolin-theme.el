;;; kaolin-theme.el --- A dark jade theme inspired by Sierra.vim

;; Copyright (C) 2017 0rdy

;; Author: 0rdy <mail@0rdy.com>
;; URL: https://github.com/0rdy/kaolin-theme
;; Package-Requires: ((emacs "24"))
;; Version: 0.8.5

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

(defcustom kaolin-wave nil
  "When t, use the wave underline style instead of regular underline."
  :group 'kaolin-theme)

(defface kaolin-boolean nil
  "Face to highlight boolean values"
  :group 'kaolin-theme)

;; TODO: move light-green to separate hl var and (??) chage to lavender
;; Kaolin color palette
(let ((c '((class color) (min-colors 89)))
      (black1          "#1b1b1b")
      (black2          "#282828")
      (black3          "#353535")
      (black4          "#414141")
      (alt-black       "#181818")
      (dark-gray       "#2a2a2a")
      (dim-gray        "#353535")
      (gray            "#545c5e")
      (alt-gray        "#60696b")
      ;; (light-gray      "#859092")
      (light-gray      "#9191a2")
      ;; (white1           "#c5c8c6")
      (white1          "#c8c8d0")
      (white2          "#babac4")
      (white3          "#adadb9")
      (white4          "#9f9fad")


      ;; TODO: (gray-brown     "#a5a19c")
      (brown           "#7d6360")
      (light-brown     "#ae9895")
      (alt-brown       "#52413f")
      ;; TODO: (bazaar       "#98777b")

      (dark-red        "#832729")
      ;; (red             "#d75f5f")
      (red             "#cd5c5c")
      (faded-red       "#a94d53")
      (alt-red         "#c93232")
      (light-red       "#d66e75")
      ;; (pink            "#d75f91")
      (pink            "#d24b83")
      (light-pink      "#ef98aa")
      (soft-pink       "#fbaed2")

      (faded-orange    "#cd9575")
      (alt-orange      "#d9a76f")
      (orange          "#dbac66")
      (light-orange    "#ddc085")
      ;; (pure-orange     "#cc5900")
      (pure-orange     "#cc6a00")

      (dark-yellow     "#555a2f")
      (yellow          "#acb370")
      (alt-yellow      "#be9266")
      (light-yellow    "#c9bb87")
      (wheat           "#b9c791")
      (alt-wheat       "#fdd5b1")

      (dark-jade       "#2e4039")
      (jade            "#597a6e")
      (alt-jade        "#4d5d53")
      (light-jade      "#709688")
      (midnight-green  "#152628")
      (deep-green      "#39656b")
      (green           "#4a858c")
      (dark-green      "#39855f")
      (light-green     "#54b685")
      (lime            "#85b654")
      (alt-lime        "#8fbc8f")
      (teal            "#80b6bc")
      (teal-blue       "#91b9c7")
      ;; (teal-blue      "#91c7c7")
      ;; (teal-green      "#80bea0")
      (teal-green      "#6fb593")

      (midnight-blue   "#1e2528")
      (grayish-blue    "#36454f")
      (dark-blue       "#2a4661")
      ;; TODO: Change blue color
      (blue            "#5077a5")
      (alt-blue        "#267fb5")
      (dark-cyan       "#008b8b")
      (cyan            "#54b6b6")
      (faded-blue      "#817f96")

      (midnight-purple "#1a121a")
      (dark-purple     "#563d56")
      (purple          "#835d83")
      (magenta         "#5454b6")
      (grayish-magenta "#796878")
      (light-purple    "#cea2ca")
      (alt-purple      "#915c83")
      (lavender        "#967bb6")
      (alt-lavender    "#9d81ba")

      (dark-violet     "#997a8d")
      (violet          "#ab98b5")

      ;; Face options
      (bold            kaolin-bold)
      (italic          kaolin-italic)
      (underline       kaolin-underline)
      (underline-style (if kaolin-wave 'wave 'line)))

  ;; Theme colors
  (let* ((fg1  white1)
         (fg2  white2)
         (fg3  white3)
         (fg4  white4)
         (bg1  black1)
         (bg2  black2)
         (bg3  black3)
         (bg4  black4)

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
         (rb9 lavender)

         (diff-add    light-green)
         (diff-change violet)
         (diff-del    red)

         (line-fg           fg4)
         (line-bg1          bg2)
         (line-bg2          dim-gray)
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

         (win-border dark-gray)
         (linum-fg   gray)
         (linum-hl   light-gray)
         (cursor     light-gray)

         (keyword    green)
         (key2       "#5f9298")
         (key3       "#41757b")
         (hl         cyan)
         (hl-indent  gray)
         (builtin    teal)
         (comment    gray)
         ;; Light
         ;; (comment    "#869a90")
         ;; (comment    "#8f9ca7")
         (functions  teal)
         (str        teal-green)
         (str-alt    jade)
         (doc        str-alt)
         ;; TODO: ?? pink
         (type       alt-orange)
         ;; TODO: ?? light-yellow
         (const      violet)
         (var        faded-blue)
         ;; TODO: change number color ??  pink ?? light-yellow ?? alt-orange
         (num        red)
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
     `(link ((,c (:foreground ,lavender :underline ,underline))))
     `(link-visited ((,c (:inherit link :underline nil))))
     `(success ((,c (:background nil :foreground ,light-green))))
     `(escape-glyph ((,c (:background nil :foreground ,cyan))))

     `(menu ((,c (:background ,bg2 :foreground ,fg2))))
     `(header-line ((,c (:background ,bg2 :foreground ,jade))))
     `(tooltip ((,c (:foreground ,tooltip-bg :foreground ,tooltip-fg))))

     `(match ((,c (:background nil :foreground ,cyan))))
     `(isearch ((,c (:background nil :foreground ,light-green :bold ,bold :underline ,underline))))
     `(isearch-fail ((,c (:background nil :foreground ,red))))

     ;; Interface
     ;; TODO: change button face
     `(button ((,c (:background ,bg4 :foreground ,teal :box (:line-width 3 :color ,bg3 :style nil)))))
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
     `(widget-button-pressed ((,c (:background nil :foreground ,faded-red))))
     `(widget-field ((,c (:background ,bg3 :foreground ,fg1 :box (:line-width 1 :color ,bg2 :style nil)))))
     `(widget-documentation ((,c (:background nil :foreground ,faded-blue))))

     `(package-name ((,c (:background nil :foreground ,cyan))))

     ;; Additional highlighting
     `(highlight ((,c (:background ,bg2 :foreground ,hl))))
     `(lazy-highlight ((,c (:background ,bg3 :foreground ,fg2))))
     `(hl-line ((,c (:background ,bg2))))
     `(highlight-numbers-number ((,c (:foreground ,num))))
     `(highlight-quoted-quote ((t (:foreground ,teal))))
     `(highlight-quoted-symbol ((t (:foreground ,green))))

     ;; Eldoc
     `(eldoc-highlight-function-argument ((t (:foreground ,violet))))

     ;; Highlight indent guides
     `(highlight-indent-guides-odd-face  ((t (:background ,hl-indent))))
     `(highlight-indent-guides-even-face  ((t (:background ,hl-indent))))
     `(highlight-indent-guides-character-face  ((t (:foreground ,hl-indent))))

     ;; Highlighting indentation
     `(highlight-indentation-face  ((t (:background ,bg2))))
     `(highlight-indentation-current-column-face  ((t (:background ,bg3))))

     ;; Linum & nlinum
     `(linum ((t (:background ,bg1 :foreground ,linum-fg :bold nil))))
     `(linum-highlight-face ((t (:inherit linum))))
     `(linum-relative-current-line ((t (:background ,bg1 :foreground ,linum-hl :bold ,bold))))
     `(nlinum-current-line ((t (:background ,bg1 :foreground ,linum-hl :bold ,bold))))
     `(nlinum-relative-current-face ((t (:background ,bg1 :foreground ,linum-hl :bold ,bold))))

     ;; Auto-dim-other-buffers
     `(auto-dim-other-buffers-face  ((t (:background ,dim-buffer))))

     ;; Fic-mode
     `(fic-face  ((t (:background nil :foreground ,red :bold ,bold))))
     `(fic-author-face  ((t (:background nil :foreground ,red :bold ,bold))))

     ;; Modeline
     ;; `(mode-line ((,c (:box (:line-width 1 :color ,line-border) :bold ,bold :background ,line-bg1 :foreground ,line-fg))))
     `(mode-line ((,c (:box (:line-width 2 :color ,line-bg2) :background ,line-bg1 :foreground ,faded-blue :bold ,bold))))
     `(mode-line-buffer-id ((,c (:background nil :foreground ,teal :bold ,bold))))
     `(mode-line-highlight ((,c (:foreground ,keyword :box nil :bold ,bold))))
     ;; `(mode-line-inactive ((,c (:box (:line-width 1 :color ,bg2 :style pressed-button) :background ,bg2 :foreground ,light-gray :weight normal))))
     `(mode-line-inactive ((,c (:box (:line-width 2 :color ,line-bg1) :background ,line-bg1 :foreground ,light-gray :bold ,bold))))
     `(mode-line-emphasis ((,c (:foreground ,fg1))))

     ;; Telephone-line
     `(telephone-line-accent-active ((t (:inherit mode-line :background ,line-bg2 :foreground ,line-fg))))
     `(telephone-line-accent-inactive ((t (:background ,line-bg1 :foreground ,light-gray :inherit mode-line-inactive))))
     `(telephone-line-evil ((t (:inherit mode-line))))
     `(telephone-line-evil-normal ((t (:background ,line-bg2 :foreground ,evil-normal :inherit telephone-line-evil))))
     `(telephone-line-evil-insert ((t (:background ,line-bg2 :foreground ,evil-insert :inherit telephone-line-evil))))
     `(telephone-line-evil-visual ((t (:background ,line-bg2 :foreground ,evil-visual :inherit telephone-line-evil))))
     `(telephone-line-evil-replace ((t (:background ,line-bg2 :foreground ,evil-replace :inherit telephone-line-evil))))
     `(telephone-line-evil-motion ((t (:background ,line-bg2 :foreground ,evil-motion :inherit telephone-line-evil))))
     `(telephone-line-evil-operator ((t (:background ,line-bg2 :foreground ,evil-operator :inherit telephone-line-evil))))
     `(telephone-line-evil-emacs ((t (:background ,line-bg2 :foreground ,evil-emacs :inherit telephone-line-evil))))

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
     `(flycheck-warning ((,c (:underline (:style ,underline-style :color ,warning)))))
     `(flycheck-error ((,c (:underline (:style ,underline-style :color ,err)))))
     `(flycheck-fringe-error ((,c (:foreground ,err))))
     `(flycheck-fringe-warning ((,c (:foreground ,warning))))
     `(flycheck-fringe-info ((,c (:foreground ,teal-blue))))

     ;; Flyspell
     `(flyspell-duplicate ((,c (:underline (:style ,underline-style :color ,warning)))))
     `(flyspell-incorrect ((,c (:underline (:style ,underline-style :color ,err)))))

     ;; Hydra
     `(hydra-face-red ((,c (:foreground ,red))))
     `(hydra-face-teal ((,c (:foreground ,teal))))
     `(hydra-face-blue ((,c (:foreground ,blue))))
     `(hydra-face-pink ((,c (:foreground ,pink))))
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
     `(org-link ((,c (:inherit link))))
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
     `(ido-indicator ((,c (:foreground ,num))))
     `(ido-first-match ((,c (:foreground ,light-green :bold ,bold))))
     `(ido-only-match ((,c (:foreground ,hl))))
     `(ido-subdir ((,c (:foreground ,lavender))))

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
     `(undo-tree-visualizer-active-branch-face ((,c :foreground ,fg1 :bold ,bold)))
     `(undo-tree-visualizer-current-face ((,c :foreground ,cyan)))
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
     `(rainbow-delimiters-depth-9-face ((,c :foreground ,rb9)))

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
     `(eshell-ls-clutter ((t (:foreground ,pink))))
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
     ;; TODO:
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
     `(aw-leading-char-face ((,c (:foreground ,pink :bold ,bold))))
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

     ;; Rst-mode
     `(rst-adornment ((,c (:foreground ,jade))))
     `(rst-block ((,c (:foreground ,teal))))
     `(rst-level-1 ((,c (:foreground ,violet))))
     `(rst-level-2 ((,c (:foreground ,green))))
     `(rst-level-3 ((,c (:foreground ,teal-blue))))
     `(rst-level-4 ((,c (:foreground ,violet))))
     `(rst-level-5 ((,c (:foreground ,green))))
     `(rst-level-6 ((,c (:foreground ,teal-blue))))

     ;; Pulse
     `(pulse-highlight-start-face ((,c (:background ,dark-yellow))))

     ;; Which-function-mode
     `(which-func ((,c (:foreground ,orange))))

     ;; Which-key
     `(which-key-key-face ((,c (:foreground ,purple :bold ,bold))))
     `(which-key-group-description-face ((,c (:foreground ,light-purple))))
     `(which-key-local-map-description-face ((,c (:foreground ,teal-green))))
     `(which-key-command-description-face ((,c (:foreground ,teal))))

     ;; Ruler-mode
     `(ruler-mode-default ((,c (:background ,bg2 :foreground ,gray))))
     `(ruler-mode-column-number ((,c (:foreground ,faded-blue))))
     `(ruler-mode-current-column ((,c (:foreground ,orange))))
     `(ruler-mode-fill-column ((,c (:foreground ,pink))))
     `(ruler-mode-comment-column ((,c (:foreground ,teal-blue))))
     `(ruler-mode-fringes ((,c (:foreground ,green))))
     `(ruler-mode-pad ((,c (:foreground ,faded-blue))))
     `(ruler-mode-tab-stop ((,c (:foreground ,violet))))
     `(ruler-mode-goal-column ((,c (:foreground ,alt-red))))

     ;; TODO: Message
     `(message-header-name ((,c (:foreground ,deep-green))))
     `(message-header-subject ((,c (:foreground ,teal-green))))
     `(message-header-to ((,c (:foreground ,teal-green))))
     `(message-header-other ((,c (:foreground ,teal))))

     ;; Elfeed
     `(elfeed-search-tag-face ((,c (:foreground ,light-yellow))))
     `(elfeed-search-feed-face ((,c (:foreground ,green))))
     `(elfeed-search-date-face ((,c (:foreground ,faded-blue))))
     `(elfeed-search-unread-title-face ((,c (:foreground ,fg1))))
     `(elfeed-search-unread-count-face ((,c (:foreground ,orange))))
     `(elfeed-search-title-face ((,c (:foreground ,comment))))

     ;; Evil ex
     `(evil-ex-info ((,c (:foreground ,orange))))
     `(evil-ex-substitute-matches ((,c (:background nil :foreground ,red :underline ,underline))))
     `(evil-ex-substitute-replacement ((,c (:background nil :foreground ,light-green))))
     `(evil-ex-lazy-highlight ((t (:inherit lazy-highlight))))

     ;; Vimish-fold
     `(vimish-fold-overlay ((,c (:background ,bg2 :foreground ,comment))))
     `(vimish-fold-fringe ((,c (:background nil :foreground ,jade))))

    ;; Avy
     `(avy-lead-face ((,c (:background ,dark-red :foreground ,fg1))))
     `(avy-lead-face-0 ((,c (:background ,jade :foreground ,fg1))))
     `(avy-lead-face-1 ((,c (:background ,dark-blue :foreground ,fg1))))
     `(avy-lead-face-2 ((,c (:background ,dark-purple :foreground ,fg1))))

     ;; Ivy & Swiper
     `(ivy-modified-buffer ((,c (:foreground ,light-yellow))))
     `(ivy-subdir ((,c (:foreground ,green :bold ,bold))))
     `(ivy-virtual ((,c (:foreground ,violet))))
     `(ivy-remote ((,c (:foreground ,teal))))
     `(ivy-current-match ((,c (:background ,hl-line :foreground ,light-green :bold t))))
     `(ivy-match-required-face ((,c (:background nil :foreground ,alt-red :bold nil))))
     `(ivy-confirm-face ((,c (:background nil :foreground ,light-orange))))
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
