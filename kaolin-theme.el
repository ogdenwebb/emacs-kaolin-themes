;;; kaolin-theme.el --- A dark jade Emacs theme inspired by Sierra.vim

;; Copyright (C) 2017 0rdy

;; Author: 0rdy <mail@mail>
;; URL:
;; Version: 0.1


;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(deftheme kaolin "Kaolin color theme")

;; Kaolin color palette
(let ((class '((class color) (min-colors 89)))
      (black        "#1b1b1b")
      (dark-gray    "#2a2a2a")
      (dim          "#353535")
      (gray         "#545c5e")
      (light-gray   "#788486")
      (white        "#c5c8c6")
      (brown        "#7d6360")
      (red          "#d75f5f")
      (alt-red      "#c93232")
      (orange       "#d2ab5d")
      (yellow       "#acb370")
      (light-yellow "#c1b175")
      (deep-green   "#30555a")
      (green        "#4a858c")
      (light-green  "#54b685")
      (lime         "#85b654")
      (teal         "#80b6bc")
      (teal-blue    "#91b9c7")
      (blue         "#5485b6")
      (cyan         "#54b6b6")
      (deep-blue    "#857f96")
      (purple       "#835d83")
      (alt-purple   "#8c4a64"))

  ;; Theme colors
  (let* ((fg1  white)
         (fg2  "#b4b6b4")
         (fg3  "#a2a5a3")
         (fg4  "#929492")
         (bg1   black)
         (bg2  "#2a2a2a")
         (bg3  "#3a3a3a")
         (bg4  "#4a4a4a")
         (key2 "#5f9298")
         (key3 "#43757c")

         (rb1 blue)
         (rb2 cyan)
         (rb3 lime)
         (rb4 orange)
         (rb5 teal)
         (rb6 white)
         (rb7 green)
         (rb8 red)

         (line-fg           fg4)
         (line-bg           bg2)
         (line-border       bg3)
         (segment-active    gray)
         (segment-inactive  gray)
         (evil-normal       lime)
         (evil-insert       light-green)
         (evil-visual       orange)
         (evil-replace      red)
         (evil-motion       yellow)
         (evil-operator     evil-normal)
         (evil-emacs        purple)

         (cursor     light-gray)
         (keyword    green)
         (hl         cyan)
         (hl-indent  gray)
         (builtin    teal)
         (const      deep-blue)
         (comment    gray)
         (win-border dark-gray)
         (functions  teal)
         (str        teal-blue)
         (str-alt    light-yellow)
         (doc        str-alt)
         (type       orange)
         (var        deep-blue)
         (num        red)
         (warning    red))

    (custom-theme-set-faces
     'kaolin
     ;; Font-lock
     `(font-lock-builtin-face ((,class (:foreground ,builtin))))
     `(font-lock-comment-face ((,class (:foreground ,comment))))
     `(font-lock-constant-face ((,class (:foreground ,const))))
     `(font-lock-reference-face ((,class (:foreground ,const))))
     `(font-lock-string-face ((,class (:foreground ,str))))
     `(font-lock-doc-face ((,class (:foreground ,doc))))
     `(font-lock-function-name-face ((,class (:foreground ,functions :bold t))))
     `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
     `(font-lock-negation-char-face ((,class (:foreground ,const))))
     `(font-lock-type-face ((,class (:foreground ,type))))
     `(font-lock-variable-name-face ((,class (:foreground ,var))))
     `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))

     ;; General
     `(default ((,class (:background ,bg1 :foreground ,fg1))))
     `(warning ((,class (:foreground ,warning))))
     `(region ((,class (:background ,bg3))))
     `(fringe ((,class (:background ,bg1 :foreground ,fg1))))
     `(cursor ((,class (:background ,cursor))))
     `(isearch ((,class (:bold t :foreground ,bg3 :background ,hl))))
     `(vertical-border ((,class (:foreground ,win-border))))
     `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
     `(default-italic ((,class (:italic t))))
     `(link ((,class (:foreground ,const :underline t))))
     `(success ((,class (:foreground ,yellow :background ,bg1))))

     ;; Highlight
     `(highlight ((,class (:background ,bg2))))
     `(lazy-highlight ((,class (:foreground ,fg2 :background ,bg3))))
     `(hl-line ((,class (:inherit highlight))))
     `(highlight-numbers-number ((,class (:foreground ,num))))
     `(highlight-quoted-quote ((t (:foreground ,teal)))) ; Face to highlight Lisp quotes
     `(highlight-quoted-symbol ((t (:foreground ,green)))) ; Face to highlight quoted Lisp symbols

     ;; Highlight indent guides
     `(highlight-indent-guides-odd-face  ((t (:background ,hl-indent))))
     `(highlight-indent-guides-even-face  ((t (:background ,hl-indent))))
     `(highlight-indent-guides-character-face  ((t (:foreground ,hl-indent))))

     ;; Linum-mode & nlinum
     `(linum ((t (:background ,bg1 :foreground ,gray))))
     `(nlinum-current-line ((t (:background ,bg1 :foreground ,green))))

     ;; Modeline
     ;; `(mode-line ((,class (:box (:line-width 1 :color ,line-border) :bold t :foreground ,line-fg :background ,line-bg))))
     `(mode-line ((,class (:box (:line-width 2 :color ,line-bg) :bold t :foreground ,deep-blue :background ,line-bg))))
     `(mode-line-buffer-id ((,class (:bold t :foreground ,teal :background nil))))
     `(mode-line-highlight ((,class (:foreground ,keyword :box nil :weight bold))))
     ;; `(mode-line-inactive ((,class (:box (:line-width 1 :color ,bg2 :style pressed-button) :foreground ,light-gray :background ,bg2 :weight normal))))
     `(mode-line-inactive ((,class (:box (:line-width 1 :color ,line-bg) :foreground ,light-gray :background ,line-bg :weight normal))))
     `(mode-line-emphasis ((,class (:foreground ,fg1))))

     ;; Telephone-line
     `(telephone-line-accent-active ((t (:foreground ,line-fg :background ,dim :inherit mode-line))))
     `(telephone-line-accent-inactive ((t (:foreground ,light-gray :background ,line-bg :inherit mode-line-inactive))))
     `(telephone-line-evil ((t (:inherit modeline))))
     `(telephone-line-evil-normal ((t (:foreground ,evil-normal :background ,dim :inherit telephone-line-evil))))
     `(telephone-line-evil-insert ((t (:foreground ,evil-insert :background ,dim :inherit telephone-line-evil))))
     `(telephone-line-evil-visual ((t (:foreground ,evil-visual :background ,dim :inherit telephone-line-evil))))
     `(telephone-line-evil-replace ((t (:foreground ,evil-replace :background ,dim :inherit telephone-line-evil))))
     `(telephone-line-evil-motion ((t (:foreground ,evil-motion :background ,dim :inherit telephone-line-evil))))
     `(telephone-line-evil-operator ((t (:foreground ,evil-operator :background ,dim :inherit telephone-line-evil))))
     `(telephone-line-evil-emacs ((t (:foreground ,evil-emacs :background ,dim :inherit telephone-line-evil))))

     ;; Org-mode
     `(org-level-1 ((,class (:bold t :foreground ,fg2 :height 1.1))))
     `(org-level-2 ((,class (:bold nil :foreground ,fg3))))
     `(org-level-3 ((,class (:bold t :foreground ,fg4))))
     `(org-level-4 ((,class (:bold nil :foreground ,bg4))))
     `(org-code ((,class (:foreground ,fg2))))
     `(org-hide ((,class (:foreground ,fg4))))
     `(org-date ((,class (:underline t :foreground ,var))))
     `(org-footnote  ((,class (:underline t :foreground ,fg4))))
     `(org-link ((,class (:underline t :foreground ,type))))
     `(org-special-keyword ((,class (:foreground ,functions))))
     `(org-block ((,class (:foreground ,fg3))))
     `(org-quote ((,class (:inherit org-block :slant italic))))
     `(org-verse ((,class (:inherit org-block :slant italic))))
     `(org-todo ((,class (:box (:line-width 1 :color ,fg3) :foreground ,keyword :bold t))))
     `(org-done ((,class (:box (:line-width 1 :color ,bg3) :bold t :foreground ,bg4))))
     `(org-warning ((,class (:underline t :foreground ,warning))))
     `(org-agenda-structure ((,class (:weight bold :foreground ,fg3 :box (:color ,fg4) :background ,bg3))))
     `(org-agenda-date ((,class (:foreground ,var :height 1.1))))
     `(org-agenda-date-weekend ((,class (:weight normal :foreground ,fg4))))
     `(org-agenda-date-today ((,class (:weight bold :foreground ,keyword :height 1.4))))
     `(org-agenda-done ((,class (:foreground ,bg4))))
     `(org-scheduled ((,class (:foreground ,type))))
     `(org-scheduled-today ((,class (:foreground ,functions :weight bold :height 1.2))))
     `(org-ellipsis ((,class (:foreground ,builtin))))
     `(org-verbatim ((,class (:foreground ,fg4))))
     `(org-document-info-keyword ((,class (:foreground ,functions))))
     `(org-sexp-date ((,class (:foreground ,fg4))))

     ;; Latex
     `(font-latex-bold-face ((,class (:foreground ,type))))
     `(font-latex-italic-face ((,class (:foreground ,key3 :italic t))))
     `(font-latex-string-face ((,class (:foreground ,str))))
     `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
     `(font-latex-match-variable-keywords ((,class (:foreground ,var))))

     `(ido-only-match ((,class (:foreground ,warning))))
     `(ido-first-match ((,class (:foreground ,keyword :bold t))))

     `(gnus-header-content ((,class (:foreground ,keyword))))
     `(gnus-header-from ((,class (:foreground ,var))))
     `(gnus-header-name ((,class (:foreground ,type))))
     `(gnus-header-subject ((,class (:foreground ,functions :bold t))))

     `(mu4e-view-url-number-face ((,class (:foreground ,type))))
     `(mu4e-cited-1-face ((,class (:foreground ,fg2))))
     `(mu4e-cited-7-face ((,class (:foreground ,fg3))))
     `(mu4e-header-marks-face ((,class (:foreground ,type))))

     `(ffap ((,class (:foreground ,fg4))))

     ;; Js-mode
     `(js2-private-function-call ((,class (:foreground ,const))))
     `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
     `(js2-jsdoc-html-tag-name ((,class (:foreground ,key2))))
     `(js2-external-variable ((,class (:foreground ,type))))
     `(js2-function-param ((,class (:foreground ,const))))
     `(js2-function-call ((,class (:foreground ,yellow))))
     `(js2-jsdoc-value ((,class (:foreground ,str))))
     `(js2-private-member ((,class (:foreground ,fg3))))
     `(js3-warning-face ((,class (:underline ,keyword))))
     `(js3-error-face ((,class (:underline ,warning))))
     `(js3-external-variable-face ((,class (:foreground ,var))))
     `(js3-function-param-face ((,class (:foreground ,key3))))
     `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
     `(js3-instance-member-face ((,class (:foreground ,const))))

     `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
     `(info-quoted-name ((,class (:foreground ,builtin))))
     `(info-string ((,class (:foreground ,str))))
     `(icompletep-determined ((,class :foreground ,builtin)))

     ;; Undo-tree
     `(undo-tree-visualizer-current-face ((,class :foreground ,builtin)))
     `(undo-tree-visualizer-default-face ((,class :foreground ,fg2)))
     `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
     `(undo-tree-visualizer-register-face ((,class :foreground ,type)))

     `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
     `(trailing-whitespace ((,class :foreground nil :background ,warning)))

     ;; Rainbow delimeters
     `(show-paren-match-face ((,class (:foreground ,cyan :background ,bg3))))
     `(rainbow-delimiters-depth-1-face ((,class (:foreground ,rb1))))
     `(rainbow-delimiters-depth-2-face ((,class :foreground ,rb2)))
     `(rainbow-delimiters-depth-3-face ((,class :foreground ,rb3)))
     `(rainbow-delimiters-depth-4-face ((,class :foreground ,rb4)))
     `(rainbow-delimiters-depth-5-face ((,class :foreground ,rb5)))
     `(rainbow-delimiters-depth-6-face ((,class :foreground ,rb6)))
     `(rainbow-delimiters-depth-7-face ((,class :foreground ,rb7)))
     `(rainbow-delimiters-depth-8-face ((,class :foreground ,rb8)))
     `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))

     ;; Magit
     `(magit-item-highlight ((,class :background ,bg3)))
     `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
     `(magit-hunk-heading           ((,class (:background ,bg3))))
     `(magit-section-highlight      ((,class (:background ,bg2))))
     `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
     `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
     `(magit-diffstat-added   ((,class (:foreground ,type))))
     `(magit-diffstat-removed ((,class (:foreground ,var))))
     `(magit-process-ok ((,class (:foreground ,functions :weight bold))))
     `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
     `(magit-branch ((,class (:foreground ,const :weight bold))))
     `(magit-log-author ((,class (:foreground ,fg3))))
     `(magit-hash ((,class (:foreground ,fg2))))
     `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))

     ;; Terminal
     `(term ((,class (:foreground ,fg1 :background ,bg1))))
     `(term-color-black ((,class (:foreground ,bg3 :background ,bg3))))
     `(term-color-blue ((,class (:foreground ,functions :background ,functions))))
     `(term-color-red ((,class (:foreground ,keyword :background ,bg3))))
     `(term-color-green ((,class (:foreground ,type :background ,bg3))))
     `(term-color-yellow ((,class (:foreground ,var :background ,var))))
     `(term-color-magenta ((,class (:foreground ,builtin :background ,builtin))))
     `(term-color-cyan ((,class (:foreground ,str :background ,str))))
     `(term-color-white ((,class (:foreground ,fg2 :background ,fg2))))

     ;; Helm
     `(helm-header ((,class (:foreground ,fg2 :background ,bg1 :underline nil :box nil))))
     `(helm-source-header ((,class (:foreground ,keyword :background ,bg1 :underline nil :weight bold))))
     `(helm-match ((,class (:inherit default :bold t :foreground ,orange))))
     `(helm-selection ((,class (:inherit hl-line))))
     `(helm-selection-line ((,class (:background ,bg2))))
     `(helm-visible-mark ((,class (:foreground ,bg1 :background ,bg3))))
     `(helm-candidate-number ((,class (:foreground ,light-yellow))))
     `(helm-separator ((,class (:foreground ,type :background ,bg1))))
     `(helm-time-zone-current ((,class (:foreground ,builtin :background ,bg1))))
     `(helm-time-zone-home ((,class (:foreground ,type :background ,bg1))))
     `(helm-buffer-not-saved ((,class (:foreground ,type :background ,bg1))))
     `(helm-buffer-process ((,class (:foreground ,builtin :background ,bg1))))
     `(helm-buffer-saved-out ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-buffer-size ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-ff-directory ((,class (:foreground ,functions :background ,bg1 :weight bold))))
     `(helm-ff-file ((,class (:foreground ,fg1 :background ,bg1 :weight normal))))
     `(helm-ff-executable ((,class (:foreground ,key2 :background ,bg1 :weight normal))))
     `(helm-ff-invalid-symlink ((,class (:foreground ,key3 :background ,bg1 :weight bold))))
     `(helm-ff-symlink ((,class (:foreground ,keyword :background ,bg1 :weight bold))))
     `(helm-ff-prefix ((,class (:foreground ,bg1 :background ,keyword :weight normal))))
     `(helm-grep-cmd-line ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-grep-file ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-grep-finish ((,class (:foreground ,fg2 :background ,bg1))))
     `(helm-grep-lineno ((,class (:foreground ,fg1 :background ,bg1))))
     `(helm-grep-match ((,class (:foreground nil :background nil :inherit helm-match))))
     `(helm-grep-running ((,class (:foreground ,functions :background ,bg1))))
     `(helm-moccur-buffer ((,class (:foreground ,functions :background ,bg1))))
     `(helm-source-go-package-godoc-description ((,class (:foreground ,str))))
     `(helm-bookmark-w3m ((,class (:foreground ,type))))

     ;; Company
     `(company-tooltip ((,class (:foreground ,fg2 :background ,bg1 :bold t))))
     `(company-tooltip-common ((,class ( :foreground ,fg3))))
     `(company-tooltip-common-selection ((,class (:foreground ,str))))
     `(company-tooltip-selection ((,class (:background ,bg3 :foreground ,fg3))))
     `(company-tooltop-annotation ((,class (:foreground ,const))))
     `(company-scrollbar-bg ((,class (:background ,bg1))))
     `(company-scrollbar-fg ((,class (:foreground ,keyword))))
     `(company-template-field ((,class (:inherit region))))
     `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
     `(company-preview ((,class (:background ,bg1 :foreground ,key2))))
     `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
     `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
     `(company-tooltip-mouse ((,class (:foreground ,fg3 :background ,bg3))))

     ;; Web
     `(css-selector ((,class (:foreground ,teal))))
     `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
     `(web-mode-html-attr-name-face ((,class (:foreground ,functions))))
     `(web-mode-html-attr-value-face ((,class (:foreground ,keyword))))
     `(web-mode-html-tag-face ((,class (:foreground ,builtin))))
     `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
     `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
     `(web-mode-keyword-face ((,class (:foreground ,keyword))))
     `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
     `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
     `(web-mode-string-face ((,class (:foreground ,str))))
     `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))

     ;; Haskell modej
     ;; `(haskell-operator-face ((,class (:foreground ,lime))))
     ;; `(haskell-type-face ((,class (:foreground ,light-yellow))))
     ;; `(haskell-constructor-face ((,class (:foreground ,orange))))

     ;; Perl6
     ;; `(perl6-identifier ((,class (:foreground ,cyan))))

     ;; Evil ex
     `(evil-ex-info ((,class (:foreground ,orange))))

     ;; Ivy & Swiper
     `(ivy-current-match ((,class (:foreground ,bg1 :background ,hl))))
     ;; `(ivy-current-match ((,class (:inherit hl-line :foreground ,hl))))
     `(swiper-match-face-1 ((,class (:foreground ,fg1 :background ,bg3))))
     `(swiper-match-face-2 ((,class (:foreground ,bg1 :background ,light-yellow))))
     `(swiper-match-face-3 ((,class (:foreground ,bg1 :background ,blue))))
     `(swiper-match-face-4 ((,class (:foreground ,bg1 :background ,alt-purple))))
     `(swiper-line-face ((,class (:inherit hl-line)))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'kaolin)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; kaolin-theme.el ends here
