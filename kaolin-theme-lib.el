;;; kaolin-theme-lib.el --- Kaolin-theme library

;; Predefined Kaolin palette
;; TODO: Add extra colors per class
(defconst kaolin-palette
  '((black1          "#1b1b1b")
    (black2          "#282828")
    (black3          "#353535")
    (black4          "#414141")
    (alt-black       "#181818")

    (dark-gray       "#2a2a2a")
    (dim-gray        "#353535")
    (gray            "#545c5e")
    (alt-gray        "#60696b")
    (light-gray      "#9191a2")
    (lavender-gray   "#b6b5c5")
    (grayish-orange  "#a5a19c")

    (white1          "#c8c8d0")
    (white2          "#babac4")
    (white3          "#adadb9")
    (white4          "#9f9fad")
    (alt-white       "#e7dfdf")

    (brown           "#7d6360")
    (light-brown     "#ae9895")
    (alt-brown       "#52413f")
    (bazaar          "#98777b")

    (dark-red        "#832729")
    (red             "#cd5c5c")
    (faded-red       "#a94d53")
    (alt-red         "#c93232")
    (light-red       "#d66e75")
    (moderate-pink   "#a0586c")
    (pink            "#d24b83")
    (light-pink      "#ef98aa")
    (soft-pink       "#fbaed2")

    (faded-orange    "#cd9575")
    (alt-orange      "#d9a76f")
    (orange          "#dbac66")
    (light-orange    "#ddc085")
    (pure-orange     "#cc6a00")

    (dark-yellow     "#555a2f")
    (yellow          "#acb370")
    (alt-yellow      "#be9266")
    (light-yellow    "#c9bb87")
    (wheat           "#b9c791")
    (alt-wheat       "#fdd5b1")
    (faded-wheat     "#D9CA9B")

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
    (teal-green      "#6fb593")


    (midnight-blue    "#1e2528")
    (alt-midnigh-blue  "#062732")
    (grayish-blue      "#687184")
    (alt-grayish-blue  "#8f9ca7")
    (dark-blue         "#2a4661")
    ;; TODO: swap blue and soft-blue with ranemd to dark-blue
    (blue            "#3B6FA3")
    (alt-blue        "#267fb5")
    (moderate-blue   "#4e7f95")
    (soft-blue       "#4CA6E8")
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
    (alt-violet      "#af94f5")
    (light-violet    "#d1aef4")

    ;; Named face options
    (bold            kaolin-bold)
    (italic          kaolin-italic)
    (underline       kaolin-underline)
    (underline-style (if kaolin-wave 'wave 'line))

    (fg1  white1)
    (fg2  white2)
    (fg3  white3)
    (fg4  white4)
    (bg1  black1)
    (bg2  black2)
    (bg3  black3)
    (bg4  black4)

    (dim-buffer alt-black)
    (hl         light-green)
    ;; TODO: (??) change to (alt)-midnight-green
    (hl-line    (if kaolin-hl-line-colored midnight-blue bg2))
    (hl-indent  gray)

    (tooltip-bg bg2)
    (tooltip-fg light-gray)
    (tooltip-hl-bg alt-brown)
    (tooltip-hl-fg light-orange)

    ;; TODO: (??) repeat 1-4, 2-5... with +2
    (rb1 teal)
    (rb2 violet)
    (rb3 jade)
    (rb4 faded-blue)
    (rb5 green)
    (rb6 light-violet)
    (rb7 grayish-orange)
    (rb8 grayish-magenta)
    (rb9 lavender)

    (diff-add    light-green)
    (diff-change violet)
    ;; TODO: rename to removed?
    (diff-del    red)

    ;; Mode-line
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

    (win-border    dark-gray)
    (line-num-fg   gray)
    (line-num-hl   light-gray)
    (cursor        alt-white)

    ;; TODO: delete these key2 and key3
    (keyword     green)
    (builtin     teal)
    (comment     gray)
    ;; TODO: add var/var with style(default, bright and colored)
    (alt-comment alt-grayish-blue)
    (functions   teal)
    (str         teal-green)
    (str-alt     jade)
    (doc         str-alt)
    ;; TODO: ?? pink
    (type        alt-orange)
    ;; TODO: ?? light-yellow
    (const       violet)
    ;; TODO: make more brighter or change
    (var         faded-blue)
    ;; TODO: change number color ??  pink ?? light-yellow ?? alt-orange
    (num         red)
    (bool        num)
    (prep        lavender)
    (warning     orange)
    (err         red)

    ;; TODO: add helm and ivy additional

    (swiper-bg   bg2)
    (ivy-bg      nil)
    (ivy1        fg1)
    (ivy2        soft-blue)
    (ivy3        light-orange)
    (ivy4        moderate-pink)))


;; Predefined Kaolin face specifications
(defconst kaolin-faces
  '(
    (default (:background bg1 :foreground fg1))
    (button  (:underline t :weight 'bold :foreground black1))
    (font-lock-comment-face   (:foreground comment))
    (error   (:foreground red))
    ;; Font-lock
    (font-lock-builtin-face (:foreground builtin))
    (font-lock-comment-delimiter-face (:foreground comment))
    (font-lock-comment-face (:foreground comment))
    (font-lock-constant-face (:foreground const))
    (font-lock-doc-face (:foreground doc))
    (font-lock-function-name-face (:foreground functions :bold bold))
    (font-lock-keyword-face (:foreground keyword :bold bold))
    (font-lock-negation-char-face (:foreground red))
    (font-lock-preprocessor-face (:foreground prep :bold nil))
    (font-lock-reference-face (:foreground const))
    (font-lock-string-face (:foreground str))
    (font-lock-type-face (:foreground type))
    (font-lock-variable-name-face (:foreground var))
    (font-lock-warning-face (:background nil :foreground warning))

    ;; Kaolin faces
    (kaolin-boolean (:foreground bool))

    ;; General
    (default (:background bg1 :foreground fg1))
    (warning (:foreground warning))
    (error (:foreground err))
    (shadow (:foreground alt-gray))
    (file-name-shadow (:inherit 'shadow))
    (region (:background bg3))
    (secondary-selection (:background dark-jade))
    (fringe (:background bg1 :foreground fg1))
    (cursor (:background cursor))
    (vertical-border (:foreground win-border))
    (minibuffer-prompt (:foreground keyword :bold bold))
    (bold (:bold bold))
    (default-italic (:italic italic))
    (link (:foreground faded-orange :underline underline))
    (link-visited (:inherit 'link :underline nil))
    (success (:background nil :foreground light-green))
    (escape-glyph (:background nil :foreground cyan))

    (menu (:background bg2 :foreground fg2))
    (header-line (:background midnight-blue :foreground faded-blue))
    (tooltip (:foreground tooltip-bg :foreground tooltip-fg))

    (match (:background nil :foreground hl))
    (isearch (:background nil :foreground hl :bold bold :underline underline))
    (isearch-fail (:background nil :foreground red))

    (package-name (:inherit 'link :underline nil))

    ;; Highlighting
    (highlight (:background bg2 :foreground light-orange))
    (lazy-highlight (:background bg3 :foreground fg2))
    (hl-line (:background hl-line))
    (highlight-numbers-number (:foreground num))
    (highlight-quoted-quote (:foreground teal))
    (highlight-quoted-symbol (:foreground green))

    ;; TODO:
    ;; Highlight indent guides
    (highlight-indent-guides-odd-face  (:background hl-indent))
    (highlight-indent-guides-even-face  (:background hl-indent))
    (highlight-indent-guides-character-face  (:foreground hl-indent))

    ;; Highlighting indentation
    (highlight-indentation-face  (:background bg2))
    (highlight-indentation-current-column-face  (:background bg3))

    ;; Pulse
    ;; TODO: split
    ;; TODO: use it in evil-goggles
    (pulse-highlight-start-face (:background dark-yellow))

    ;; Auto-dim-other-buffers
    (auto-dim-other-buffers-face  (:background dim-buffer))

    ;; Linum & nlinum
    (linum                        (:background bg1 :foreground line-num-fg :bold nil))
    (linum-highlight-face         (:inherit 'linum))
    (linum-relative-current-line  (:background bg1 :foreground line-num-hl :bold bold))
    (nlinum-current-line          (:background bg1 :foreground line-num-hl :bold bold))
    (nlinum-relative-current-face (:background bg1 :foreground line-num-hl :bold bold))

    ;; Which-function-mode
    ;; TODO: split
    (which-func (:foreground orange))

    ;; Which-key
    ;; TODO: split
    (which-key-key-face (:foreground light-green :bold bold))
    (which-key-group-description-face (:foreground alt-lavender))
    (which-key-local-map-description-face (:foreground soft-blue))
    (which-key-command-description-face (:foreground teal))

    ;; Ruler-mode
    ;; TODO: split
    (ruler-mode-default (:background bg2 :foreground gray))
    (ruler-mode-column-number (:foreground faded-blue))
    (ruler-mode-current-column (:foreground orange))
    (ruler-mode-fill-column (:foreground pink))
    (ruler-mode-comment-column (:foreground teal-blue))
    (ruler-mode-fringes (:foreground green))
    (ruler-mode-pad (:foreground faded-blue))
    (ruler-mode-tab-stop (:foreground violet))
    (ruler-mode-goal-column (:foreground alt-red))

    ;; TODO: Message faces
    (message-header-name (:foreground deep-green))
    (message-header-subject (:foreground teal-green))
    (message-header-to (:foreground teal-green))
    (message-header-other (:foreground teal))

    ;; Elfeed
    ;; TODO: split
    (elfeed-search-tag-face (:foreground light-yellow))
    (elfeed-search-feed-face (:foreground green))
    (elfeed-search-date-face (:foreground faded-blue))
    (elfeed-search-unread-title-face (:foreground fg1))
    (elfeed-search-unread-count-face (:foreground orange))
    (elfeed-search-title-face (:foreground comment))


    ;; Modeline
    (mode-line           (:box (:line-width 2 :color line-bg2) :background line-bg1 :foreground faded-blue :bold bold))
    (mode-line-buffer-id (:background nil :foreground teal :bold bold))
    (mode-line-highlight (:foreground keyword :box nil :bold bold))
    (mode-line-inactive  (:box (:line-width 2 :color line-bg1) :background line-bg1 :foreground light-gray :bold bold))
    (mode-line-emphasis  (:foreground fg1))

    ;; Telephone-line
    (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground line-fg))
    (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground light-gray))
    (telephone-line-evil            (:inherit 'mode-line))
    (telephone-line-evil-normal     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-normal))
    (telephone-line-evil-insert     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-insert))
    (telephone-line-evil-visual     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-visual))
    (telephone-line-evil-replace    (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-replace))
    (telephone-line-evil-motion     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-motion))
    (telephone-line-evil-operator   (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-operator))
    (telephone-line-evil-emacs      (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-emacs))

    ;; Powerline
    ;; TODO: check it
    (powerline-active1   (:inherit 'mode-line))
    (powerline-active2   (:inherit 'mode-line))
    (powerline-inactive1 (:inherit 'mode-line-inactive))
    (powerline-inactive2 (:inherit 'mode-line-inactive))

    ;; Spaceline
    (spaceline-highlight-face (:foreground teal))

    ;; Smart-mode-line
    (sml/line-number (:foreground lime))
    (sml/modes (:foreground purple))
    (sml/global (:foreground teal))
    (sml/filename (:foreground green))
    (sml/charging (:foreground green))
    (sml/discharging (:foreground red))
    (sml/modified (:foreground light-green :bold bold))
    (sml/outside-modified (:background alt-red :foreground fg1))
    (sml/prefix (:foreground line-fg))
    (sml/read-only (:foreground orange))

    ;; TODO: maybe delete or change to other package
    ;; Fic-mode
    (fic-face  (:background nil :foreground red :bold bold))
    (fic-author-face  (:background nil :foreground red :bold bold))

    ;; Company
    (company-tooltip (:background tooltip-bg :foreground fg3 :bold bold))
    (company-tooltip-common (:foreground hl))
    (company-tooltip-common-selection (:foreground light-orange))
    (company-tooltip-selection (:background tooltip-hl-bg :foreground tooltip-hl-fg))
    (company-tooltip-annotation (:foreground faded-blue))
    (company-scrollbar-bg (:background bg1))
    (company-scrollbar-fg (:foreground keyword))
    ;; TODO: read about template
    (company-template-field (:background bg3))
    (company-echo-common (:background bg1 :foreground light-yellow))
    (company-preview (:background nil :foreground keyword))
    (company-preview-common (:background bg2 :foreground light-orange))
    (company-preview-search (:background bg1 :foreground blue))
    (company-tooltip-mouse (:background bg3 :foreground fg3))


    ;; Flycheck
    (flycheck-info (:foreground teal-blue))
    (flycheck-warning (:underline (:style underline-style :color warning)))
    (flycheck-error (:underline (:style underline-style :color err)))
    (flycheck-fringe-error (:foreground err))
    (flycheck-fringe-warning (:foreground warning))
    (flycheck-fringe-info (:foreground teal-blue))

    ;; Flyspell
    (flyspell-duplicate (:underline (:style underline-style :color warning)))
    (flyspell-incorrect (:underline (:style underline-style :color err)))

    ;; Hydra
    (hydra-face-red (:foreground red))
    (hydra-face-teal (:foreground teal))
    (hydra-face-blue (:foreground soft-blue))
    (hydra-face-pink (:foreground pink))
    (hydra-face-amaranth (:foreground purple))

    ;; Ido
    (ido-indicator (:foreground num))
    (ido-first-match (:foreground hl :bold bold))
    (ido-only-match (:foreground cyan))
    (ido-subdir (:foreground lavender))

    ;; Gnus
    (gnus-header-content (:foreground keyword))
    (gnus-header-from (:foreground var))
    (gnus-header-name (:foreground type))
    (gnus-header-subject (:foreground functions :bold bold))

    ;; Mu4e
    (mu4e-header-marks-face (:foreground type))
    (mu4e-view-url-number-face (:foreground type))
    (mu4e-cited-1-face (:foreground fg2))
    (mu4e-cited-7-face (:foreground fg3))

    ;; ffap
    (ffap (:foreground fg4))

    ;; Slime
    (slime-repl-inputed-output-face (:foreground type))

    ;; Js-mode
    (js2-private-function-call (:foreground const))
    (js2-jsdoc-html-tag-delimiter (:foreground str))
    (js2-jsdoc-html-tag-name (:foreground keyword))
    (js2-external-variable (:foreground type))
    (js2-function-param (:foreground const))
    (js2-error (:underline (:color alt-red :style underline-style)))
    (js2-function-call (:foreground functions))
    (js2-object-property (:foreground light-brown))
    (js2-jsdoc-value (:foreground str))
    (js2-private-member (:foreground fg3))
    (js3-function-param-face (:foreground keyword))
    (js3-instance-member-face (:foreground const))
    (js3-external-variable-face (:foreground var))
    (js3-jsdoc-tag-face (:foreground keyword))
    (js3-warning-face (:underline keyword))
    (js3-error-face (:underline err))

    ;; Undo-tree
    (undo-tree-visualizer-active-branch-face (:foreground fg1 :bold bold))
    (undo-tree-visualizer-current-face       (:foreground cyan))
    (undo-tree-visualizer-default-face       (:foreground fg2))
    (undo-tree-visualizer-unmodified-face    (:foreground var))
    (undo-tree-visualizer-register-face      (:foreground type))

    ;; Rainbow delimeters
    ;; TODO: change color & rewrite
    (show-paren-match (:background nil :foreground orange :bold bold))
    ;; (if (eq show-paren-style 'expression)
    ;;  (show-paren-match-face (:background bg3 :foreground nil))))
    ;;  (show-paren-match-face (:background nil :foreground orange :bold bold)))))

    (show-paren-mismatch (:background red :foreground bg2))
    (rainbow-delimiters-unmatched-face (:foreground warning))
    (rainbow-delimiters-depth-1-face (:foreground rb1))
    (rainbow-delimiters-depth-2-face (:foreground rb2))
    (rainbow-delimiters-depth-3-face (:foreground rb3))
    (rainbow-delimiters-depth-4-face (:foreground rb4))
    (rainbow-delimiters-depth-5-face (:foreground rb5))
    (rainbow-delimiters-depth-6-face (:foreground rb6))
    (rainbow-delimiters-depth-7-face (:foreground rb7))
    (rainbow-delimiters-depth-8-face (:foreground rb8))
    (rainbow-delimiters-depth-9-face (:foreground rb9))

    ;; Diff
    (diff-header (:background bg2))
    (diff-file-header (:background bg2 :foreground green))
    (diff-added (:background dark-green :foreground fg1))
    (diff-changed (:background diff-change :foreground fg1))
    (diff-removed (:background dark-red :foreground fg1))

    ;; Git gutter
    (git-gutter:unchanged (:background bg1 :foreground nil))
    (git-gutter:added (:background bg1 :foreground diff-add :bold bold))
    (git-gutter:modified (:background bg1 :foreground diff-change :bold bold))
    (git-gutter:deleted (:background bg1 :foreground diff-del :bold bold))

    ;; Diff-hl
    ;; TODO: split
    (diff-hl-insert (:background diff-add))
    (diff-hl-change (:background diff-change))
    (diff-hl-delete (:background diff-del))

    ;; Popup
    ;; TODO: split
    (popup-face (:background tooltip-bg :foreground tooltip-fg :bold bold))
    (popup-menu-selection-face (:background tooltip-hl-bg :foreground tooltip-hl-fg :bold bold))

    ;; Terminal
    ;; TODO: split
    (term (:foreground fg1))
    (term-color-black (:foreground bg1))
    (term-color-blue (:foreground blue))
    (term-color-red (:foreground red))
    (term-color-green (:foreground green))
    (term-color-yellow (:foreground yellow))
    (term-color-magenta (:foreground purple))
    (term-color-cyan (:foreground cyan))
    (term-color-white (:foreground fg2))

    ;; EShell
    ;; TODO: split
    (eshell-prompt (:foreground green :bold bold))
    (eshell-ls-directory (:foreground magenta :bold bold))
    (eshell-ls-symlink (:foreground blue :bold bold))
    (eshell-ls-executable (:foreground lime :bold bold))
    (eshell-ls-archive (:foreground red))
    (eshell-ls-backup (:foreground purple))
    (eshell-ls-clutter (:foreground pink))
    (eshell-ls-missing (:background bg3 :foreground red))
    (eshell-ls-product (:foreground yellow))
    (eshell-ls-readonly (:foreground fg2))
    (eshell-ls-special (:foreground light-green))
    (eshell-ls-unreadable (:foreground faded-blue))

    ;; Whitespace
    ;; TODO: split
    (whitespace-empty            (:foreground red))
    (whitespace-line             (:background bg2))
    (whitespace-space            (:background bg2))
    (whitespace-tab              (:foreground gray))
    (whitespace-newline          (:foreground gray))
    (whitespace-hspace           (:foreground orange))
    (whitespace-trailing         (:background bg1))
    ;; Smartparens

    (sp-pair-overlay-face (:foreground nil))

    ;; Web
    ;; TODO: split
    (css-selector                   (:inherit 'font-lock-builtin-face))
    (web-mode-css-selector-face     (:inherit 'font-lock-builtin-face))
    (web-mode-type-face             (:inherit 'font-lock-type-face))
    (web-mode-html-tag-face         (:inherit 'font-lock-keyword-face))
    (web-mode-html-tag-bracket-face (:inherit 'web-mode-html-tag-face))
    (web-mode-html-attr-name-face   (:inherit 'font-lock-function-name-face))
    (web-mode-html-attr-value-face  (:inherit 'font-lock-string-face))
    (web-mode-builtin-face          (:inherit 'font-lock-builtin-face))
    (web-mode-keyword-face          (:inherit 'font-lock-builtin-face))
    (web-mode-constant-face         (:inherit 'font-lock-constant-face))
    (web-mode-comment-face          (:inherit 'font-lock-comment-face))
    (web-mode-doctype-face          (:foreground purple :bold bold))
    (web-mode-function-name-face    (:inherit 'font-lock-function-name-face))
    (web-mode-string-face           (:inherit 'font-lock-string-face))
    (web-mode-warning-face          (:inherit 'font-lock-warning-face))

    ;; Evil ex
    ;; TODO: split
    (evil-ex-info (:foreground orange))
    (evil-ex-substitute-matches (:background nil :foreground red :underline underline))
    (evil-ex-substitute-replacement (:background nil :foreground light-green))
    (evil-ex-lazy-highlight (:inherit 'lazy-highlight))

    ;; Ivy & swiper basic
    (ivy-minibuffer-match-face-1 (:background nil :foreground fg1))
    (ivy-minibuffer-match-face-2 (:background nil :foreground soft-blue :bold bold))
    (ivy-minibuffer-match-face-3 (:background nil :foreground light-orange :bold bold))
    (ivy-minibuffer-match-face-4 (:background nil :foreground moderate-pink :bold bold))

    (swiper-match-face-1 (:background bg2 :foreground fg1))
    (swiper-match-face-2 (:background bg2 :foreground soft-blue :bold bold))
    (swiper-match-face-3 (:background bg2 :foreground light-orange :bold bold))
    (swiper-match-face-4 (:background bg2 :foreground moderate-pink :bold bold))
    (swiper-line-face    (:inherit 'hl-line))))

;; Predefined Kaolin variables
(defconst kaolin-vars
  '())

;; Works
;; (let ((palette kaolin-palette))
;;   (cl-loop for el in test-palette
;;            do (kaolin-theme--add-to-alist 'palette (car el) (cdr el)))
;;   (alist-get 'yellow palette))

(provide 'kaolin-theme-lib)

;;; kaolin-eclipse-theme.el ends here
