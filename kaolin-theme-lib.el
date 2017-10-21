;;; kaolin-theme-lib.el --- Kaolin-theme library

;; Common Kaolin palette
;; Add extra colors per class
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


;; Common Kaolin faces specifications
(defconst kaolin-faces
  '(
    (default (:background midnight-purple :foreground yellow))
    (button  (:underline t :weight 'bold :foreground black1))
    (font-lock-comment-face   (:foreground blue))
    (error   (:foreground red))))

;; Works
;; (let ((palette kaolin-palette))
;;   (cl-loop for el in test-palette ;; change to real arg
;;            do (kaolin-theme--add-to-alist 'palette (car el) (cdr el)))
;;   (alist-get 'yellow palette))

(provide 'kaolin-theme-lib)

;;; kaolin-eclipse-theme.el ends here
