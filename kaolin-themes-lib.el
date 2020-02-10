;;; kaolin-themes-lib.el --- Kaolin-themes library, provides common parts for the package  -*- lexical-binding: t; -*-
;;; Commentary:
;;
;;; Color order
;; TODO: add color close to color5 but satured
;;
;; color0 - almost pure/contrast >= 70 sat & > 70 val
;; color1 - regular
;; color2 - dark
;; color3 - light/soft
;; color4 - desaturated/faded/muted
;;
;; color5 - midnight <= 50 sat & < 30 value
;; color6 - very dark color 20-30 sat & val; see spring-green
;; color7 - dark grayish sat ~20 & val 50+; adjust with gray3; see erin7
;; color8 - light grayish; sat ~10-15 & val ~ 65+
;; color9 - very light color hue 8-17 value ~90-95
;;
;;; Color list
;;
;; Color name - color hex - Kaolin hue
;;
;; Black #020203 - 240
;; Gray #CED8D9 - 189
;; White #FDFDFF - 240
;;
;; Colors:
;; Red #FF0000 - 358-360
;; Brown #A33C28 - 6
;; Vermilion #FF3F00 - 17
;; Orange #FF7F00 - 36
;; Amber #FFBF00 - 43
;; Yellow #FFFF00 - 60
;; Lime #BFFF00 - 75
;; Chartreuse #7FFF00 - 86
;; Harlequin #3FFF00 - 104
;; Green #00FF00 - 120
;; Erin #00FF3F - 135
;; Spring-green #00FF7F - 150
;; Aquamarine #00FFBF - 163
;; Teal #00A89D - 178
;; Cyan #00FFFF - 182
;; Capri/Deep Sky Blue #00BFFF - 201
;; Azure/Sky Blue #007FFF - 210
;; Cerulean #003FFF - 221
;; Blue #0000FF - 238
;; Ultramarine #3F00FF - 254
;; Violet #7F00FF - 270
;; Purple #BF00FF - 279
;; Magenta/Fuchsia #FF00FF - 300
;; Cerise #FF00BF - 315
;; Pink/rose #FF007F - 335
;; Crimson #FF003F - 347
;;

(defun color-lab-luminance (color)
  "Return the luminance through LAB color space of a color string (e.g. \"#ffaa00\", \"blue\")."
  (nth 0 (apply #'color-srgb-to-lab (color-name-to-rgb color))))

(defun color-dark-p (color)
  "Return t if COLOR (e.g. hex string or name) is dark."
  (< (color-lab-luminance color) 50))

(defun color-light-p (color)
  "Return t if COLOR (e.g. hex string or name) is light."
  (>= (color-lab-luminance color) 50))

;;; Code:
(defconst kaolin-palette
  '(
    ;; Old black
    ;; (black0 "#181818")
    ;; (black1 "#1b1b1b")
    ;; (black2 "#252525")
    ;; (black3 "#2f2f2f")
    ;; (black4 "#353535")

    ;; Black - #020203
    (black0 "#161618")
    (black1 "#18181B")
    (black2 "#222225")
    (black3 "#2B2B2F")
    (black4 "#303035")

    ;; Gray - #CED8D9
    (gray0 "#353b3c")
    (gray1 "#383e3f")
    ;; (gray2 "#414849") ; old gray
    (gray2 "#4b5254")
    (gray3 "#545c5e")
    (gray4 "#60696b") ; old alt-gray
    (gray5 "#697375")
    (gray6 "#737d80")
    (gray7 "#7c878a") ; old bright-gray
    (gray8 "#879193")
    (gray9 "#919a9c") ; old light-gray

    ;; White - #FDFDFF
    (white0 "#f2f2f2")
    (white1 "#e6e6e8")
    (white2 "#d4d4d6")
    (white3 "#c9c9cd")
    (white4 "#bebec4")

    ;; Yellow #FFFF00
    (yellow0 "#eeeb28")
    (yellow1 "#E3D168")
    (yellow2 "#a39423") ; dark-yellow
    (yellow3 "#eae46a")
    (yellow4 "#c8c493" "#ffd7a5")
    (yellow5 "#1e1e14") ; old midnight yellow
    (yellow6 "#40402E")
    (yellow7 "#848468")
    (yellow8 "#c5c5a5")
    (yellow9 "#EEEED3")

    ;; Amber #FFBF00
    (amber0 "#f3c91f")
    (amber1 "#CFB05F")
    (amber2 "#91762a")
    (amber3 "#eed891")
    (amber4 "#c5b378")
    (amber5 "#1e1c14")
    (amber6 "#403B2E")
    (amber7 "#6E6653")
    (amber8 "#c7c2af")
    (amber9 "#eee6d3")

    ;; Orange #FF7F00
    (orange0 "#e67417")
    (orange1 "#dbac66")
    (orange2 "#b87e3c")
    (orange3 "#f5c791")
    ;; (orange4 "#e1b079")
    (orange4 "#dbb68f")
    (orange5 "#1e1914")
    (orange6 "#40392E")
    (orange7 "#847968")
    (orange8 "#c2b4a1") ; grayish-orange
    (orange9 "#EEE6D3")

    ;; Vermilion #FF3F00
    (vermilion0 "#F84B1B")
    (vermilion1 "#ca6036")
    (vermilion2 "#a14223")
    (vermilion3 "#ee7042")
    (vermilion4 "#cd9575" "#d7af87") ; faded-orange
    (vermilion5 "#231610")
    (vermilion6 "#40332E")
    (vermilion7 "#847068")
    (vermilion8 "#bfaa9f")
    (vermilion9 "#EEDBD3")

    ;; Brown #A33C28
    (brown0 "#872C19")
    (brown1 "#7d6360")
    (brown2 "#52413f")
    (brown3 "#B08C77")
    (brown4 "#B89A88")
    (brown5 "#1C1511")
    (brown6 "#40332E")
    (brown7 "#846B68")
    (brown8 "#AEA19E")
    (brown9 "#EEDDD3")

    ;; Red #FF0000
    (red0 "#c93237")
    (red1 "#cd5c60")
    (red2 "#832729")
    (red3 "#e84c58")
    (red4 "#c86d6d")
    (red5 "#1E1414")
    (red6 "#402e2e")
    (red7 "#846869")
    (red8 "#CAABAB")
    (red9 "#EED3D7")

    ;; Crimson #FF003F
    (crimson0 "#D6224D")
    (crimson1 "#e55c7a")
    (crimson2 "#941b37")
    (crimson3 "#ef6787")    ; light-pink
    (crimson4 "#a0586c")    ; moderate-pink
    (crimson5 "#210E14")
    (crimson6 "#402E33")
    (crimson7 "#84686E")
    (crimson8 "#c5b3b9")
    (crimson9 "#EED3DB")

    ;; Rose/pink #FF007F
    (pink0 "#eb3380")
    (pink1 "#d24b83")
    (pink2 "#9E2256")
    (pink3 "#fbaed2")
    (pink4 "#c791aa")
    ;; (pink5 "#210F17")
    (pink5 "#1e1419")
    ;; (pink6 "#402E35")
    (pink6 "#513C44")
    (pink7 "#846874")
    (pink8 "#CAB2BD")
    (pink9 "#EED3DF")

    ;; Cerise #FF00BF
    (cerise0 "#e121b1")
    (cerise1 "#cf44ac")
    (cerise2 "#a31880")
    (cerise3 "#e361c3")
    (cerise4 "#a9779c")
    (cerise5 "#23121c")
    (cerise6 "#402E3B")
    (cerise7 "#84687D")
    (cerise8 "#c7b7c2")
    (cerise9 "#EED3EA")

    ;; Magenta/Fuchsia #FF00FF
    (magenta0 "#c932c9")
    ;; (magenta1 "#cd5ccd")
    (magenta1 "#D16BD1")
    ;; (magenta2 "#563d56")
    (magenta2 "#734073")
    (magenta3 "#cea2ca") ; light-puprle
    ;; (magenta4 "#835d83") ; purple
    (magenta4 "#845A84") ; purple
    (magenta5 "#1a121a") ; old midnight-purple
    (magenta6 "#402E40")
    (magenta7 "#846884")
    (magenta8 "#BFA8BF")
    (magenta9 "#EED3EE")

    ;; Purple #BF00FF
    (purple0 "#ab33eb")
    (purple1 "#A34BD2")
    ;; TODO: change
    (purple2 "#73229E")
    (purple3 "#bc90d4")
    (purple4 "#ab98b5")
    (purple5 "#1f1623")
    (purple6 "#392E40")
    (purple7 "#7A6884")
    (purple8 "#bcacbf")
    (purple9 "#E6D3EE")

    ;; Violet #7F00FF
    (violet0 "#853AE1")
    (violet1 "#8B48CF")
    (violet2 "#61259e")
    (violet3 "#c79af4")
    (violet4 "#9d81ba") ; alt-lavender
    (violet5 "#1f1926")
    (violet6 "#372E40")
    (violet7 "#766884")
    (violet8 "#B8ABC5")
    (violet9 "#E2D3EE")

    ;; Ultramarine #3F00FF
    (ultramarine0 "#554AF5")
    (ultramarine1 "#7F77F2")
    (ultramarine2 "#6D6487")
    (ultramarine3 "#9587DD")
    (ultramarine4 "#787096")
    (ultramarine5 "#16141e")
    ;; (ultramarine6 "#322E40")
    (ultramarine6 "#2D2C58")
    (ultramarine7 "#6E6884")
    (ultramarine8 "#b0acc5")
    (ultramarine9 "#DBD3EE")

    ;; Blue #0000FF
    (blue0 "#3237CA")
    (blue1 "#4246BA")
    ;; (blue2 "#2C30AB")
    (blue2 "#3242A1")
    (blue3 "#526AF3")
    (blue4 "#807f96") ; old faded-blue
    (blue5 "#14141e" black2) ; old alt-midnight-blue
    (blue6 "#2E2E40")
    (blue7 "#686984")
    (blue8 "#A1A0C5")
    (blue9 "#D3D7EE")

    ;; Cerulean #003FFF
    ;; (cerulean0 "#0e4cd1")
    (cerulean0 "#316CED")
    (cerulean1 "#3f66ba")
    (cerulean2 "#2d4b8c")
    (cerulean3 "#4c7de8")
    (cerulean4 "#738FD7")
    (cerulean5 "#14171e")
    (cerulean6 "#2E3340")
    (cerulean7 "#687184") ; old grayish-blue
    (cerulean8 "#8F97A7")
    (cerulean9 "#C6D5E8")

    ;; Azure #007FFF
    (azure0 "#0e70d1")
    ;; (azure1 "#3f7dba") ; old blue
    (azure1 "#3B84CC") ; old blue
    ;; (azure2 "#2a4661")
    (azure2 "#3B6F87")
    ;; TODO:
    ;; (azure2 "#325074")
    (azure3 "#4ca6e8") ; old soft-blue
    (azure4 "#53859d") ; old moderate-blue
    (azure5 "#14191e")
    (azure6 "#2E3740")
    (azure7 "#687684")
    (azure8 "#8B9AA7")
    (azure9 "#D3E4F0")

    ;; Capri #00BFFF
    ;; TODO: adjust
    (capri0 "#1a9eee")
    (capri1 "#2683b5")
    (capri2 "#1c5f87")
    (capri3 "#41b0f3")
    (capri4 "#91b9c7")
    (capri5 "#1e2528" black2) ;; old midnight-blue
    (capri6 "#2E3940")
    (capri7 "#687A84")
    (capri8 "#98AAB3")
    (capri9 "#D3E6EE")

    ;; Cyan #00FFFF
    ;; TODO: #00B7EB
    (cyan0 "#0bc9cf")
    (cyan1 "#57bfc2")
    (cyan2 "#09878b")
    (cyan3 "#6bd9db")
    ;; TODO:
    ;; (cyan3 "#68d7f3")
    (cyan4 "#65a0a1")
    ;; (cyan5 "#142223")
    (cyan5 "#141e1e")
    (cyan6 "#2e3f40")
    (cyan7 "#688384")
    (cyan8 "#A2C5C5")
    (cyan9 "#D3EEEE")

    ;; Teal #00A89D
    (teal0 "#0D9C94")
    (teal1 "#4d9391")
    (teal2 "#1D5E5C")
    (teal3 "#49bdb0")
    (teal4 "#80bcb6")
    (teal5 "#141e1d")
    (teal6 "#2E403F")
    (teal7 "#5F7A79")
    (teal8 "#a4bab9")
    (teal9 "#D3EEEC")

    ;; Aquamarine #00FFBF
    (aquamarine0 "#0ed49b")
    (aquamarine1 "#47ba99")
    (aquamarine2 "#40826d")
    (aquamarine3 "#68f3ca")
    (aquamarine4 "#709688")
    (aquamarine5 "#141e1b")
    (aquamarine6 "#2E403B")
    (aquamarine7 "#68847C")
    (aquamarine8 "#A7C2BA")
    (aquamarine9 "#D3EEE6")

    ;; Spring green #00FF7F
    (spring-green0 "#2ae186")
    (spring-green1 "#35BF88")
    (spring-green2 "#39855f") ; dark
    (spring-green3 "#65E6A7")
    (spring-green4 "#5D8272") ; faded
    (spring-green5 "#141E1A")
    (spring-green6 "#2E4038") ; old midnight
    (spring-green7 "#688476")
    (spring-green8 "#90aea1")
    (spring-green9 "#D4EEE3")

    ;; Erin #00FF3F
    (erin0 "#26e356")
    (erin1 "#48ca69")
    (erin2 "#39854C")
    (erin3 "#68f385")
    (erin4 "#597a64")
    (erin5 "#141e17")
    (erin6 "#2E4032")
    (erin7 "#526156")
    (erin8 "#A8CFB6")
    (erin9 "#D3EEDB")

    ;; Green #00FF00
    (green0 "#21e121")
    (green1 "#47cc47")
    (green2 "#18a318")
    ;; (green3 "#61e361")
    (green3 "#7CF083")
    (green4 "#73c66c")
    ;; (green5 "#111C11")
    (green5 "#141e14")
    (green6 "#2E402E")
    (green7 "#688468")
    (green8 "#abc6a8")
    (green9 "#D3EED3")

    ;; Harlequin #3FFF00
    (harlequin0 "#58f021")
    (harlequin1 "#6FC550")
    (harlequin2 "#37A111")
    (harlequin3 "#91f368")
    (harlequin4 "#60A148")
    (harlequin5 "#161E14")
    (harlequin6 "#33402E")
    (harlequin7 "#6F8468")
    (harlequin8 "#b0c6a8")
    (harlequin9 "#DBEED3")

    ;; Chartreuse #7FFF00
    (chartreuse0 "#88ee1a")
    (chartreuse1 "#92c550")
    (chartreuse2 "#5ba111")
    (chartreuse3 "#9de346")
    (chartreuse4 "#7fa148")
    (chartreuse5 "#161E0D")
    (chartreuse6 "#38402e")
    (chartreuse7 "#788468")
    (chartreuse8 "#afbaa2")
    (chartreuse9 "#E2EED3")

    ;; Lime #BFFF00
    (lime0 "#aadc13")
    (lime1 "#a8c749")
    (lime2 "#82a80e")
    (lime3 "#c7ee53")
    (lime4 "#b9c791")
    (lime5 "#1B210E")
    (lime6 "#3B402E")
    (lime7 "#7D8468")
    (lime8 "#b5baa4")
    (lime9 "#EAEED3")


    ;; Named color vars
    (italic          kaolin-themes-italic)
    (bold            kaolin-themes-bold)
    (underline       kaolin-themes-underline)
    (underline-style (if kaolin-themes-underline-wave 'wave 'line))

    (fg0  white0)
    (fg1  white1)
    (fg2  white2)
    (fg3  white3)
    (fg4  white4)

    (bg0  black0)
    (bg1  black1)
    (bg2  black2)
    (bg3  black3)
    (bg4  black4)
    (pane bg0)

    (dim-buffer bg0)
    (comment     gray3)
    (comment-alt teal2)

    ;; TODO:
    (kaolin-comment
      (pcase kaolin-themes-comments-style
        ('normal comment)
        ('color  comment-alt)
        ('bright comment)))

    (hl         aquamarine3)
    (hl-bg      comment)
    (hl-line    (if kaolin-themes-hl-line-colored capri5 bg2))
    (hl-indent  comment)
    (selection  bg3)
    (pulse      spring-green6)

    (todo red1)
    (done spring-green3)

    (adaptive-fg (if (color-dark-p bg1) white0 bg1))
    ;; TODO: add pos-tip in custom-theme-set-variables
    (tooltip-bg bg2)
    (tooltip-fg fg2)
    (tooltip-hl-bg brown2)
    (tooltip-hl-fg amber3)

    (rb-match hl)
    (rb1 cyan3)
    (rb2 purple4)
    (rb3 spring-green4)
    (rb4 blue4)
    (rb5 teal1)
    (rb6 violet3)
    (rb7 orange8)
    (rb8 magenta4)
    (rb9 violet4)

    (diff-add    spring-green1)
    (diff-mod    purple3)
    (diff-rem    red1)

    (diff-bg-add spring-green2)
    (diff-bg-mod vermilion4)
    (diff-bg-rem crimson4)

    (keyword     teal1)
    (metakey     (if kaolin-themes-distinct-metakeys keyword comment))
    (builtin     teal4)
    (header      builtin)
    (functions   builtin)
    (str         spring-green3)
    (str-alt     spring-green4)
    (doc         str-alt)
    (type        vermilion4)
    (var         blue4)
    (const       purple4)
    (num         red1)
    (bool        num)
    (prep        violet4)
    (link        prep)
    ;; MAYBE: add orange/yellow background?
    (warning     orange1)
    (err         red1)

    (keysym      prep)
    (prompt      keyword)

    ;; Custom buttons
    (button amber6)
    (button-bg doc)
    (button-border (if (color-dark-p bg1) gray3 white4))
    (button-hl amber3)

    ;; Mode-line
    (line-fg           fg3)
    (line-inactive     comment)
    (line-bg1          bg2)
    (line-bg2          bg4)
    (line-border       (if kaolin-themes-modeline-border bg4 line-bg1))
    (line-color1       fg1)
    (line-color2       builtin)
    ; TODO:
    (segment-active    gray3)
    (segment-inactive  gray3)

    (evil-normal       keyword)
    (evil-insert       done)
    (evil-visual       var)
    (evil-replace      todo)
    (evil-motion       warning)
    (evil-operator     type)
    (evil-emacs        prep)

    (fringe        (if kaolin-themes-distinct-fringe bg2 bg1))
    (win-border    bg3)
    (line-num-bg   (if kaolin-themes-distinct-fringe bg2 bg1))
    (line-num-fg   gray3)
    (line-num-hl   gray9)
    (cursor        white0)

    (company-scroll-bg (if kaolin-themes-distinct-company-scrollbar bg4 bg2))
    (company-scroll-fg (if kaolin-themes-distinct-company-scrollbar line-num-hl bg4))

    (ivy1     fg1)
    (search1  azure3)
    (search2  amber3)
    (search3  violet3)))

;; Predefined Kaolin face specifications
(defconst kaolin-faces
  `(
    ;; Font-lock
    (font-lock-builtin-face           (:foreground builtin))
    (font-lock-comment-delimiter-face (:background nil :foreground kaolin-comment :italic kaolin-themes-italic-comments))
    (font-lock-comment-face           (:background nil :foreground kaolin-comment :italic kaolin-themes-italic-comments))
    (font-lock-constant-face          (:foreground const))
    (font-lock-doc-face               (:foreground doc))
    (font-lock-function-name-face     (:foreground functions))
    (font-lock-keyword-face           (:foreground keyword))
    (font-lock-negation-char-face     (:foreground err))
    (font-lock-preprocessor-face      (:foreground prep))
    (font-lock-reference-face         (:foreground const))
    (font-lock-string-face            (:foreground str))
    (font-lock-type-face              (:foreground type))
    (font-lock-variable-name-face     (:foreground var))
    (font-lock-warning-face           (:background nil :foreground warning))

    ;; Kaolin faces
    (kaolin-themes-boolean (:foreground bool))

    ;; General
    (default             (:background bg1 :foreground fg1))
    (warning             (:foreground warning))
    (error               (:foreground err))
    (shadow              (:foreground comment))
    (file-name-shadow    (:inherit 'shadow))
    (region              (:background selection :foreground fg4))
    (secondary-selection (:background hl-bg :foreground adaptive-fg))
    (fringe              (:background fringe :foreground fg1))
    (cursor              (:background cursor))
    (vertical-border     (:foreground win-border))
    (window-divider      (:foreground win-border))
    (minibuffer-prompt   (:background nil :foreground prompt :bold bold))
    (bold                (:bold bold))
    (italic              (:italic italic))
    (default-italic      (:italic italic))
    (bold-italic         (:bold bold :italic italic))
    (link                (:foreground link :underline underline))
    (link-visited        (:inherit 'link :underline nil))
    (success             (:background nil :foreground done))
    (escape-glyph        (:background nil :foreground cyan3))
    (trailing-whitespace (:background err))

    (menu        (:background bg2 :foreground fg2))
    ;; TODO: default bg
    (header-line (:background nil :foreground num))
    (tool-bar    (:inherit 'header-line))

    (tooltip      (:background tooltip-bg :foreground tooltip-fg))

    (match        (:background nil :foreground hl))
    (isearch      (:background nil :foreground hl :bold bold :underline underline))
    (isearch-fail (:background nil :foreground err))

    ;; Interface
    (package-name                   (:inherit 'link :underline nil))
    (button                         (:inherit 'link))
    (custom-button                  (:background button-bg :foreground adaptive-fg :box (:line-width 2 :color button-bg :style 'none) :height 0.9))
    (custom-button-mouse            (:inherit 'custom-button :foreground button-hl :box (:line-width 2 :color button-hl :style 'none)))
    (custom-button-pressed          (:inherit 'custom-button :foreground button-hl :box (:line-width 2 :color button-border :style 'none)))
    (custom-button-unraised         (:inherit 'custom-button))
    (custom-button-pressed-unraised (:inherit 'custom-button-pressed))
    (custom-group-tag               (:foreground header :height 1.2 :weight 'bold))
    (custom-group-subtitle          (:foreground header :height 1.0 :weight 'bold))
    (custom-variable-button         (:inherit 'button))
    (custom-comment                 (:background hl-bg :foreground fg1))
    (custom-comment-tag             (:foreground comment))
    (custom-documentation           (:foreground fg1))
    (custom-visibility              (:background nil :foreground cyan1 :height 0.9 :underline underline))
    (custom-state                   (:background nil :foreground str))
    (custom-changed                 (:background nil :foreground diff-mod))
    (custom-set                     (:background nil :foreground done))
    (custom-themed                  (:background nil :foreground done))
    (custom-invalid                 (:background nil :foreground err))
    (custom-variable-tag            (:foreground var))
    (custom-variable-obsolete       (:inherit 'shadow))
    (widget-documentation           (:background nil :foreground var))
    ;; (widget-button                  (:background nil :foreground keyword))
    (widget-button-pressed          (:background nil :foreground builtin))
    (widget-field                   (:background bg2 :foreground fg2 :box (:line-width 2 :color bg4 :style nil)))
    (widget-single-line-field       (:inherit 'widget-field))

    ;; Dashboard
    (dashboard-heading   (:foreground header))
    (dashboard-navigator (:foreground prep))
    (dashboard-footer    (:foreground str))

    ;; Compilation
    (compilation-column-number  (:foreground fg2))
    (compilation-line-number    (:foreground num))
    (compilation-info           (:inherit 'success))
    (compilation-warning        (:inherit 'warning))
    (compilation-error          (:inherit 'error :weight 'bold))
    (compilation-mode-line-exit (:inherit 'compilation-info))
    (compilation-mode-line-fail (:inherit 'compilation-error))

    ;; Dired
    (dired-header     (:foreground header :weight 'bold))
    (dired-directory  (:foreground keyword))
    (dired-ignored    (:foreground comment))
    (dired-flagged    (:foreground err))
    (dired-mark       (:foreground num :weight 'bold))
    (dired-marked     (:foreground hl :weight 'bold))
    (dired-perm-write (:foreground fg1 :underline t))
    (dired-symlink    (:foreground functions))
    (dired-warning    (:inherit 'font-lock-warning-face))

    ;; dired-plus
    (diredp-dir-name               (:foreground keyword :weight 'bold :strike-through nil))
    (diredp-dir-heading            (:foreground header :weight 'bold :strike-through nil))
    (diredp-file-name              (:foreground fg1 :strike-through nil))
    (diredp-file-suffix            (:foreground const))
    (diredp-ignored-file-name      (:inherit 'shadow))
    (diredp-omit-file-name         (:inherit 'shadow))
    (diredp-compressed-file-suffix (:foreground comment))
    (diredp-symlink                (:foreground functions))
    (diredp-read-priv              (:foreground diff-add))
    (diredp-write-priv             (:foreground diff-mod))
    (diredp-exec-priv              (:foreground diff-rem))
    (diredp-executable-tag         (:foreground diff-rem))
    (diredp-rare-priv              (:foreground err :weight 'bold))
    (diredp-dir-priv               (:foreground keyword :weight 'bold))
    (diredp-other-priv             (:foreground warning))
    (diredp-no-priv                (:foreground comment))
    (diredp-number                 (:foreground num))
    (diredp-date-time              (:foreground prep))
    (diredp-flag-mark              (:background hl-bg :foreground diff-mod))
    (diredp-flag-mark-line         (:background hl-bg))
    (diredp-deletion               (:background nil :foreground err :underline underline))
    (diredp-deletion-file-name     (:background nil :foreground err :underline underline))
    (diredp-autofile-name          (:foreground num :underline underline))

    ;; diredfl
    (diredfl-autofile-name          (:foreground num :underline underline))
    (diredfl-compressed-file-name   (:foreground comment))
    (diredfl-compressed-file-suffix (:foreground comment))
    (diredfl-date-time              (:foreground prep))
    (diredfl-deletion               (:background nil :foreground err :underline underline))
    (diredfl-deletion-file-name     (:background nil :foreground err :underline underline))
    (diredfl-dir-heading            (:foreground header :weight 'bold :strike-through nil))
    (diredfl-dir-name               (:foreground keyword :weight 'bold :strike-through nil))
    (diredfl-dir-priv               (:foreground keyword :weight 'bold))
    (diredfl-exec-priv              (:foreground diff-rem))
    (diredfl-executable-tag         (:foreground diff-rem))
    (diredfl-file-name              (:foreground fg1 :strike-through nil))
    (diredfl-file-suffix            (:foreground const))
    (diredfl-flag-mark              (:background selection :foreground diff-mod))
    (diredfl-flag-mark-line         (:background selection))
    (diredfl-ignored-file-name      (:inherit 'shadow))
    (diredfl-link-priv              (:foreground functions))
    (diredfl-no-priv                (:foreground comment))
    (diredfl-number                 (:foreground num))
    (diredfl-other-priv             (:foreground warning))
    (diredfl-rare-priv              (:foreground err :weight 'bold))
    (diredfl-read-priv              (:foreground diff-add))
    (diredfl-symlink                (:foreground functions))
    (diredfl-tagged-autofile-name   (:foreground num :underline underline))
    (diredfl-write-priv             (:foreground diff-mod))

    ;; Jabber
    (jabber-activity-face          (:foreground todo   :weight 'bold))
    (jabber-activity-personal-face (:foreground str  :weight 'bold))
    (jabber-chat-error             (:foreground err   :weight 'bold))
    (jabber-chat-prompt-foreign    (:foreground const   :weight 'bold))
    (jabber-chat-prompt-local      (:foreground builtin  :weight 'bold))
    (jabber-chat-prompt-system     (:foreground str-alt :weight 'bold))
    (jabber-chat-text-foreign      (:foreground fg1))
    (jabber-chat-text-local        (:foreground fg1))
    (jabber-rare-time-face         (:foreground str-alt))
    (jabber-roster-user-away       (:foreground warning))
    (jabber-roster-user-chatty     (:foreground done :weight 'bold))
    (jabber-roster-user-dnd        (:foreground err))
    (jabber-roster-user-error      (:foreground err))
    (jabber-roster-user-offline    (:foreground comment))
    (jabber-roster-user-online     (:foreground done :weight 'bold))
    (jabber-roster-user-xa         (:foreground num))
        
    ;; Highlighting
    (highlight                (:background hl-bg :foreground fg1))
    (lazy-highlight           (:background bg4 :foreground hl))
    (hl-line                  (:background hl-line))
    (highlight-numbers-number (:foreground num))
    (highlight-quoted-quote   (:inherit 'font-lock-builtin-face))
    (highlight-quoted-symbol  (:inherit 'font-lock-keyword-face))
    (highlight-symbol-face    (:background bg4))

    ;; Highlight indent guides
    (highlight-indent-guides-odd-face        (:background hl-indent))
    (highlight-indent-guides-even-face       (:background hl-indent))
    (highlight-indent-guides-character-face  (:foreground hl-indent))

    ;; Indent-guide
    (indent-guide-face (:foreground hl-indent))

    ;; Highlighting indentation
    (highlight-indentation-face                 (:background bg2))
    (highlight-indentation-current-column-face  (:background bg3))

    ;; Eldoc
    (eldoc-highlight-function-argument  (:inherit 'font-lock-constant-face))

    ;; Eldoc-box
    (eldoc-box-body (:background tooltip-bg))
    (eldoc-box-border (:background bg4))

    ;; Pulse
    (pulse-highlight-start-face (:background pulse))

    ;; Auto-dim-other-buffers
    (auto-dim-other-buffers-face  (:background dim-buffer))


    ;; Linum & nlinum
    (linum                        (:background line-num-bg :foreground line-num-fg :bold nil
                                               :italic nil :underline nil :strike-through nil))
    (linum-highlight-face          (:background line-num-bg :foreground line-num-hl :bold bold
                                                :italic nil :underline nil :strike-through nil))
    (nlinum-current-line          (:background line-num-bg :foreground line-num-hl :bold bold
                                               :italic nil :underline nil :strike-through nil))
    (linum-relative-current-line  (:inherit 'linum-highlight-face))
    (nlinum-relative-current-face (:inherit 'nlinum-current-line))

    ;; Native line numbers
    (line-number                  (:background line-num-bg :foreground line-num-fg :bold nil
                                               :italic nil :underline nil :strike-through nil))
    (line-number-current-line     (:background line-num-bg :foreground line-num-hl :bold bold
                                               :italic nil :underline nil :strike-through nil))

    ;; Which-function-mode
    (which-func (:foreground orange1))

    ;; Which-key
    (which-key-key-face                   (:foreground keysym :bold bold))
    (which-key-group-description-face     (:foreground violet4))
    (which-key-local-map-description-face (:foreground azure3))
    (which-key-command-description-face   (:foreground cyan3))

    ;; Ruler-mode
    (ruler-mode-default        (:background bg2 :foreground gray3))
    (ruler-mode-column-number  (:foreground var))
    (ruler-mode-current-column (:foreground orange1))
    (ruler-mode-fill-column    (:foreground pink1))
    (ruler-mode-comment-column (:foreground capri4))
    (ruler-mode-fringes        (:foreground teal1))
    (ruler-mode-pad            (:foreground var))
    (ruler-mode-tab-stop       (:foreground purple4))
    (ruler-mode-goal-column    (:foreground red0))

    ;; TODO: Message faces
    (message-header-name    (:foreground teal2))
    (message-header-subject (:foreground spring-green3))
    (message-header-to      (:foreground spring-green3))
    (message-header-other   (:foreground cyan3))

    ;; TODO: Elfeed
    (elfeed-search-tag-face          (:foreground amber3))
    (elfeed-search-feed-face         (:foreground teal1))
    (elfeed-search-date-face         (:foreground var))
    (elfeed-search-unread-title-face (:foreground fg1))
    (elfeed-search-unread-count-face (:foreground orange1))
    (elfeed-search-title-face        (:foreground comment))

    ;; Modeline
    (mode-line           (:background line-bg1 :foreground line-fg :bold nil
                                      :box (:line-width 2 :color line-border)))
    (mode-line-inactive  (:background line-bg1 :foreground line-inactive :bold nil
                                      :box (:line-width 2 :color line-border)))
    (mode-line-buffer-id (:background nil :foreground line-color2 :bold nil))
    (mode-line-highlight (:foreground hl :box nil :bold nil))
    (mode-line-emphasis  (:foreground hl))

    ;; Telephone-line
    (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground line-fg))
    (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground line-inactive))
    (telephone-line-evil            (:inherit 'mode-line))
    (telephone-line-evil-normal     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-normal))
    (telephone-line-evil-insert     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-insert))
    (telephone-line-evil-visual     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-visual))
    (telephone-line-evil-replace    (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-replace))
    (telephone-line-evil-motion     (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-motion))
    (telephone-line-evil-operator   (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-operator))
    (telephone-line-evil-emacs      (:inherit 'telephone-line-evil :background line-bg2 :foreground evil-emacs))
    (telephone-line-projectile      (:foreground var))

    ;; Doom-modeline
    (doom-modeline-bar                 (:background keyword))
    (doom-modeline-inactive-bar        (:background line-bg1))
    (doom-modeline-evil-normal-state   (:foreground evil-normal))
    (doom-modeline-evil-insert-state   (:foreground evil-insert))
    (doom-modeline-evil-visual-state   (:foreground evil-visual))
    (doom-modeline-evil-replace-state  (:foreground evil-replace))
    (doom-modeline-evil-motion-state   (:foreground evil-motion))
    (doom-modeline-evil-operator-state (:foreground evil-operator))
    (doom-modeline-evil-emacs-state    (:foreground evil-emacs))
    (doom-modeline-panel               (:background hl :foreground line-bg1))
    (doom-modeline-buffer-path         (:foreground var))
    (doom-modeline-buffer-major-mode   (:foreground str))

    ;; Powerline
    (powerline-active0           (:background line-bg2 :foreground line-color1))
    (powerline-active1           (:background line-bg2 :foreground line-color2))
    (powerline-active2           (:background line-bg1 :foreground line-color2))
    (powerline-inactive0         (:inherit 'mode-line-inactive))
    (powerline-inactive1         (:inherit 'mode-line-inactive))
    (powerline-inactive2         (:inherit 'mode-line-inactive))

    ;; ;; Spaceline
    ;; TODO:
    (spaceline-highlight-face (:background line-bg2 :foreground hl :bold nil))

    ;; Smart-mode-line
    (sml/line-number      (:foreground chartreuse1))
    (sml/modes            (:foreground magenta4))
    (sml/global           (:foreground cyan3))
    (sml/filename         (:foreground teal1))
    (sml/charging         (:foreground teal1))
    (sml/discharging      (:foreground red1))
    (sml/modified         (:foreground spring-green1))
    (sml/outside-modified (:background red0 :foreground fg1))
    (sml/prefix           (:foreground line-fg))
    (sml/read-only        (:foreground orange1))

    ;; Highlight TODOs
    (fic-face         (:background nil :foreground todo :bold bold))
    (fic-author-face  (:background nil :foreground todo :bold bold))
    (hl-todo          (:background nil :foreground todo :bold bold))

    ;; Additional completion
    (ac-completion-face    (:foreground keyword :underline underline))
    (icompletep-determined (:foreground builtin))

    ;; info faces
    (Info-quoted      (:foreground builtin))
    (info-quoted-name (:foreground builtin))
    (info-string      (:foreground str))
    (info-menu-star   (:foreground err))
    (info-index-match (:inherit 'match))
    (info-node        (:foreground functions))
    (info-menu-header (:foreground keyword :weight 'bold :height 1.1))
    (info-title-1     (:foreground header :weight 'bold :height 1.3))
    (info-title-2     (:foreground header :weight 'bold :height 1.2))
    (info-title-3     (:foreground header :weight 'bold :height 1.1))
    (info-title-4     (:foreground header :weight 'bold))

    ;; Helpful
    (helpful-heading (:foreground header :weight 'bold :height 1.1))

    ;; Company
    (company-tooltip                  (:background tooltip-bg :foreground tooltip-fg :bold bold))
    (company-tooltip-common           (:foreground hl :underline underline))
    (company-tooltip-common-selection (:foreground hl :underline underline))
    (company-tooltip-selection        (:background tooltip-hl-bg :foreground tooltip-hl-fg))
    (company-tooltip-mouse            (:background bg3 :foreground tooltip-hl-fg))
    (company-tooltip-annotation       (:foreground doc))
    (company-tooltip-search           (:background hl :foreground bg1 :distant-foreground fg1))
    (company-tooltip-search-selection (:background selection))
    (company-scrollbar-bg             (:background company-scroll-bg))
    (company-scrollbar-fg             (:background company-scroll-fg))
    (company-template-field           (:foreground comment :underline t))
    (company-echo-common              (:background bg1 :foreground diff-mod))
    (company-preview                  (:background nil :foreground keyword))
    (company-preview-common           (:background bg2 :foreground diff-mod))
    (company-preview-search           (:inherit 'company-tooltip-search))

    ;; all-the-icons
    (all-the-icons-red      (:foreground red1))
    (all-the-icons-red-alt  (:foreground red0))
    (all-the-icons-lred     (:foreground red3))
    (all-the-icons-dred     (:foreground red2))
    (all-the-icons-green    (:foreground spring-green1))
    (all-the-icons-lgreen   (:foreground spring-green3))
    (all-the-icons-dgreen   (:foreground spring-green2))
    (all-the-icons-yellow   (:foreground yellow3))
    (all-the-icons-lyellow  (:foreground amber3))
    (all-the-icons-dyellow  (:foreground orange2))
    (all-the-icons-orange   (:foreground vermilion1))
    (all-the-icons-lorange  (:foreground vermilion3))
    (all-the-icons-dorange  (:foreground vermilion2))
    (all-the-icons-blue     (:foreground cerulean3))
    (all-the-icons-blue-alt (:foreground capri1))
    (all-the-icons-lblue    (:foreground capri3))
    (all-the-icons-dblue    (:foreground capri2))
    (all-the-icons-maroon   (:foreground crimson1))
    (all-the-icons-lmaroon  (:foreground crimson3))
    (all-the-icons-dmaroon  (:foreground crimson2))
    (all-the-icons-purple   (:foreground violet1))
    (all-the-icons-lpurple  (:foreground violet3))
    (all-the-icons-dpurple  (:foreground purple2))
    (all-the-icons-cyan     (:foreground cyan1))
    (all-the-icons-cyan-alt (:foreground teal3))
    (all-the-icons-lcyan    (:foreground cyan3))
    (all-the-icons-dcyan    (:foreground cyan2))
    (all-the-icons-pink     (:foreground pink1))
    (all-the-icons-lpink    (:foreground pink3))
    (all-the-icons-dpink    (:foreground pink2))
    (all-the-icons-silver   (:foreground gray7))
    (all-the-icons-lsilver  (:foreground gray9))
    (all-the-icons-dsilver  (:foreground gray4))

    ;; Magit
    (magit-section-highlight         (:background bg2))
    (magit-section-heading           (:foreground keyword))
    (magit-section-heading-selection (:foreground button-hl :bold bold))
    (magit-item-highlight            (:background bg3))
    (magit-blame-heading             (:background bg3 :foreground var))

    (magit-branch                      (:foreground cyan1))
    (magit-branch-local                (:foreground cyan1))
    (magit-branch-remote               (:foreground aquamarine1))
    (magit-hunk-heading                (:background bg3))
    (magit-hunk-heading-highlight      (:background bg3))
    (magit-diff-hunk-heading           (:background bg3))
    (magit-diff-hunk-heading-highlight (:background bg4 :foreground header))
    ;; TODO:
    ;; (magit-diff-hunk-heading-selection (:background selection))
    (magit-diff-file-heading           (:foreground fg1 :bold bold))
    (magit-diff-file-heading-highlight (:background bg3 :bold bold))
    ;; TODO:
    ;; (magit-diff-file-heading-selection (:background selection))
    ;; TODO:
    (magit-diff-base              (:background vermilion3 :foreground fg2))
    (magit-diff-base-highlight    (:background vermilion3 :foreground fg1))
    (magit-diff-context           (:background bg1 :foreground fg3))
    (magit-diff-context-highlight (:background bg2 :foreground fg2))
    (magit-diff-added             (:background bg1 :foreground diff-bg-add))
    (magit-diff-added-highlight   (:background diff-bg-add :foreground fg0))
    (magit-diff-removed           (:background bg1 :foreground diff-bg-rem))
    (magit-diff-removed-highlight (:background diff-bg-rem :foreground fg0))
    (magit-diffstat-added         (:foreground diff-add))
    (magit-diffstat-removed       (:foreground diff-rem))
    (magit-tag                    (:foreground orange1))
    (magit-hash                   (:inherit 'magit-tag))
    (magit-dimmed                 (:inherit 'shadow))
    (magit-log-author             (:foreground prep))
    (magit-log-date               (:foreground var))
    (magit-log-graph              (:foreground str))

    (magit-process-ok             (:foreground done :bold bold))
    (magit-process-ng             (:foreground warning :bold bold))

    (magit-reflog-amend           (:foreground violet1))
    (magit-reflog-checkout        (:foreground capri3))
    (magit-reflog-cherry-pick     (:foreground spring-green1))
    (magit-reflog-commit          (:foreground spring-green1))
    (magit-reflog-merge           (:foreground spring-green1))
    (magit-reflog-rebase          (:foreground violet1))
    (magit-reflog-remote          (:foreground cyan1))
    (magit-reflog-reset           (:foreground err :bold bold))
    (magit-reflog-other           (:foreground cyan1))
    (magit-refname                (:foreground var))

    (magit-sequence-head (:foreground capri1))
    (magit-sequence-drop (:foreground red1))
    (magit-sequence-part (:foreground yellow1))
    (magit-sequence-stop (:inherit 'success))

    (magit-cherry-equivalent      (:foreground violet1))
    (magit-cherry-unmatched       (:foreground cyan1))
    (magit-bisect-good            (:foreground aquamarine1))
    (magit-bisect-bad             (:foreground red0))
    (magit-bisect-skip            (:foreground lime1))
    (magit-signature-good         (:foreground spring-green3))
    (magit-signature-bad          (:foreground red0))
    (magit-signature-untrusted    (:foreground cyan1))

    (magit-popup-key              (:foreground keysym))

    ;; Magit Transient
    (transient-heading         (:foreground header))
    (transient-key             (:foreground keysym))
    (transient-argument        (:foreground hl))
    (transient-enabled-suffix  (:background done :foreground bg1))
    (transient-disabled-suffix (:background err :foreground bg1))

    ;; Flymake
    (flymake-note    (:underline (:style underline-style :color done)))
    (flymake-warning (:underline (:style underline-style :color warning)))
    (flymake-error   (:underline (:style underline-style :color err)))

    ;; Flycheck
    (flycheck-info           (:underline (:style underline-style :color done)))
    (flycheck-warning        (:underline (:style underline-style :color warning)))
    (flycheck-error          (:underline (:style underline-style :color err)))
    (flycheck-fringe-error   (:foreground err))
    (flycheck-fringe-warning (:foreground warning))
    (flycheck-fringe-info    (:foreground done))

    ;; Flycheck posframe
    (flycheck-posframe-face            (:inherit 'default))
    (flycheck-posframe-background-face (:background tooltip-bg))
    (flycheck-posframe-info-face       (:inherit 'flycheck-posframe-face :foreground tooltip-fg))
    (flycheck-posframe-warning-face    (:inherit 'flycheck-posframe-face :foreground warning))
    (flycheck-posframe-error-face      (:inherit 'flycheck-posframe-face :foreground err))

    ;; Flyspell
    (flyspell-duplicate (:underline (:style underline-style :color warning)))
    (flyspell-incorrect (:underline (:style underline-style :color err)))

    ;; Hydra
    ;; (hydra-face-red      (:foreground red1))
    (hydra-face-red      (:foreground err))
    (hydra-face-teal     (:foreground teal3))
    (hydra-face-blue     (:foreground azure3))
    (hydra-face-pink     (:foreground pink1))
    (hydra-face-amaranth (:foreground purple3))

    ;; Hydra-posframe
    (hydra-posframe-face (:background bg2 :foreground fg1))
    (hydra-posframe-border-face (:background bg2))

    ;; Ido
    (ido-indicator   (:foreground num))
    (ido-first-match (:foreground hl :bold bold))
    (ido-only-match  (:foreground cyan1))
    (ido-subdir      (:foreground violet4))

    ;; Gnus
    (gnus-header-content (:foreground keyword))
    (gnus-header-from    (:foreground var))
    (gnus-header-name    (:foreground type))
    (gnus-header-subject (:foreground functions :bold bold))

    ;; Mu4e
    (mu4e-header-marks-face    (:foreground type))
    (mu4e-view-url-number-face (:foreground type))
    (mu4e-cited-1-face         (:foreground fg2))
    (mu4e-cited-7-face         (:foreground fg3))

    ;; ffap
    (ffap (:foreground fg4))

    ;; Slime
    (slime-repl-inputed-output-face (:foreground type))

    ;; Js-mode
    (js2-private-function-call    (:foreground const))
    (js2-jsdoc-html-tag-delimiter (:foreground str))
    (js2-jsdoc-html-tag-name      (:foreground keyword))
    (js2-external-variable        (:foreground type))
    (js2-function-param           (:foreground const))
    (js2-error                    (:underline (:color err :style underline-style)))
    (js2-function-call            (:foreground functions))
    (js2-object-property          (:foreground num))
    (js2-jsdoc-value              (:foreground str))
    (js2-private-member           (:foreground fg3))
    (js3-function-param-face      (:foreground keyword))
    (js3-instance-member-face     (:foreground const))
    (js3-external-variable-face   (:foreground var))
    (js3-jsdoc-tag-face           (:foreground keyword))
    (js3-warning-face             (:underline keyword))
    (js3-error-face               (:underline err))

    ;; Latex
    (font-latex-bold-face                (:foreground type))
    (font-latex-italic-face              (:foreground keyword :italic italic))
    (font-latex-string-face              (:foreground str))
    (font-latex-match-reference-keywords (:foreground const))
    (font-latex-match-variable-keywords  (:foreground var))

    ;; Rst-mode
    (rst-adornment (:foreground comment))
    (rst-block     (:foreground functions))
    (rst-level-1   (:foreground keyword))
    (rst-level-2   (:foreground builtin))
    (rst-level-3   (:foreground num))
    (rst-level-4   (:foreground const))
    (rst-level-5   (:foreground type))
    (rst-level-6   (:foreground keyword))

    ;; Latex/Auctex
    (font-latex-warning-face      (:inherit 'warning))
    (font-latex-string-face       (:inherit 'font-lock-string-face))
    ;; TODO: change colors to vars
    (font-latex-math-face         (:foreground purple4))
    (font-latex-sedate-face       (:foreground capri4))
    (font-latex-script-char-face  (:foreground purple4))
    (font-latex-sectioning-0-face (:foreground amber3 :bold bold))
    (font-latex-sectioning-1-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-2-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-3-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-4-face (:inherit 'font-latex-sectioning-0-face))
    (font-latex-sectioning-5-face (:inherit 'font-latex-sectioning-0-face))

    ;; Undo-tree
    (undo-tree-visualizer-active-branch-face (:foreground functions :bold bold))
    (undo-tree-visualizer-current-face       (:foreground hl))
    (undo-tree-visualizer-default-face       (:foreground fg2))
    (undo-tree-visualizer-unmodified-face    (:foreground done))
    (undo-tree-visualizer-register-face      (:foreground type))

    ;; Rainbow delimeters
    ;; TODO: light themes
    (show-paren-match (:background bg2 :foreground rb-match :bold bold))
    (show-paren-mismatch (:background red2 :foreground bg2))

    (rainbow-delimiters-mismatched-face (:background red2 :foreground err))
    (rainbow-delimiters-unmatched-face (:inherit 'rainbow-delimiters-mismatched-face))
    (rainbow-delimiters-base-face    (:foreground rb1))
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
    (diff-context           (:foreground comment))
    (diff-header            (:background bg4))
    (diff-function          (:background bg4 :foreground functions))
    (diff-nonexistent       (:foreground err))
    (diff-hunk-header       (:background bg4))
    (diff-file-header       (:background nil :foreground keyword))
    (diff-added             (:background diff-bg-add :foreground fg1))
    (diff-changed           (:background diff-bg-mod :foreground fg1))
    (diff-removed           (:background diff-bg-rem :foreground fg1))
    (diff-refine-added      (:background diff-add :foreground fg0))
    (diff-refine-changed    (:background diff-mod :foreground fg1))
    (diff-refine-removed    (:background diff-rem :foreground fg1))
    (diff-indicator-added   (:background bg1 :foreground diff-add))
    (diff-indicator-changed (:background bg1 :foreground diff-mod))
    (diff-indicator-removed (:background bg1 :foreground diff-rem))

    ;; smerge
    (smerge-base    (:background bg2))
    (smerge-upper   (:background diff-bg-add))
    (smerge-lower   (:background diff-bg-rem))
    (smerge-markers (:background comment :foreground bg1))
    ;; Emacs version <= 25
    (smerge-mine    (:background diff-bg-add))
    (smerge-other   (:background diff-bg-rem))
    ;; ??
    ;; smerge-refined-added
    ;; smerge-refined-removed

   ;; Ediff
   ;; (ediff-current-diff-A (:background hl-line :foreground fg4))
   ;; (ediff-current-diff-B (:background hl-line :foreground fg4))
   ;; (ediff-current-diff-C (:background hl-line :foreground fg4))
   (ediff-current-diff-Ancestor (:background diff-bg-mod :foreground fg2))
   (ediff-current-diff-A (:background red2 :foreground fg2))
   (ediff-current-diff-B (:background spring-green2 :foreground fg2))
   (ediff-current-diff-C (:background cyan2 :foreground fg2))

   (ediff-even-diff-Ancestor (:background bg3))
   (ediff-even-diff-A (:background bg3))
   (ediff-even-diff-B (:background bg3))
   (ediff-even-diff-C (:background bg3))

   (ediff-fine-diff-Ancestor (:background diff-bg-mod :bold bold :foreground white0))
   (ediff-fine-diff-A (:background red3 :bold bold :foreground white0))
   (ediff-fine-diff-B (:background spring-green1 :bold bold :foreground white0))
   (ediff-fine-diff-C (:background cyan1 :bold bold :foreground white0))

   (ediff-odd-diff-Ancestor (:background bg4))
   (ediff-odd-diff-A (:background bg4))
   (ediff-odd-diff-B (:background bg4))
   (ediff-odd-diff-C (:background bg4))

   ;; calfw
   (cfw:face-grid (:foreground comment))
   (cfw:face-title (:foreground prep :weight 'bold :height 1.8))
   (cfw:face-default-content (:foreground fg1))
   (cfw:face-day-title (:background bg3 :foreground fg1))
   (cfw:face-today (:background bg3 :foreground prep))
   (cfw:face-today-title (:background todo :foreground bg1))
   (cfw:face-saturday (:background bg3 :foreground todo))
   (cfw:face-sunday (:background bg3 :foreground todo))
   (cfw:face-holiday (:background bg3 :foreground functions))
   (cfw:face-periods (:foreground num))
   (cfw:face-header (:background bg3 :foreground str))
   (cfw:face-annotation (:foreground doc))
   (cfw:face-select (:background hl :foreground bg1))
   (cfw:face-toolbar (:background nil :foreground nil))
   (cfw:face-toolbar-button-on (:background nil :foreground hl))
   (cfw:face-toolbar-button-off (:foreground fg4))
   (cfw:face-disable (:background bg3 :foreground comment))

    ;; Imenu list
    ;; (imenu-list-entry-subalist-face-0 (:inherit 'font-lock-keyword-face))
    (imenu-list-entry-face   (:inherit 'font-lock-keyword-name-face))
    (imenu-list-entry-face-0 (:inherit 'font-lock-keyword-face :height 1.0))
    (imenu-list-entry-face-1 (:inherit 'font-lock-function-name-face))
    (imenu-list-entry-face-2 (:inherit 'font-lock-string-face))
    (imenu-list-entry-face-3 (:inherit 'font-lock-type-face))
    (imenu-list-entry-subalist-face-0 (:inherit 'imenu-list-entry-face-0 :bold bold))
    (imenu-list-entry-subalist-face-1 (:inherit 'imenu-list-entry-face-1 :bold bold))
    (imenu-list-entry-subalist-face-2 (:inherit 'imenu-list-entry-face-2 :bold bold))
    (imenu-list-entry-subalist-face-3 (:inherit 'imenu-list-entry-face-3 :bold bold))

    ;; Treemacs
    (treemacs-root-face             (:foreground keyword :height 1.2 :underline nil))
    (treemacs-directory-face        (:foreground functions))
    (treemacs-git-modified-face     (:foreground diff-mod))
    (treemacs-fringe-indicator-face (:foreground prep))
    (treemacs-tags-face             (:foreground fg1))
    (treemacs-on-success-pulse-face (:background diff-bg-add :foreground bg1))
    (treemacs-on-failure-pulse-face (:background err :foreground bg1))
    (treemacs-term-node-face        (:foreground prep))

    ;; Neotree
    (neo-root-dir-face    (:foreground keyword :underline nil))
    (neo-dir-link-face    (:foreground functions))
    (neo-file-link-face   (:foreground fg1))
    (neo-expand-btn-face  (:foreground hl))
    (neo-vc-added-face    (:foreground diff-add))
    (neo-vc-edited-face   (:foreground diff-mod))
    (neo-vc-removed-face  (:foreground diff-rem :strike-through t))
    (neo-vc-conflict-face (:foreground err))
    (neo-vc-missing-face  (:foreground warning))
    (neo-vc-ignored-face  (:foreground comment))

    ;; Git gutter
    (git-gutter:unchanged (:background bg1 :foreground nil))
    (git-gutter:added     (:background bg1 :foreground diff-add :bold bold))
    (git-gutter:modified  (:background bg1 :foreground diff-mod :bold bold))
    (git-gutter:deleted   (:background bg1 :foreground diff-rem :bold bold))

    ;; Diff-hl
    (diff-hl-insert (:background diff-add))
    (diff-hl-change (:background diff-mod))
    (diff-hl-delete (:background diff-rem))
    (diff-hl-margin-insert (:background diff-add :foreground bg1 :slant 'normal))
    (diff-hl-margin-change (:background diff-mod :foreground bg1 :slant 'normal))
    (diff-hl-margin-delete (:background diff-rem :foreground bg1 :slant 'normal))

    ;; Popup
    (popup-face                (:background tooltip-bg :foreground tooltip-fg :bold bold))
    (popup-menu-selection-face (:background tooltip-hl-bg :foreground tooltip-hl-fg :bold bold))
    (popup-tip-face            (:background tooltip-hl-bg :foreground fg1 :bold bold))

    ;; TODO: mb add term0-16 vars
    ;; Terminal
    (term               (:background bg1 :foreground fg1))
    (term-color-black   (:foreground black1))
    (term-color-blue    (:foreground azure3))
    (term-color-red     (:foreground red3))
    (term-color-green   (:foreground teal0))
    (term-color-yellow  (:foreground yellow3))
    (term-color-magenta (:foreground magenta3))
    (term-color-cyan    (:foreground cyan3))
    (term-color-white   (:foreground white2))
    (term-underline     (:inherit 'underline))

    ;; EShell
    (eshell-prompt        (:foreground teal0 :bold bold))
    (eshell-ls-directory  (:foreground blue1 :bold bold))
    (eshell-ls-symlink    (:foreground azure3 :bold bold))
    (eshell-ls-executable (:foreground chartreuse1 :bold bold))
    (eshell-ls-archive    (:foreground red3))
    (eshell-ls-backup     (:foreground magenta3))
    (eshell-ls-clutter    (:foreground pink1))
    (eshell-ls-missing    (:background bg3 :foreground red3))
    (eshell-ls-product    (:foreground yellow3))
    (eshell-ls-readonly   (:foreground fg2))
    (eshell-ls-special    (:foreground spring-green3))
    (eshell-ls-unreadable (:foreground var))

    ;; Whitespace mode
    (whitespace-empty            (:background bg4))
    (whitespace-line             (:background bg1 :foreground num))
    (whitespace-indentation      (:background hl-indent :foreground hl-indent))
    (whitespace-tab              (:background bg2 :foreground comment))
    (whitespace-space            (:background bg2 :foreground comment))
    (whitespace-newline          (:inherit 'whitespace-space))
    (whitespace-space-before-tab (:background diff-mod :foreground bg2))
    (whitespace-space-after-tab  (:background diff-mod :foreground bg2))
    ;; (whitespace-hspace           (:foreground num))
    (whitespace-trailing         (:inherit 'trailing-whitespace :foreground bg2))
    (whitespace-big-indent       (:background red0 :foreground bg2))

    ;; Org-mode
    (org-todo                      (:foreground todo :bold bold))
    (org-done                      (:foreground done  :bold bold))
    (org-headline-done             (:foreground gray4  :bold nil))
    (org-ellipsis                  (:foreground builtin :underline nil))
    (org-date                      (:foreground builtin :underline underline))
    (org-date-selected             (:background bg4 :foreground hl :weight 'bold))
    (org-link                      (:inherit 'link))
    (org-code                      (:foreground amber3))
    (org-verbatim                  (:foreground azure3))
    (org-hide                      (:foreground bg1))
    (org-special-keyword           (:foreground prep))
    (org-table                     (:background bg2 :foreground fg3))
    (org-formula                   (:background nil :foreground type))
    (org-warning                   (:foreground warning :underline underline))
    (org-tag                       (:foreground prep))
    (org-checkbox                  (:foreground functions))

    (org-document-info-keyword     (:foreground metakey))
    (org-meta-line                 (:inherit 'org-document-info-keyword))
    (org-block                     (:background bg2 :foreground fg2))
    (org-block-begin-line          (:background bg2 :foreground comment :height 0.9))
    (org-block-end-line            (:inherit 'org-block-begin-line))
    (org-list-dt                   (:inherit 'org-checkbox))
    (org-document-title            (:foreground builtin :bold bold :height 1.2))
    (org-document-info             (:foreground builtin))
    (org-footnote                  (:foreground fg4 :underline underline))
    (org-quote                     (:background bg2 :foreground fg2 :italic italic))
    (org-verse                     (:foreground fg3 :italic italic))

    (org-level-1            (:foreground keyword :bold bold :height 1.1))
    (org-level-2            (:foreground builtin  :bold nil))
    (org-level-3            (:foreground num :bold nil))
    (org-level-4            (:foreground const :bold nil))


    ;; org-agenda
    (org-agenda-dimmed-todo-face (:foreground comment))
    (org-agenda-date             (:foreground fg1))
    (org-agenda-date-today       (:foreground prep :bold bold))
    (org-agenda-date-weekend     (:foreground warning))
    (org-agenda-done             (:foreground done))
    (org-agenda-structure        (:foreground builtin))
    (org-agenda-clocking         (:background hl-bg :foreground fg1))
    (org-scheduled               (:foreground fg1))
    (org-scheduled-today         (:foreground functions :bold bold))
    (org-sexp-date               (:foreground fg4))
    (org-time-grid               (:foreground comment))

    ;; Emmet
    (emmet-preview-input   (:foreground nil :background nil))
    (emmet-preview-output  (:foreground nil :background nil))

    ;; Flx
    (flx-highlight-face (:foreground hl :underline underline))

    ;; Smartparens
    ;; TODO:
    (sp-pair-overlay-face (:foreground nil))

    ;; Web-mode
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
    (web-mode-doctype-face          (:inherit 'font-lock-preprocessor-face))
    (web-mode-function-name-face    (:inherit 'font-lock-function-name-face))
    (web-mode-string-face           (:inherit 'font-lock-string-face))
    (web-mode-warning-face          (:inherit 'font-lock-warning-face))

    ;; Nim
    (nim-font-lock-export-face (:inherit 'font-lock-function-name-face :italic nil))

    ;; Evil ex
    (evil-ex-info                   (:foreground orange1))
    (evil-ex-substitute-matches     (:background nil :foreground err :underline underline))
    (evil-ex-substitute-replacement (:background nil :foreground spring-green1))
    (evil-ex-lazy-highlight         (:inherit 'lazy-highlight))

    ;; Evil-goggles
    ;; TODO: add rest of evil goggles faces
    (evil-goggles-default-face  (:background pulse))

    ;; Evil-snipe
    (evil-snipe-first-match-face (:foreground search1))
    (evil-snipe-matches-face     (:foreground search2))

    ;; Beacon-mode
    (beacon-fallback-background (:background pulse))

    ;; Solaire mode
    (solaire-default-face    (:inherit 'default :background pane))
    (solaire-minibuffer-face (:inherit 'solaire-default-face))
    (solaire-hl-line-face    (:inherit 'hl-line :background bg3))
    (solaire-org-hide-face   (:inherit 'org-hide))

    ;; Tuareg/OCaml
    (tuareg-font-double-colon-face            (:foreground warning))
    (tuareg-font-lock-governing-face          (:foreground keyword))
    ;; TODO maybe str or prep, num, functions
    (tuareg-font-lock-operator-face           (:foreground builtin))
    (tuareg-font-lock-error-face              (:inherit 'error))
    (tuareg-font-lock-multistage-face         (:inherit 'font-lock-preprocessor-face))
    (tuareg-font-lock-interactive-error-face  (:inherit 'error))
    (tuareg-font-lock-interactive-output-face (:inherit 'default))

    ;; Markdown
    (markdown-bold-face               (:inherit 'bold))
    (markdown-italic-face             (:inherit 'italic))
    (markdown-header-face             (:foreground header :bold 'bold))
    (markdown-header-delimiter-face   (:inherit 'markdown-header-face))
    (markdown-metadata-key-face       (:foreground metakey))
    (markdown-metadata-value-face     (:foreground doc))
    (markdown-markup-face             (:foreground functions))
    (markdown-list-face               (:foreground functions))
    (markdown-pre-face                (:foreground str))
    (markdown-code-face               (:background bg2))
    (markdown-inline-code-face        (:inherit '(markdown-code-face markdown-pre-face)))
    (markdown-link-face               (:inherit 'link))
    (markdown-url-face                (:foreground str))
    (markdown-blockquote-face         (:foreground fg4))
    (markdown-reference-face          (:foreground doc))
    (markdown-language-keyword-face   (:foreground type))
    (markdown-html-tag-name-face      (:inherit 'font-lock-keyword-face))
    (markdown-html-tag-delimiter-face (:inherit 'web-mode-html-tag-face))
    (markdown-html-entity-face        (:inherit 'font-lock-variable-name-face))
    (markdown-html-attr-name-face     (:inherit 'font-lock-function-name-face))
    (markdown-html-attr-value-face    (:inherit 'font-lock-string-face))

    ;; Shell script
    (sh-escaped-newline (:inherit 'font-lock-string-face))
    (sh-heredoc         (:foreground doc))
    (sh-quoted-exec     (:foreground num))

    ;; Helm
    (helm-M-x-key                             (:foreground keysym))
    (helm-action                              (:foreground fg1 :underline underline))
    (helm-header                              (:inherit 'header-line))
    (helm-header-line-left-margin             (:inherit 'header-line))
    (helm-helper                              (:foreground keysym))
    (helm-source-header                       (:background bg2 :foreground str :underline nil :bold bold))
    (helm-match                               (:foreground search1 :bold bold))
    (helm-selection                           (:background bg3 :foreground hl :bold bold))
    (helm-selection-line                      (:inherit 'helm-selection))
    (helm-visible-mark                        (:background diff-bg-add :foreground bg1))
    (helm-candidate-number                    (:foreground str))
    (helm-separator                           (:foreground type))

    (helm-bookmark-addressbook                (:foreground prep))
    (helm-bookmark-directory                  (:foreground keyword))
    (helm-bookmark-file                       (:foreground fg1))
    (helm-bookmark-gnus                       (:foreground search2))
    (helm-bookmark-info                       (:foreground str))
    (helm-bookmark-man                        (:foreground doc))
    (helm-bookmark-w3m                        (:foreground search1))

    (helm-buffer-process                      (:foreground str))
    (helm-buffer-saved-out                    (:background diff-bg-mod :foreground bg1))
    (helm-buffer-size                         (:foreground fg1))
    (helm-buffer-directory                    (:foreground keyword))
    (helm-buffer-archive                      (:foreground const))
    (helm-buffer-not-saved                    (:foreground diff-mod))
    (helm-buffer-modified                     (:foreground warning))

    (helm-etags-file                          (:foreground str))
    (helm-ff-directory                        (:foreground keyword :bold bold))
    (helm-ff-dotted-directory                 (:inherit 'helm-ff-directory))
    (helm-ff-symlink                          (:foreground functions :bold bold))
    (helm-ff-dotted-symlink-directory         (:inherit 'helm-ff-symlink))
    (helm-ff-file                             (:foreground fg1 :weight 'normal))
    (helm-ff-executable                       (:foreground num :weight 'normal))
    (helm-ff-invalid-symlink                  (:inherit 'error))
    (helm-ff-prefix                           (:foreground keyword))
    (helm-ff-denied                           (:inherit 'error :underline underline))

    (helm-grep-cmd-line                       (:foreground fg1))
    (helm-grep-file                           (:foreground str))
    (helm-grep-finish                         (:foreground done))
    (helm-grep-lineno                         (:foreground line-num-fg))
    (helm-grep-match                          (:inherit 'helm-match))
    (helm-grep-running                        (:foreground functions))
    (helm-locate-finish                       (:foreground done))
    (helm-moccur-buffer                       (:foreground functions))
    (helm-resume-need-update                  (:background err :foreground nil))
    (helm-lisp-completion-info                (:foreground doc))
    (helm-lisp-show-completion                (:background selection :foreground fg4 :underline t))
    (helm-prefarg                             (:foreground prep))
    (helm-mode-prefix                         (:background err :foreground bg1))

    (helm-time-zone-current                   (:foreground builtin))
    (helm-time-zone-home                      (:foreground type))
    (helm-source-go-package-godoc-description (:foreground str))

    ;; helm-swoop
    (helm-swoop-line-number-face       (:background nil :foreground line-num-fg))
    (helm-swoop-target-line-face       (:inherit 'isearch))
    (helm-swoop-target-line-block-face (:inherit 'isearch))
    (helm-swoop-target-word-face       (:background num :foreground bg1 :bold bold))


    ;; Wgrep
    (wgrep-face        (:background bg2 :foreground diff-mod))
    (wgrep-delete-face (:background diff-bg-rem :foreground bg2))
    (wgrep-done-face   (:foreground done))
    (wgrep-file-face   (:foreground comment))
    (wgrep-reject-face (:inherit 'error))

     ;; Avy
    (avy-background-face (:inherit 'font-lock-comment-face))
    (avy-lead-face       (:background spring-green2 :foreground adaptive-fg :weight 'bold))
    (avy-lead-face-0     (:background red2 :foreground adaptive-fg :weight 'bold))
    (avy-lead-face-1     (:background magenta2 :foreground adaptive-fg :weight 'bold))
    (avy-lead-face-2     (:background capri2 :foreground adaptive-fg :weight 'bold))

    ;; Ace-window
    (aw-leading-char-face (:background nil :foreground hl :bold bold :height 1.4))
    (aw-background-face   (:foreground comment :bold bold))
    (aw-key-face          (:foreground keysym :bold bold))

    ;; Ivy
    (ivy-confirm-face            (:inherit 'success))
    (ivy-current-match           (:background hl-line :foreground hl :bold t))
    (ivy-cursor                  (:background bg3 :foreground fg1))
    (ivy-grep-info               (:foreground header))
    (ivy-highlight-face          (:background nil :foreground functions))
    (ivy-match-required-face     (:background nil :foreground err :bold nil))
    (ivy-modified-buffer         (:foreground diff-mod))
    (ivy-remote                  (:foreground prep))
    (ivy-subdir                  (:foreground keyword :bold bold))
    (ivy-virtual                 (:foreground ivy1))
    (ivy-minibuffer-match-face-1 (:background nil :foreground ivy1))
    (ivy-minibuffer-match-face-2 (:background nil :foreground search1 :bold bold))
    (ivy-minibuffer-match-face-3 (:background nil :foreground search2 :bold bold))
    (ivy-minibuffer-match-face-4 (:background nil :foreground search3 :bold bold))

    ;; Ivy posframe
    (ivy-posframe (:background tooltip-bg))
    (ivy-posframe-border (:background bg4))

    ;; Counsel
    (counsel-key-binding (:foreground keysym))

    ;; Swiper
    (swiper-match-face-1 (:background bg2 :foreground ivy1))
    (swiper-match-face-2 (:background bg2 :foreground search1 :bold bold))
    (swiper-match-face-3 (:background bg2 :foreground search2 :bold bold))
    (swiper-match-face-4 (:background bg2 :foreground search3 :bold bold))
    (swiper-background-match-face-1 (:inherit 'swiper-match-face-1 :bold nil :background bg1))
    (swiper-background-match-face-2 (:inherit 'swiper-match-face-2 :bold nil :background bg1))
    (swiper-background-match-face-3 (:inherit 'swiper-match-face-3 :bold nil :background bg1))
    (swiper-background-match-face-4 (:inherit 'swiper-match-face-4 :bold nil :background bg1))
    (swiper-line-face    (:inherit 'hl-line))

    ;; tabbar
    (tabbar-default             (:background bg1 :foreground bg1 :height 1.0))
    (tabbar-highlight           (:background hl-bg :foreground fg1 :distant-foreground bg1))
    (tabbar-button              (:foreground fg1 :background bg1))
    (tabbar-button-highlight    (:foreground bg1 :background fg4 ))
    (tabbar-modified            (:inherit 'tabbar-default :foreground diff-mod :weight 'bold))
    (tabbar-unselected          (:inherit 'tabbar-default :foreground comment))
    (tabbar-unselected-modified (:inherit 'tabbar-modified))
    (tabbar-selected            (:inherit 'tabbar-default :foreground fg1 :background bg2 :weight 'bold))
    (tabbar-selected-modified   (:inherit 'tabbar-selected :foreground diff-add))

    ;; Awesome-tabs
    (awesome-tab-default    (:background bg1 :foreground bg1))
    (awesome-tab-unselected (:background bg2 :foreground comment))
    (awesome-tab-selected   (:background bg3 :foreground hl))

    ;; Centaur-tabs
    (centaur-tabs-default    (:background bg0 :foreground bg0))
    (centaur-tabs-selected   (:background bg1 :foreground fg1))
    (centaur-tabs-unselected (:background bg0 :foreground comment))
    (centaur-tabs-selected-modified   (:background bg1 :foreground todo))
    (centaur-tabs-unselected-modified (:background bg0 :foreground todo))
    (centaur-tabs-active-bar-face (:background keyword))
    (centaur-tabs-modified-marker-selected (:inherit 'centaur-tabs-selected :foreground keyword))
    (centaur-tabs-modified-marker-unselected (:inherit 'centaur-tabs-unselected :foreground keyword))))

(provide 'kaolin-themes-lib)

;;; kaolin-themes-lib.el ends here
