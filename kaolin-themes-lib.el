;;; kaolin-themes-lib.el --- Kaolin-themes library, provides common parts for the package  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Color order
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

;;; Color list
;;
;; Color name - color hex - Kaolin hue
;;
;; Black #020203 - 240
;; Gray #CED8D9 - 189
;; White #FDFDFF - 240
;;
;; TODO: (??) change strict colors such as erin
;; Yellow #FFFF00 - 60
;; Amber #FFBF00 - 43
;; Orange #FF7F00 - 36
;; Vermilion #FF3F00 - 17
;; Brown #A33C28 - 6
;; Red #FF0000 - 358
;; Crimson #FF003F - 347
;; Pink/rose #FF007F - 335
;; Cerise #FF00BF - 315
;; Magenta/Fuchsia #FF00FF - 300
;; Purple #BF00FF - 279
;; Violet #7F00FF - 270
;; Ultramarine #3F00FF - 254
;; Blue #0000FF - 238
;; Cerulean #003FFF - 221
;; Azure/Sky Blue #007FFF - 210
;; Capri/Deep Sky Blue #00BFFF - 201
;; Cyan #00FFFF - 182
;; Teal #00A89D - 178
;; Aquamarine #00FFBF - 163
;; Spring-green #00FF7F - 150
;; Erin #00FF3F - 135
;; Green #00FF00 - 120
;; Harlequin #3FFF00 - 104
;; Chartreuse #7FFF00 - 86
;; Lime #BFFF00 - 75

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
    ;; TODO: adjust
    (yellow0 "#eeeb28")
    (yellow1 "#b9b963") ; yellow
    (yellow2 "#919120") ; dark-yellow
    (yellow3 "#eae46a")
    (yellow4 "#c8c493" "#ffd7a5")
    (yellow5 "#1e1e14") ; old midnight yellow
    (yellow6 "#40402E")
    (yellow7 "#848468")
    (yellow8 "#c5c5a5")
    (yellow9 "#EEEED3")

    ;; Amber #FFBF00
    (amber0 "#f3c91f")
    (amber1 "#d4b668")
    (amber2 "#91762a")
    (amber3 "#eed891")
    (amber4 "#c5b378")
    (amber5 "#1e1c14")
    (amber6 "#403B2E")
    (amber7 "#847C68")
    (amber8 "#c7c2af")
    (amber9 "#EEE6D3")

    ;; Orange #FF7F00
    (orange0 "#e67417")
    (orange1 "#dbac66")
    (orange2 "#b87e3c")
    (orange3 "#f5c791")
    (orange4 "#e1b079")
    (orange5 "#1e1914")
    (orange6 "#40392E")
    (orange7 "#847968")
    (orange8 "#c2b4a1") ; grayish-orange
    (orange9 "#EEE6D3")

    ;; Vermilion #FF3F00
    (vermilion0 "#fa5016")
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
    (crimson0 "#dc2e58")
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
    (pink6 "#402E35")
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
    (magenta1 "#cd5ccd")
    (magenta2 "#563d56")
    (magenta3 "#cea2ca") ; light-puprle
    (magenta4 "#835d83") ; purple
    (magenta5 "#1a121a") ; old midnight-purple
    (magenta6 "#402E40")
    (magenta7 "#846884")
    (magenta8 "#BFA8BF")
    (magenta9 "#EED3EE")

    ;; Purple #BF00FF
    (purple0 "#ab33eb")
    (purple1 "#A34BD2")
    (purple2 "#73229E")
    (purple3 "#bc90d4")
    (purple4 "#ab98b5")
    (purple5 "#1f1623")
    (purple6 "#392E40")
    (purple7 "#7A6884")
    (purple8 "#bcacbf")
    (purple9 "#E6D3EE")

    ;; Violet #7F00FF
    (violet0 "#7f1de1")
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
    ;; TODO adjust
    (ultramarine0 "#4618dc")
    (ultramarine1 "#5f3eca")
    (ultramarine2 "#40249C")
    (ultramarine3 "#6d44eb")
    (ultramarine4 "#787096")
    (ultramarine5 "#16141e")
    ;; (ultramarine6 "#322E40")
    (ultramarine6 "#2D2C58")
    (ultramarine7 "#6E6884")
    (ultramarine8 "#b0acc5")
    (ultramarine9 "#DBD3EE")

    ;; Blue #0000FF
    (blue0 "#3237CA")
    (blue1 "#4145b6")
    (blue2 "#2B2FA6")
    (blue3 "#525df3")
    (blue4 "#807f96") ; old faded-blue
    (blue5 "#14141e" black2) ; old alt-midnight-blue
    (blue6 "#2E2E40")
    (blue7 "#686984")
    (blue8 "#A1A0C5")
    (blue9 "#D3D7EE")

    ;; Cerulean #003FFF
    (cerulean0 "#0e4cd1")
    (cerulean1 "#3f66ba")
    (cerulean2 "#2d4b8c")
    (cerulean3 "#4c7de8")
    (cerulean4 "#536a9d")
    (cerulean5 "#14171e")
    (cerulean6 "#2E3340")
    (cerulean7 "#687184") ; old grayish-blue
    (cerulean8 "#8F97A7")
    (cerulean9 "#C6D5E8")

    ;; Azure #007FFF
    (azure0 "#0e70d1")
    (azure1 "#3f7dba") ; old blue
    (azure2 "#2a4661")
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
    (teal0 "#0d948d")
    (teal1 "#4d9391")
    (teal2 "#396b68")
    (teal3 "#49bdb0")
    (teal4 "#80bcb6")
    (teal5 "#141e1d")
    (teal6 "#2E403F")
    (teal7 "#688483")
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
    (spring-green1 "#51b884")
    (spring-green2 "#39855f") ; dark
    (spring-green3 "#65E6A7")
    (spring-green4 "#597a6c") ; faded
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
    (green3 "#61e361")
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
    (chartreuse0 "#8bee1a")
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

    (dim-buffer bg0)

    (hl         aquamarine3)
    (hl-mono    gray4)
    (hl-line    (if kaolin-themes-hl-line-colored capri5 bg2))
    (hl-indent  gray3)
    (selection  bg3)
    (pulse      spring-green6)

    (todo red1)
    (done spring-green3)

    (button orange8)
    (button-hl amber3)

    (tooltip-bg bg2)
    (tooltip-fg fg2)
    (tooltip-hl-bg brown2)
    (tooltip-hl-fg amber3)

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
    (diff-bg-mod purple3)
    (diff-bg-rem crimson4)

    (comment     gray3)
    (alt-comment azure8)
    (keyword     teal1)
    (second-key  comment)
    (builtin     teal4)
    (functions   builtin)
    ;; TODO: (??) change to brown3 like sierra.vim
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
    (warning     orange1)
    (err         red1)

    (keysym      prep)
    (prompt      keyword)

    ;; Mode-line
    (line-fg           fg4)
    (line-bg1          bg2)
    (line-bg2          bg4)
    (line-border       bg4)
    (line-color1       keyword)
    (line-color2       builtin)
    ; TODO:
    (segment-active    gray3)
    (segment-inactive  gray3)
    (evil-normal       teal1)
    (evil-insert       spring-green1)
    (evil-visual       orange1)
    (evil-replace      red1)
    (evil-motion       yellow1)
    (evil-operator     evil-normal)
    (evil-emacs        amber3)

    (fringe        (if kaolin-themes-distinct-fringe bg2 bg1))
    (win-border    bg3)
    (line-num-bg   (if kaolin-themes-distinct-fringe bg2 bg1))
    (line-num-fg   gray3)
    (line-num-hl   gray9)
    (cursor        white0)

    (swiper-bg   bg2)
    (ivy-bg      nil)
    (ivy1        fg1)
    (ivy2        azure3)
    (ivy3        amber3)
    (ivy4        violet3)))

;; Predefined Kaolin face specifications
(defconst kaolin-faces
  '(
    ;; Font-lock
    (font-lock-builtin-face           (:foreground builtin))
    (font-lock-comment-delimiter-face (:foreground comment :italic kaolin-themes-italic-comments))
    (font-lock-comment-face           (:foreground comment :italic kaolin-themes-italic-comments))
    (font-lock-constant-face          (:foreground const))
    (font-lock-doc-face               (:foreground doc))
    (font-lock-function-name-face     (:foreground functions :bold bold))
    (font-lock-keyword-face           (:foreground keyword :bold bold))
    (font-lock-negation-char-face     (:foreground red1))
    (font-lock-preprocessor-face      (:foreground prep :bold nil))
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
    (shadow              (:foreground gray4))
    (file-name-shadow    (:inherit 'shadow))
    (region              (:background selection))
    (secondary-selection (:background spring-green6))
    (fringe              (:background fringe :foreground fg1))
    (cursor              (:background cursor))
    (vertical-border     (:foreground win-border))
    (window-divider      (:foreground win-border))
    (minibuffer-prompt   (:foreground prompt :bold bold))
    (bold                (:bold bold))
    (italic              (:italic italic))
    (default-italic      (:italic italic))
    (bold-italic         (:bold bold :italic italic))
    (link                (:foreground link :underline underline))
    (link-visited        (:inherit 'link :underline nil))
    (success             (:background nil :foreground spring-green1))
    (escape-glyph        (:background nil :foreground cyan3))

    (menu        (:background bg2 :foreground fg2))
    (header-line (:background bg4 :foreground var))

    ;; TODO: (??) color fg
    (tooltip      (:background tooltip-bg :foreground tooltip-fg))


    (match        (:background nil :foreground hl))
    (isearch      (:background nil :foreground hl :bold bold :underline underline))
    (isearch-fail (:background nil :foreground red1))


    ;; Interface
    (package-name          (:inherit 'link :underline nil))
    (button                (:inherit 'link))
    (custom-button         (:background bg3 :foreground button :box (:line-width 2 :color bg2 :style 'released-button)))
    (custom-button-mouse   (:background bg4 :foreground button-hl :box (:line-width 2 :color bg2 :style 'released-button)))
    (custom-button-pressed (:background bg4 :foreground button-hl :box (:line-width 2 :color bg2 :style 'pressed-button)))
    (custom-button-unraised (:inherit 'custom-button))
    (custom-button-pressed-unraised (:inherit 'custom-button-pressed))
    (custom-state          (:background nil :foreground teal1))
    (custom-changed        (:background nil :foreground orange1))
    (custom-visibility     (:background nil :foreground cyan1 :height 0.9 :underline underline))
    (custom-invalid        (:background nil :foreground red1))
    (custom-set            (:background nil :foreground aquamarine4))
    (widget-documentation  (:background nil :foreground var))
    (widget-button         (:background nil :foreground keyword))

    ;; Highlighting
    (highlight                (:background bg2 :foreground amber3))
    (lazy-highlight           (:background bg4 :foreground hl))
    (hl-line                  (:background hl-line))
    (highlight-numbers-number (:foreground num))
    (highlight-quoted-quote   (:inherit 'font-lock-builtin-face))
    (highlight-quoted-symbol  (:inherit 'font-lock-keyword-face))

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

    ;; Elfeed
    (elfeed-search-tag-face          (:foreground amber3))
    (elfeed-search-feed-face         (:foreground teal1))
    (elfeed-search-date-face         (:foreground var))
    (elfeed-search-unread-title-face (:foreground fg1))
    (elfeed-search-unread-count-face (:foreground orange1))
    (elfeed-search-title-face        (:foreground comment))

    ;; Modeline
    (mode-line           (:box (:line-width 2 :color line-border) :background line-bg1 :foreground var :bold bold))
    (mode-line-inactive  (:box (:line-width 2 :color line-border) :background line-bg1 :foreground gray9 :bold bold))
    (mode-line-buffer-id (:background nil :foreground line-color2 :bold bold))
    (mode-line-highlight (:foreground line-color2 :box nil :bold bold))
    (mode-line-emphasis  (:foreground fg1))

    ;; Telephone-line
    (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground line-fg))
    (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))
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
    (spaceline-highlight-face (:foreground cyan3))

    ;; Smart-mode-line
    (sml/line-number      (:foreground chartreuse1))
    (sml/modes            (:foreground magenta4))
    (sml/global           (:foreground cyan3))
    (sml/filename         (:foreground teal1))
    (sml/charging         (:foreground teal1))
    (sml/discharging      (:foreground red1))
    (sml/modified         (:foreground spring-green1 :bold bold))
    (sml/outside-modified (:background red0 :foreground fg1))
    (sml/prefix           (:foreground line-fg))
    (sml/read-only        (:foreground orange1))

    ;; Highlight TODOs
    (fic-face         (:background nil :foreground todo :bold bold))
    (fic-author-face  (:background nil :foreground todo :bold bold))
    (hl-todo          (:background nil :foreground todo :bold bold))

    ;; Additional completion
    (ac-completion-face    (:foreground keyword :underline underline))
    (info-quoted-name      (:foreground builtin))
    (info-string           (:foreground str))
    (icompletep-determined (:foreground builtin))

    ;; Company
    (company-tooltip                  (:background tooltip-bg :foreground tooltip-fg :bold bold))
    (company-tooltip-common           (:foreground hl :underline underline))
    (company-tooltip-common-selection (:foreground hl :underline underline))
    (company-tooltip-selection        (:background tooltip-hl-bg :foreground tooltip-hl-fg))
    (company-tooltip-annotation       (:foreground var))
    (company-scrollbar-bg             (:background bg1))
    (company-scrollbar-fg             (:foreground keyword))
    ;; TODO: read about template
    (company-template-field           (:background bg3))
    (company-echo-common              (:background bg1 :foreground amber3))
    (company-preview                  (:background nil :foreground keyword))
    (company-preview-common           (:background bg2 :foreground amber3))
    (company-preview-search           (:background bg1 :foreground azure1))
    (company-tooltip-mouse            (:background bg3 :foreground fg3))

    ;; Magit
    (magit-section-highlight         (:background bg2))
    (magit-section-heading           (:foreground keyword))
    (magit-section-heading-selection (:foreground button-hl :bold bold))
    (magit-item-highlight            (:background bg3))
    (magit-blame-heading             (:background bg3 :foreground var))


    (magit-branch                 (:foreground cyan1))
    (magit-branch-local           (:foreground cyan1))
    (magit-branch-remote          (:foreground aquamarine1))
    (magit-hunk-heading           (:background bg3))
    (magit-hunk-heading-highlight (:background bg3))
    (magit-diff-base              (:background amber2 :foreground fg2))
    (magit-diff-base-highlight    (:background amber2 :foreground fg1))
    (magit-diff-file-header       (:background bg3 :foreground fg2))
    (magit-diff-context           (:background bg3 :foreground fg3))
    (magit-diff-context-highlight (:background bg3 :foreground fg2))
    (magit-diff-added             (:background diff-bg-add :foreground fg1))
    (magit-diff-added-highlight   (:background diff-bg-add :foreground fg0))
    (magit-diff-removed           (:background diff-bg-rem :foreground fg1))
    (magit-diff-removed-highlight (:background diff-bg-rem :foreground fg0))
    (magit-diffstat-added         (:foreground diff-add))
    (magit-diffstat-removed       (:foreground diff-rem))
    (magit-tag                    (:foreground orange1))
    (magit-hash                   (:inherit 'magit-tag))
    (magit-dimmed                 (:inherit 'shadow))
    (magit-log-author             (:foreground prep))
    (magit-log-date               (:foreground var))
    (magit-log-graph              (:foreground str))

    (magit-process-ok             (:foreground spring-green3 :bold bold))
    (magit-process-ng             (:foreground warning :bold bold))

    (magit-reflog-amend           (:foreground violet1))
    (magit-reflog-checkout        (:foreground capri1))
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
    (magit-signature-good         (:foreground aquamarine1))
    (magit-signature-bad          (:foreground red0))
    (magit-signature-untrusted    (:foreground cyan1))

    (magit-popup-key              (:foreground keysym))


    ;; Flycheck
    (flycheck-info           (:underline (:style underline-style :color done)))
    (flycheck-warning        (:underline (:style underline-style :color warning)))
    (flycheck-error          (:underline (:style underline-style :color err)))
    (flycheck-fringe-error   (:foreground err))
    (flycheck-fringe-warning (:foreground warning))
    (flycheck-fringe-info    (:foreground done))

    ;; Flyspell
    (flyspell-duplicate (:underline (:style underline-style :color warning)))
    (flyspell-incorrect (:underline (:style underline-style :color err)))

    ;; Hydra
    (hydra-face-red      (:foreground red1))
    ;; TODO: (??) change to teal
    (hydra-face-teal     (:foreground cyan3))
    (hydra-face-blue     (:foreground azure3))
    (hydra-face-pink     (:foreground pink1))
    (hydra-face-amaranth (:foreground magenta4))

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
    (js2-error                    (:underline (:color red0 :style underline-style)))
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

    ;; Latex/Auctex
    (font-latex-warning-face      (:inherit 'warning))
    (font-latex-string-face       (:inherit 'font-lock-string-face))
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
    (undo-tree-visualizer-active-branch-face (:foreground fg1 :bold bold))
    (undo-tree-visualizer-current-face       (:foreground cyan1))
    (undo-tree-visualizer-default-face       (:foreground fg2))
    (undo-tree-visualizer-unmodified-face    (:foreground var))
    (undo-tree-visualizer-register-face      (:foreground type))

    ;; Rainbow delimeters
    (show-paren-match (:background nil :foreground orange1 :bold bold))

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
    (diff-header      (:background bg2))
    (diff-file-header (:background bg2 :foreground teal1))
    (diff-added       (:background diff-bg-add :foreground fg1))
    (diff-changed     (:background diff-bg-mod :foreground fg1))
    (diff-removed     (:background diff-bg-rem :foreground fg1))

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

    ;; TODO Imenu list
    (imenu-list-entry-subalist-face-0 (:inherit 'font-lock-keyword-face))
    (imenu-list-entry-face-1 (:foreground tooltip-fg))
    (imenu-list-entry-face-0 (:inherit 'font-lock-type-face))

    ;; TODO Treemacs
    (treemacs-header-face (:inherit 'header-line))

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
    (popup-tip-face            (:background tooltip-hl-bg :foreground builtin :bold bold))

    ;; Terminal
    (term               (:background bg1 :foreground fg1))
    (term-color-black   (:foreground black1))
    (term-color-blue    (:foreground azure1))
    (term-color-red     (:foreground red1))
    (term-color-green   (:foreground teal1))
    (term-color-yellow  (:foreground yellow1))
    (term-color-magenta (:foreground magenta4))
    (term-color-cyan    (:foreground cyan1))
    (term-color-white   (:foreground white2))

    ;; EShell
    (eshell-prompt        (:foreground teal1 :bold bold))
    (eshell-ls-directory  (:foreground blue1 :bold bold))
    (eshell-ls-symlink    (:foreground azure1 :bold bold))
    (eshell-ls-executable (:foreground chartreuse1 :bold bold))
    (eshell-ls-archive    (:foreground red1))
    (eshell-ls-backup     (:foreground magenta4))
    (eshell-ls-clutter    (:foreground pink1))
    (eshell-ls-missing    (:background bg3 :foreground red1))
    (eshell-ls-product    (:foreground yellow1))
    (eshell-ls-readonly   (:foreground fg2))
    (eshell-ls-special    (:foreground spring-green1))
    (eshell-ls-unreadable (:foreground var))

    ;; Whitespace mode
    ;; TODO: Add variant for light themes
    (whitespace-empty            (:background spring-green6 :foreground gray9))
    (whitespace-line             (:background bg3 :foreground warning))
    (whitespace-newline          (:foreground cyan3))
    (whitespace-indentation      (:background hl-indent))
    (whitespace-tab              (:background aquamarine4))
    (whitespace-space            (:background gray4 :foreground spring-green6))
    (whitespace-hspace           (:foreground cyan1))
    (whitespace-space-before-tab (:background orange2 :foreground bg2))
    (whitespace-space-after-tab  (:background orange2 :foreground bg2))
    (whitespace-trailing         (:foreground red0))
    (whitespace-big-indent       (:background red2 :foreground red0))

    ;; Org-mode
    ;; TODO: org agenda faces
    (org-todo                      (:foreground todo :bold bold))
    (org-done                      (:foreground done  :bold bold))
    (org-headline-done             (:foreground gray4  :bold nil))
    (org-ellipsis                  (:foreground builtin))
    (org-date                      (:foreground amber3 :underline underline))
    (org-link                      (:inherit 'link))
    (org-code                      (:foreground amber3))
    (org-verbatim                  (:foreground azure3))
    (org-hide                      (:foreground bg1))
    (org-special-keyword           (:foreground functions))
    (org-table                     (:background bg2 :foreground fg3))
    (org-formula                   (:background nil :foreground type))
    (org-warning                   (:foreground warning :underline underline))
    (org-tag                       (:foreground prep))
    (org-checkbox                  (:inherit 'org-special-keyword))

    (org-document-info-keyword     (:foreground second-key))
    (org-meta-line                 (:inherit 'org-document-info-keyword))
    (org-block-begin-line          (:foreground second-key))
    (org-block-end-line            (:inherit 'org-block-begin-line))
    (org-list-dt                   (:inherit 'org-checkbox))
    (org-document-title            (:foreground builtin :bold bold))
    (org-document-info             (:foreground builtin))
    (org-footnote                  (:foreground fg4 :underline underline))

    (org-agenda-date-weekend       (:foreground fg4 :weight 'normal))
    (org-block                     (:foreground fg3))
    (org-quote                     (:inherit 'org-block :italic italic))
    (org-verse                     (:inherit 'org-block :italic italic))
    (org-agenda-done               (:foreground bg4))
    (org-scheduled                 (:foreground type))
    (org-scheduled-today           (:foreground functions :height 1.2 :bold bold))
    (org-sexp-date                 (:foreground fg4))

    (org-level-1            (:foreground keyword :bold bold :height 1.1))
    (org-level-2            (:foreground builtin  :bold nil))
    (org-level-3            (:foreground num :bold nil))
    (org-level-4            (:foreground const :bold nil))

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
    (evil-ex-substitute-matches     (:background nil :foreground red1 :underline underline))
    (evil-ex-substitute-replacement (:background nil :foreground spring-green1))
    (evil-ex-lazy-highlight         (:inherit 'lazy-highlight))

    ;; Evil-goggles
    ;; TODO: add rest of evil goggles faces
    (evil-goggles-default-face  (:background pulse))

    ;; Beacon-mode
    (beacon-fallback-background (:background pulse))

    ;; Tuareg/OCaml
    (tuareg-font-double-colon-face (:foreground keyword))

    ;; Helm
    ;; TODO: (!!) find helm status line color that based on var face
    ;; TODO: update faces
    ;; TODO: light theme: helm-find-files header
    ;; TODO: add helm-locate-finish and helm-prefarg
    ;; TODO: customize '[?]' write something like fsfasfsa in helm
    (helm-header                              (:foreground fg2 :underline nil :box nil))
    (helm-source-header                       (:foreground keyword :underline nil :bold bold))
    (helm-match                               (:foreground type :bold bold))
    (helm-header-line-left-margin             (:background azure1 :foreground bg1))
    (helm-selection                           (:background bg2 :foreground type :bold bold))
    (helm-selection-line                      (:background bg2 :foreground type :bold bold))
    (helm-visible-mark                        (:foreground azure1))
    (helm-candidate-number                    (:foreground aquamarine4))
    (helm-separator                           (:foreground type))
    (helm-time-zone-current                   (:foreground builtin))
    (helm-time-zone-home                      (:foreground type))
    (helm-buffer-not-saved                    (:foreground type))
    (helm-buffer-process                      (:foreground builtin))
    (helm-buffer-saved-out                    (:foreground fg1))
    (helm-buffer-size                         (:foreground fg1))
    (helm-ff-directory                        (:foreground functions :bold bold))
    (helm-buffer-directory                    (:foreground magenta4))
    (helm-ff-dotted-directory                 (:foreground functions :bold bold))
    (helm-ff-dotted-symlink-directory         (:foreground azure1 :bold bold))
    (helm-ff-file                             (:foreground fg1 :weight 'normal))
    (helm-ff-executable                       (:foreground keyword :weight 'normal))
    (helm-ff-invalid-symlink                  (:foreground warning :bold bold))
    (helm-resume-need-update                  (:background red0 :foreground nil))
    (helm-ff-symlink                          (:foreground keyword :bold bold))
    (helm-ff-prefix                           (:background keyword :foreground bg1 :weight 'normal))
    (helm-grep-cmd-line                       (:foreground fg1))
    (helm-grep-file                           (:foreground fg1))
    (helm-grep-finish                         (:foreground fg2))
    (helm-grep-lineno                         (:foreground fg1))
    (helm-grep-match                          (:inherit 'helm-match :background nil :foreground nil))
    (helm-grep-running                        (:foreground functions))
    (helm-moccur-buffer                       (:foreground functions))
    (helm-source-go-package-godoc-description (:foreground str))
    (helm-bookmark-w3m                        (:foreground type))

     ;; Avy
    (avy-lead-face-0 (:background spring-green2 :foreground fg1))
    (avy-lead-face   (:background red2 :foreground fg1))
    (avy-lead-face-1 (:background capri2 :foreground fg1))
    (avy-lead-face-2 (:background magenta2 :foreground fg1))

    ;; Ivy & swiper
    (ivy-current-match           (:background hl-line :foreground hl :bold t))
    (ivy-match-required-face     (:background nil :foreground err :bold nil))
    (ivy-subdir                  (:foreground keyword :bold bold))
    (ivy-cursor                  (:background bg3 :foreground fg1))
    (ivy-confirm-face            (:inherit 'success))
    (ivy-modified-buffer         (:foreground diff-mod))

    (ivy-minibuffer-match-face-1 (:background nil :foreground ivy1))
    (ivy-minibuffer-match-face-2 (:background nil :foreground ivy2 :bold bold))
    (ivy-minibuffer-match-face-3 (:background nil :foreground ivy3 :bold bold))
    (ivy-minibuffer-match-face-4 (:background nil :foreground ivy4 :bold bold))

    (swiper-match-face-1 (:background bg2 :foreground ivy1))
    (swiper-match-face-2 (:background bg2 :foreground ivy2 :bold bold))
    (swiper-match-face-3 (:background bg2 :foreground ivy3 :bold bold))
    (swiper-match-face-4 (:background bg2 :foreground ivy4 :bold bold))
    (swiper-line-face    (:inherit 'hl-line))))

;; Predefined Kaolin variables
;; (defconst kaolin-vars
;;   '())

(provide 'kaolin-themes-lib)

;;; kaolin-themes-lib.el ends here
