;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme galaxy "Bright theme based on one of the Sebastian Andaur arts."
  ;; Palette modification
  (
   ;; dark
   ;; (bg1  "#1d1c21" black1)
   ;; (bg2          "#26252c" black2)
   ;; (bg3          "#302e36" black3)
   ;; (bg4          "#393741" black4)

   ;; Brigth
   (bg1          "#212026" black1)
   (bg2          "#2a2931" black2)
   (bg3          "#33323b" black3)
   (bg4          "#3d3b46" black4)

   (violet       "#9f84ae")
   (yellow       "#b9b963")
   (light-yellow "#eae46a")
   (wheat        "#c8c493" "#ffd7a5")
   (dark-blue    "#232c35")
   (cyan1        "#22aabb")
   (blue         "#2a57cc")
   ;; (blue      "#335599")
   (orange       "#d2ab5d")
   (alt-orange   "#ab6448")
   (red4    "#9c3b42")
   (red3    "#e84c58")
   (pink         "#e55c7a")
   (light-pink   "#cc7799")
   (lime         "#73c66c")
   ;; TEAL
   (teal1        "#609ca6")
   (light-green  "#6dd797")

   (keyword     light-violet)
   (second-key  purple alt-purple)
   ;; TODO: adjust contrast with keyword
   (builtin     violet)
   (functions   builtin)
   ;; (var         lime)
   ;; (const       light-green)
   (var         light-orange)
   (const       wheat)
   (type        teal1)
   (num         pink "#5f87af")
   (prep        soft-blue)
   (bool        num)

   ;; TODO: a bit more blue; at least for alt-comment
   (comment     gray3)
   ;; TODO:
   (alt-comment "#4c344c")
   (str         capri3)
   ;; TODO: make bor bright
   (str-alt     grayish-blue)
   (doc         str-alt)
   ;; TODO: or light-orange
   (warning     orange)
   (err         red3)

   (dim-buffer "#140E14")
   (hl         light-green)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   ;; TODO: (??) less bright
   (selection  bg4)
   (pulse      dark-blue)

   (todo red3)
   (done light-green)

   (tooltip-bg    bg2)
   (tooltip-fg    gray9)
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg cyan1)

   (ivy2 cyan1)
   (ivy3 faded-orange)
   (ivy4 red4)

   (rb1 alt-lavender)
   (rb2 teal4)
   (rb3 light-violet)
   (rb4 faded-blue)
   (rb5 wheat)
   (rb6 grayish-blue)
   (rb7 grayish-orange)
   (rb8 purple)
   (rb9 pink)

   (diff-add    light-green)
   (diff-change light-violet)
   (diff-rem    red3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       lavender)
   (line-border       bg3)

   (segment-active    gray3)
   (segment-inactive  gray3)
   (evil-normal       teal1)
   (evil-insert       light-green)
   (evil-visual       orange)
   (evil-replace      red1)
   (evil-motion       yellow)
   (evil-operator     evil-normal)
   (evil-emacs        light-yellow)

   (win-border    black3)
   (line-num-bg   bg1)
   (line-num-fg   bg4 black4)
   (line-num-hl   gray9)

   (evil-normal capri3)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (

   ;; TODO: cyan, soft-blue, pink
   (link                (:foreground pink :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground alt-grayish-blue))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (org-document-title  (:foreground grayish-blue :bold bold))
   (org-document-info   (:foreground grayish-blue))
   (org-date            (:foreground spring-green3 :underline underline))
   (org-code            (:foreground faded-orange))
   (org-verbatim        (:foreground light-jade))
   (org-quote           (:foreground faded-blue)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-galaxy
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-galaxy
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-galaxy-theme.el ends here
