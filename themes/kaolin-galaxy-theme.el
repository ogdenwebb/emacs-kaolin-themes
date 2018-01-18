;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme galaxy "Bright theme based on one of the Sebastian Andaur arts."
  ;; Palette modification
  (
   (azure2    "#232c35")
   (azure1         "#2a57cc")
   ;; (azure1      "#335599")
   (chartreuse1         "#73c66c")
   (spring-green1  "#6dd797")

   ;; Dark
   ;; (bg1  "#1d1c21" black1)
   ;; (bg2          "#26252c" black2)
   ;; (bg3          "#302e36" black3)
   ;; (bg4          "#393741" black4)

   ;; Brigth
   (bg1          "#212026" black1)
   (bg2          "#2a2931" black2)
   (bg3          "#33323b" black3)
   (bg4          "#3d3b46" black4)


   (keyword     violet3)
   (second-key  magenta4 cerise4)
   ;; TODO: adjust contrast with keyword
   (builtin     violet4)
   (functions   builtin)
   ;; (var         chartreuse1)
   ;; (const       spring-green1)
   (var         amber3)
   (const       orange3)
   (type        teal1)
   (num         crimson1 "#5f87af")
   (prep        capri3)
   (bool        num)

   ;; TODO: a bit more azure1; at least for alt-comment
   (comment     gray3)
   ;; TODO:
   (alt-comment "#4c344c")
   (str         capri4)
   ;; TODO: make bor bright
   (str-alt     cerulean6)
   (doc         str-alt)
   ;; TODO: or amber3
   (warning     orange1)
   (err         red3)

   (dim-buffer "#140E14")
   (hl        cyan0)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   ;; TODO: (??) less bright
   (selection  bg4)
   (pulse      azure2)

   (todo red3)
   (done spring-green1)

   ;; Tooltip
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg cyan0)

   ;; TODO: (??) change
   (ivy2 lime3)
   (ivy3 vermilion3)
   (ivy4 red3)

   (rb1 violet4)
   (rb2 teal4)
   (rb3 violet3)
   (rb4 blue4)
   (rb5 capri4)
   (rb6 cerulean6)
   (rb7 orange6)
   (rb8 magenta4)
   (rb9 crimson3)

   (diff-add spring-green1)
   (diff-mod violet3)
   (diff-rem red3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       violet4)
   (line-border       bg3)

   (segment-active    gray3)
   (segment-inactive  gray3)
   (evil-normal       teal1)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        yellow3)

   (win-border    black3)
   (line-num-bg   bg1)
   (line-num-fg   bg4 black4)
   (line-num-hl   gray9)

   (evil-normal capri4)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (

   ;; TODO: cyan, azure3, crimson1
   (link                (:foreground crimson1 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure6))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (org-document-title  (:foreground cerulean6 :bold bold))
   (org-document-info   (:foreground cerulean6))
   (org-date            (:foreground spring-green3 :underline underline))
   (org-code            (:foreground vermilion4))
   (org-verbatim        (:foreground aquamarine4))
   (org-quote           (:foreground blue4)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-galaxy
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-galaxy
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-galaxy-theme.el ends here
