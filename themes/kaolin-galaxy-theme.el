;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-galaxy nil
  "Kaolin galaxy theme options."
  :group 'kaolin-themes)

(defcustom kaolin-galaxy-alt-bg nil
  "Use alternative darker background color."
  :type 'boolean
  :group 'kaolin-themes)

(define-kaolin-theme galaxy "Bright theme based on one of the Sebastian Andaur arts."
  ;; Palette modification
  (
   (azure2        "#232c35")
   (azure1        "#2a57cc")
   ;; (azure1     "#335599")
   (chartreuse1   "#73c66c")
   (spring-green1 "#6dd797")

   ;;                             dark      bright
   (bg0 (if kaolin-galaxy-alt-bg "#19181C" "#1C1B21") black0)
   (bg1 (if kaolin-galaxy-alt-bg "#1d1c21" "#212026") black1)
   (bg2 (if kaolin-galaxy-alt-bg "#26252c" "#2a2931") black2)
   (bg3 (if kaolin-galaxy-alt-bg "#302e36" "#33323b") black3)
   (bg4 (if kaolin-galaxy-alt-bg "#393741" "#3d3b46") black4)

   ;; TODO: add other fg vars
   (fg1 blue9)

   (keyword     violet3)
   (second-key  ultramarine4 cerise4)
   (builtin     violet4)
   (functions   builtin)
   (var         amber3)
   (const       orange3)
   (type        teal1)
   (num         crimson1 "#5f87af")
   (prep        azure3)
   (bool        num)

   ;; TODO: a bit more like azure1 at least for alt-comment
   (comment     gray3)
   (str         capri4)
   (str-alt     cerulean8)
   (doc         str-alt)
   (warning     orange3)
   (err         crimson0)

   (dim-buffer "#140E14")
   (hl         cyan0)
   (hl-line    (if kaolin-themes-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   ;; TODO: (??) less bright
   (selection  bg4)
   (pulse      ultramarine6)

   (todo crimson1)
   (done spring-green1)

   ;; Tooltip
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg cyan0)

   (ivy2 lime3)
   (ivy3 vermilion3)
   (ivy4 red3)

   (rb1 violet4)
   (rb2 teal4)
   (rb3 violet3)
   (rb4 blue4)
   (rb5 capri4)
   (rb6 cerulean7)
   (rb7 orange8)
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
   (evil-normal       capri4)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        yellow3)

   (win-border    black3)
   (line-num-fg   bg4 black4)
   (line-num-hl   cerulean8)

   (cursor        "#c3c8e0"))

  (
   ;; Custom theme set faces
   (link                (:foreground crimson1 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure8))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (highlight-quoted-symbol  (:foreground var))

   (org-document-title  (:foreground cerulean7 :bold bold))
   (org-document-info   (:foreground cerulean7))
   (org-date            (:foreground spring-green3 :underline underline))
   (org-code            (:foreground erin3))
   (org-verbatim        (:foreground aquamarine3))
   (org-quote           (:foreground blue9)))

  ;; Set custom vars
  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-galaxy
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-galaxy-theme.el ends here
