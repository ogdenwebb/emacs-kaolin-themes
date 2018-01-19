;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme mono "TODO"
  ;; Palette modification
  (
   (bg0          "#0c120f")
   (bg1          "#111915")
   (bg2          "#16211C")
   (bg3          "#1B2822")
   (bg4          "#25352D")

   ;; Accent color
   (hl aquamarine3)
   (keyword spring-green6)

   (second-key  magenta4 cerise4)
   ;; TODO: adjust contrast with keyword
   (builtin     fg0)
   (functions   builtin)
   (const       builtin)
   (var         const)
   (type        fg0)

   ;; (comment     spring-green4)
   (comment     "#3b5749")
   (alt-comment gray6)
   (warning     orange1)
   (err         red1)

   (prep        aquamarine1)
   (num         aquamarine1)
   (bool        num)
   (str         aquamarine1)
   (str-alt     str)
   (doc         str-alt)


   (dim-buffer bg0)
   (hl-line    bg3)
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo red3)
   (done spring-green1)

   (tooltip-hl-bg bg4)
   (tooltip-hl-fg hl)

   (ivy2 amber1)
   (ivy3 vermilion3)
   (ivy4 green1)

   (rb1 gray9)
   (rb2 gray9)
   (rb3 gray9)
   (rb4 gray9)
   (rb5 gray9)
   (rb6 gray9)
   (rb7 gray9)
   (rb8 gray9)
   (rb9 gray9)

   (diff-add aquamarine3)
   (diff-mod amber3)
   (diff-rem red3)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       keyword)
   (line-border       bg3)

   (segment-active    gray3)
   (segment-inactive  gray3)
   (evil-normal       teal1)
   (evil-insert       spring-green1)
   (evil-visual       orange1)
   (evil-replace      red1)
   (evil-motion       yellow1)
   (evil-operator     evil-normal)
   (evil-emacs        amber3)

   (win-border    bg3)
   (line-num-bg   bg1)
   (line-num-fg   bg4 black4)

   (evil-normal capri4)

   (line-num-hl  gray9)
   (cursor       white2))

  ;; Custom theme set faces
  (
   (default             (:background bg1 :foreground fg3))
   (minibuffer-prompt   (:foreground prep :bold bold))

   (link                (:foreground pink1 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure6))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (org-document-title  (:foreground cerulean6 :bold bold))
   (org-document-info   (:foreground cerulean6))
   (org-date            (:foreground spring-green3 :underline underline))
   (org-code            (:foreground vermilion4))
   (org-verbatim        (:foreground orange1))
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
