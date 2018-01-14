;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme mono "TODO"
  ;; Palette modification
  (
   (bg1          gray0)
   (bg2          gray1)
   (bg3          gray2)
   (bg4          gray3)

   ;; Accent color
   (hl aquamarine3)

   (keyword     fg0)
   (second-key  magenta4 cerise4)
   ;; TODO: adjust contrast with keyword
   (builtin     fg1)
   (functions   builtin)
   ;; (var         chartreuse1)
   ;; (const       spring-green1)
   (const       builtin)
   (var         const)
   (type        hl)
   (num         fg0)
   (bool        num)
   (prep        fg0)

   ;; TODO: a bit more blue; at least for alt-comment
   (comment     gray4)
   ;; TODO:
   (alt-comment "#4c344c")
   (str         fg4)
   (str-alt     str)
   (doc         str-alt)
   (warning     orange1)
   (err         red1)

   (dim-buffer black4)
   (hl-line    gray2)
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      gray3)

   (todo red1)
   (done spring-green1)

   (tooltip-hl-bg bg4)
   (tooltip-hl-fg hl)

   (ivy2 cyan1)
   (ivy3 vermilion4)
   (ivy4 red4)

   (rb1 violet4)
   (rb2 cyan3)
   (rb3 violet3)
   (rb4 blue4)
   (rb5 amber3)
   (rb6 grayish-blue)
   (rb7 grayish-orange)
   (rb8 magenta4)
   (rb9 pink1)

   (diff-add    aquamarine3)
   (diff-change amber3)
   (diff-rem    red1)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       fg0)
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

   (win-border    black4)
   (line-num-bg   bg1)
   (line-num-fg   bg4 black4)

   (evil-normal capri4)

   (line-num-hl  gray9)
   (cursor       white2))

  ;; Custom theme set faces
  (
   (default             (:background bg1 :foreground fg2))

   (link                (:foreground pink1 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure5))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (org-document-title  (:foreground grayish-blue :bold bold))
   (org-document-info   (:foreground grayish-blue))
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
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-galaxy-theme.el ends here
