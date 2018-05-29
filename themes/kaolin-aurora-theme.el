;;; kaolin-aurora-theme.el --- Kaolin meets polar lights.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme aurora "Kaolin meets polar lights."
  ;; Palette modification
  (
   (teal0 "#0E9E97")
   (aquamarine3 "#63E8C1")
   (cyan3 "#62D2DB")
   (spring-green0 "#31E183")
   (magenta3 "#CE8EC8")

   (bg0 "#0C0F12" black0)
   (bg1 azure5 black1)
   (bg2 "#191F26" black2)
   (bg3 "#1F272E" black3)
   (bg4 "#252D35" black4)

   ;; TODO: mb yellow3
   (hl magenta3)

   (keyword     violet4)
   (builtin     violet4)
   (second-key  teal0)

   (functions   aquamarine3)
   (const       teal0)
   (var         spring-green0)
   (type        cyan3)


   ;; TODO:
   ;; (comment     gray2)
   ;; (comment     "#424153")
   (comment     "#454459")
   (alt-comment gray6)
   (warning     orange1)
   (err         crimson1)

   (prep        spring-green0)
   (num         orange3)
   (bool        num)

   (str         orange3)
   ;; (str         amber3)
   (str-alt     vermilion4)
   (doc         str-alt)

   (dim-buffer bg0)
   ;; TODO:
   (hl-line    (if kaolin-themes-hl-line-colored bg3 bg3))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo crimson1)

   (done spring-green1)

   (tooltip-hl-bg bg4)
   (tooltip-hl-fg hl)

   (ivy2 lime3)
   (ivy3 crimson3)
   (ivy4 erin3)

   (rb1 teal0)
   (rb2 aquamarine1)
   (rb3 cyan3)
   (rb4 violet4)
   (rb5 spring-green1)
   (rb6 cerulean7)
   (rb7 vermilion4)
   (rb8 magenta4)
   (rb9 aquamarine1)

   (diff-add teal3)
   (diff-mod orange3)
   (diff-rem crimson1)

   (diff-bg-add aquamarine6)
   (diff-bg-mod vermilion6)
   (diff-bg-rem red6)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       teal0)
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

   (win-border   bg3)
   (line-num-fg  azure6 black4)
   (line-num-hl  teal0)

   (cursor       white0))

  (
   ;; Custom theme set faces
   (default             (:background bg1 :foreground fg2))

   (link                (:foreground erin3 :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure8))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (highlight-quoted-quote   (:foreground type))
   (highlight-quoted-symbol  (:foreground teal0))

   (org-level-1         (:foreground teal0 :bold bold :height 1.1))
   (org-level-2         (:foreground aquamarine3 :bold nil))
   (org-level-3         (:foreground violet3 :bold nil))
   (org-level-4         (:foreground orange3 :bold nil))
   (org-document-title  (:foreground cyan3 :bold bold))
   (org-document-info   (:foreground cyan3))
   (org-date            (:foreground teal0 :underline underline))
   (org-table           (:foreground capri4))
   (org-code            (:foreground yellow3))
   (org-verbatim        (:inherit    'org-code))
   (org-quote           (:foreground blue4)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-aurora
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-aurora-theme.el ends here
