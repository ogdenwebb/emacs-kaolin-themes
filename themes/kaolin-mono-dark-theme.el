;;; kaolin-mono-dark-theme.el --- Almost monochrome dark green Kaolin theme.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme mono-dark "Almost monochrome dark green Kaolin theme."
  ;; Palette modification
  (
   (bg0 "#0c120f")
   (bg1 "#111915")
   (bg2 "#16211C")
   (bg3 "#1B2822")
   (bg4 "#25352D")

   (fg0 "#f5f6f5")
   (fg1 "#e9eae7")
   (fg2 "#dcded9")
   (fg3 "#cfd2cb")
   (fg4 "#c5c9c0")

   ;; Accent color
   (hl aquamarine3)
   (keyword spring-green8)

   (second-key  magenta4 cerise4)
   ;; (builtin     "#E0F3EB")
   (builtin     spring-green9)
   (functions   builtin)
   (const       builtin)
   (var         builtin)
   (type        spring-green7)

   (comment "#41544B")
   (alt-comment gray6)
   (warning     orange1)
   (err         red1)

   (prep        aquamarine1)
   (num         aquamarine1)
   (bool        num)
   (str         spring-green3)
   (str-alt     str)
   (doc         str-alt)

   (dim-buffer bg0)
   ;; TODO:
   (hl-line    (if kaolin-themes-hl-line-colored brown6 bg3))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo red1)

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

   (diff-add teal3)
   (diff-mod amber3)
   (diff-rem red1)

   (diff-bg-add spring-green4)
   (diff-bg-mod amber4)
   (diff-bg-rem crimson4)

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
   (line-num-fg   spring-green6 black4)
   (line-num-hl  gray9)

   (cursor       white2))

  ;; Custom theme set faces
  (
   (default             (:background bg1 :foreground fg3))
   (minibuffer-prompt   (:foreground prep :bold bold))

   (link                (:foreground prep :underline underline))
   (show-paren-mismatch (:background bg2 :foreground red0))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure8))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (org-level-1         (:foreground teal1 :bold bold :height 1.1))
   (org-level-2         (:foreground keyword :bold nil))
   (org-level-3         (:foreground spring-green3 :bold nil))
   (org-level-4         (:foreground aquamarine1 :bold nil))
   (org-document-title  (:foreground cerulean7 :bold bold))
   (org-document-info   (:foreground cerulean7))
   (org-date            (:foreground spring-green3 :underline underline))
   (org-code            (:foreground vermilion4))
   (org-verbatim        (:foreground orange1))
   (org-quote           (:foreground blue4)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-mono-dark
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-mono-dark-theme.el ends here
