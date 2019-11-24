;;; kaolin-mono-dark-theme.el --- Almost monochrome dark green Kaolin theme.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme mono-dark "Almost monochrome dark green Kaolin theme."
  ;; Palette modification
  (
   (bg0 "#0c120f" black0)
   ;; (bg1 "#111915" black1)
   (bg1 aquamarine5 black1)
   (bg2 "#16211C" black2)
   (bg3 "#1B2822" black3)
   (bg4 "#25352D" black4)

   (fg0 "#f5f6f5")
   (fg1 "#e9eae7")
   (fg2 "#dcded9")
   (fg3 "#cfd2cb")
   (fg4 "#c5c9c0")

   ;; Accent color
   (hl aquamarine3)
   ;; (keyword spring-green8)
   (keyword     aquamarine2)

   (comment     "#41544B")
   (metakey     (if kaolin-themes-distinct-metakeys spring-green4 comment))
   ;; (builtin     aquamarine2)
   (builtin     spring-green8)
   (functions   builtin)
   (const       teal4)
   (var         teal4)
   (type        aquamarine1)

   ;; TODO:
   (comment-alt gray6)
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
   (hl-line    (if kaolin-themes-hl-line-colored "#af5f00" bg3))
   ; (hl-indent bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo red1)

   (done spring-green1)

   (tooltip-hl-bg bg4)
   (tooltip-hl-fg hl)

   (search1 amber1)
   (search2 vermilion3)
   (search3 green1)

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
   (diff-mod vermilion4)
   (diff-rem red1)

   (diff-bg-add spring-green4)
   (diff-bg-mod amber4)
   (diff-bg-rem crimson4)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       keyword)
   (line-border       (if kaolin-themes-modeline-border bg3 line-bg1))

   (segment-active    gray3)
   (segment-inactive  gray3)

   (win-border    bg3)
   (line-num-fg   spring-green6 black4)
   (line-num-hl  gray9)

   (cursor       white2))

  ;; Custom theme set faces
  (
   (default             (:background bg1 :foreground fg3))
   (minibuffer-prompt   (:foreground prep :bold bold))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground azure8))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (highlight-quoted-quote   (:foreground functions))
   (highlight-quoted-symbol  (:foreground num))

   (org-level-1         (:foreground teal4 :bold bold :height 1.1))
   (org-level-2         (:foreground keyword :bold nil))
   (org-level-3         (:foreground spring-green3 :bold nil))
   (org-level-4         (:foreground aquamarine1 :bold nil))
   (org-document-title  (:foreground aquamarine1 :bold bold))
   (org-date            (:foreground warning :underline underline))
   ;; (org-code            (:foreground warning))
   (org-verbatim        (:foreground orange1))
   (org-quote           (:foreground fg4)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-mono-dark
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-mono-dark-theme.el ends here
