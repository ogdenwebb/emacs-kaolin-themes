;;; kaolin-ocean-theme.el --- Dark blue Kaolin theme
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme ocean "Dark blue Kaolin theme variant."
  ;; Palette modification
  ((bg1 alt-midnight-blue black1)
   (bg2 "#1d1d2b" black2)
   (bg3 "#28283a" black3)
   (bg4 "#32324a" black4)

   (violet    "#a78db5")
   (dark-blue "#325074")

   (keyword     moderate-blue)
   ;; TODO: a bit more bright
   (second-key  bg4 alt-purple)
   (builtin     teal-blue)
   (functions   builtin)
   (var         alt-lavender)
   (const       alt-purple)
   (type        cyan)
   (num         pink)
   (bool        num)
   (prep        magenta)

   (comment     black4)
   (alt-comment "#34344c")
   (str         wheat "#ffd787")
   (str-alt     faded-orange)
   (doc         str-alt)
   (warning     orange)
   (err         red)

   (dim-buffer "#0F0F17")
   (hl         dark-cyan)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      bg4)

   (todo pink)

   (tooltip-bg    bg2)
   (tooltip-fg    light-gray)
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg lime)

   (ivy2 pink)
   (ivy3 light-orange)
   (ivy4 light-green)

   (rb1 teal)
   (rb2 lavender)
   (rb3 teal)
   (rb4 faded-blue)
   (rb5 jade)
   (rb6 teal-green)
   (rb7 light-yellow)
   (rb8 dark-blue)
   (rb9 soft-blue)

   (diff-add    light-jade)
   (diff-change purple)
   (diff-rem    faded-red)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       bg3)

   (segment-active    gray)
   (segment-inactive  gray)
   (evil-normal       green)
   (evil-insert       light-green)
   (evil-visual       orange)
   (evil-replace      red)
   (evil-motion       yellow)
   (evil-operator     evil-normal)
   (evil-emacs        light-yellow)

   (win-border    bg3)
   (line-num-bg   bg1)
   (line-num-fg   bg4)
   (line-num-hl   faded-blue light-gray)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (
   (link                (:foreground cyan :underline underline))
   (show-paren-mismatch (:background bg2 :foreground alt-red))

   (org-code            (:foreground green))
   (org-verbatim        (:foreground light-green))
   (org-quote           (:foreground faded-blue))

   (git-gutter:added    (:background diff-add :foreground diff-add))
   (git-gutter:modified (:background diff-change :foreground diff-change))
   (git-gutter:deleted  (:background diff-rem :foreground diff-rem)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-ocean
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-ocean
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))



;;; kaolin-ocean-theme.el ends here
