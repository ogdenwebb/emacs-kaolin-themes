;;; kaolin-tribal-theme.el --- theme based on Tribal color scheme by Dayle Rees
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme tribal "theme based on Tribal color scheme by Dayle Rees"
  ;; Palette modification
  ((bg1  "#19191d" black1)
   ;; (bg2  "#291c29")
   ;; (bg4  "#473147")
   (bg2       "#222228" black2)
   (bg3       "#2b2b32" black3)
   (bg4       "#34343d" black4)
   (violet    "#a78db5")
   (dark-blue "#325074")
   (faded-red "#9c3b42")

   ;; violet or lavender or purple, alt-purple, moderate-pink
   (keyword     faded-red)
   (second-key  "#642628" alt-purple)
   (builtin     grayish-blue)
   (functions   builtin)
   ;; TODO:
   (var         teal-blue)
   (const       teal-blue)
   (type        fg1)
   (num         alt-lavender)
   (bool        num)
   (prep        moderate-blue "#5f87af")

   ;; TODO:
   (comment     bg4)
   (alt-comment "#4c344c")
   (str         cyan)
   (str-alt     dark-purple)
   (doc         str-alt)
   (warning     alt-orange)
   (err         red)

   (dim-buffer "#140E14")
   ;; TODO: or soft/light-pink
   (hl         soft-blue)
   (hl-line    (if kaolin-hl-line-colored bg2 black1))
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      dark-blue)

   (todo pink)
   (done light-green)

   (tooltip-bg    bg2)
   (tooltip-fg    light-gray)
   ;; TODO:
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg light-violet)

   (ivy2 dark-green)
   (ivy3 faded-orange)
   (ivy4 alt-blue)

   (rb1 moderate-pink)
   (rb2 lavender)
   (rb3 light-brown)
   (rb4 faded-blue)
   (rb5 brown)
   (rb6 grayish-blue)
   (rb7 grayish-orange)
   (rb8 purple)
   (rb9 faded-red)

   (diff-add    light-jade)
   (diff-change purple)
   (diff-rem    faded-red)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       lavender)
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

   (win-border    dark-gray)
   (line-num-bg   bg1)
   (line-num-fg   bg4 black4)
   ;; TOOD: or hl
   (line-num-hl   faded-blue light-gray)
   (cursor        "#c3c8e0"))

  ;; Custom theme set faces
  (
   (link                (:foreground dark-cyan :underline underline))
   (show-paren-mismatch (:background bg2 :foreground alt-red))

   (org-document-title  (:foreground moderate-pink :bold bold))
   (org-document-info   (:foreground moderate-pink))
   (org-date            (:foreground teal-green :underline underline))
   (org-code            (:foreground faded-orange))
   (org-verbatim        (:foreground orange))
   (org-quote           (:foreground faded-blue)))

  ;; Set custom vars
  (custom-theme-set-variables
   'kaolin-tribal
   '(kaolin-hl-line-colored t))

  (when kaolin-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-tribal
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-change :foreground ,diff-change))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))


;;; kaolin-tribal-theme.el ends here
