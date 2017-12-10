;;; kaolin-galaxy-theme.el --- Bright theme based on one of the Sebastian Andaur arts.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(define-kaolin-theme mono "Bright theme based on one of the Sebastian Andaur arts."
  ;; Palette modification
  (
   (bg1          gray0)
   (bg2          gray1)
   (bg3          gray2)
   (bg4          gray3)

   (fg1          white2)
   (fg2          white3)
   (fg3          white4)
   (fg3          gray9)

   (keyword     white4)
   (second-key  purple alt-purple)
   ;; TODO: adjust contrast with keyword
   (builtin     gray7)
   (functions   builtin)
   ;; (var         lime)
   ;; (const       light-green)
   (var         white0)
   (const       white0)
   (type        green)
   (num         red)
   (bool        num)
   (prep        pink "#5f87af")

   ;; TODO: a bit more blue; at least for alt-comment
   (comment     gray3)
   ;; TODO:
   (alt-comment "#4c344c")
   (str         teal-blue)
   ;; TODO: make bor bright
   (str-alt     grayish-blue)
   (doc         str-alt)
   ;; TODO: or light-orange
   (warning     orange)
   (err         light-red)

   (dim-buffer "#140E14")
   (hl         light-green)
   (hl-line    gray2)
   (hl-indent  bg4)
   (selection  bg4)
   (pulse      dark-blue)

   (todo light-red)
   (done light-green)

   (tooltip-bg    bg2)
   (tooltip-fg    gray9)
   (tooltip-hl-bg bg4)
   (tooltip-hl-fg cyan)

   (ivy2 cyan)
   (ivy3 faded-orange)
   (ivy4 faded-red)

   (rb1 alt-lavender)
   (rb2 teal)
   (rb3 light-violet)
   (rb4 faded-blue)
   (rb5 wheat)
   (rb6 grayish-blue)
   (rb7 grayish-orange)
   (rb8 purple)
   (rb9 pink)

   (diff-add    light-green)
   (diff-change light-violet)
   (diff-rem    light-red)

   ;; Mode-line
   (line-fg           fg4)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-color2       lavender)
   (line-border       bg3)

   (segment-active    gray3)
   (segment-inactive  gray3)
   (evil-normal       green)
   (evil-insert       light-green)
   (evil-visual       orange)
   (evil-replace      red)
   (evil-motion       yellow)
   (evil-operator     evil-normal)
   (evil-emacs        light-yellow)

   (win-border    black3)
   (line-num-bg   bg1)
   (line-num-fg   bg4 black4)

   (evil-normal teal-blue)

   (line-num-hl  gray9)
   (cursor       white4))

  ;; Custom theme set faces
  (

   ;; TODO: cyan, soft-blue, pink
   (link                (:foreground pink :underline underline))
   (show-paren-mismatch (:background bg2 :foreground alt-red))

   (telephone-line-accent-active   (:inherit 'mode-line :background line-bg2 :foreground alt-grayish-blue))
   (telephone-line-accent-inactive (:inherit 'mode-line-inactive :background line-bg1 :foreground gray9))

   (org-document-title  (:foreground grayish-blue :bold bold))
   (org-document-info   (:foreground grayish-blue))
   (org-date            (:foreground teal-green :underline underline))
   (org-code            (:foreground faded-orange))
   (org-verbatim        (:foreground orange))
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
