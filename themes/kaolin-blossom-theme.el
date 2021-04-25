;;; kaolin-blossom-theme.el --- Kaolin theme based on orange and purple shades with dark pink background.
;;; Commentary:

;;; Code:
(require 'kaolin-themes)

(defgroup kaolin-blossom nil
  "Kaolin blossom theme options."
  :group 'kaolin-themes)

(defcustom kaolin-blossom-bg nil
  "Use alternative brighter background."
  :type 'boolean
  :group 'kaolin-blossom)

(define-kaolin-theme blossom  "Kaolin theme based on orange and purple shades with dark pink background."
  ;; Palette modification
  (
   ;; Colors
   (spring-green6 "#3e594e")
   (aquamarine3   "#53E6B5")
   (aquamarine4   "#518270")
   ;; (cerulean4     "#536a9d")
   (cerulean4     "#47629E")
   (new-cyan      "#8ee6d6")
   (vermilion3    "#ee7042")
   (vermilion4    "#CD8266")
   (amber3        "#EED47E")
   (crimson4      "#BA5B75")

   ;; Color vars
   (bg0 "#1C1616" black0)
   (bg1 "#2E2025" black1)
   (bg2 "#33242A" black2)
   (bg3 "#3A2930" black3)
   (bg4 "#453038" black4)

   (fg1 yellow9)

   ;; Root colors
   (kaolin-black   bg2)
   (kaolin-red     red3)
   (kaolin-green   spring-green3)
   (kaolin-yellow  yellow3)
   (kaolin-blue    azure3)
   (kaolin-magenta magenta3)
   (kaolin-cyan    cyan3)
   (kaolin-white   fg1)

   (keyword     orange4)
   (builtin     amber3)

   (var       purple4)
   (const     magenta3)

   (type      vermilion4)

   ;; (comment "#635C4A")
   ;; (comment brown7)
   (comment     "#6B4B53")
   (comment-alt green7)
   (comment-contrast "#8A636D")

   (str         new-cyan)
   (str-alt     cyan4)
   (doc         str-alt)

   (prep        crimson4)
   (num         crimson3)
   (bool        num)
   (warning     amber0)
   (err         red3)

   (dim-buffer white0)
   (hl         vermilion3)
   ;; TODO: colored
   (hl-line    (if kaolin-themes-hl-line-colored brown6 bg3))
   (hl-indent  "#453947")
   (selection bg4)
   (pulse bg4)

   (todo red3)

   (tooltip-hl-bg bg3)
   (tooltip-hl-fg hl)

   (rb1 purple4)
   (rb2 amber3)
   (rb3 violet4)
   (rb4 cyan1)
   (rb5 spring-green1)
   (rb6 amber3)
   (rb7 magenta3)
   (rb8 brown3)
   (rb9 crimson3)

   (diff-add teal1)
   (diff-mod orange1)
   (diff-rem crimson4)

    ;; Mode-line
   (line-fg           fg3)
   (line-color2       str)
   (line-bg1          bg2)
   (line-bg2          bg3)
   (line-border       (if kaolin-themes-modeline-border bg4 line-bg1))

   ;; Telephone-line
   (segment-active    gray2)
   (segment-inactive  gray2)

   (win-border    bg3)
   (evil-normal   amber3)
   (line-num-fg   comment)
   (line-num-hl   purple4)

   (cursor        fg1)

   (ivy1          fg3)
   (search1       azure3)
   (search2       lime3)
   (search3       red3))

  (
   ;; (highlight-quoted-symbol  (:foreground num))

   (org-document-title     (:foreground header :bold bold))
   ;; (org-document-info      (:foreground brown3))

   ;; TODO:
   (org-level-1            (:foreground keyword :bold bold :height kaolin-org-heading-size))
   (org-level-2            (:foreground functions  :bold nil))
   (org-level-3            (:foreground str :bold nil))
   (org-level-4            (:foreground builtin :bold nil))
   (org-code               (:foreground functions))
   (org-verbatim           (:foreground str))
   (org-list-dt            (:foreground str)))

  (when kaolin-themes-git-gutter-solid
    (custom-theme-set-faces
     'kaolin-blossom
     `(git-gutter:added     ((t (:background ,diff-add :foreground ,diff-add))))
     `(git-gutter:modified  ((t (:background ,diff-mod :foreground ,diff-mod))))
     `(git-gutter:deleted   ((t (:background ,diff-rem :foreground ,diff-rem)))))))

;;; kaolin-blossom-theme.el ends here
