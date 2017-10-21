;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-theme)

(define-kaolin-theme light  "Light Kaolin theme variant."
  ;; Palette modification
  ((bg1 white4)
   (bg2 white3)
   (bg2 white3)
   (fg1 black4)
   (fg2 black3)
   (fg3 black2)
   (fg4 black1)
   (hl-line "dim gray")
   (keyword red))

  ;; Set theme faces
  (
   (default (:background bg2 :foreground fg2))
   (hl-line (:background "dim gray" :foreground bg3))))


;;; kaolin-eclipse-theme.el ends here
