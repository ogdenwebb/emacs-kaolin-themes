;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-theme)

(define-kaolin-theme light  "Light Kaolin theme variant."
  ;; Palette modification
  ((yellow  "white")
   (hl-line "dim gray"))

  ;; Set theme faces
  (
   (default (:background fg1 :foreground bg3))))


;;; kaolin-eclipse-theme.el ends here
