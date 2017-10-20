;;; kaolin-eclipse-theme.el --- Dark purple Kaolin theme variant
;;; Commentary:

;;; Code:
(require 'kaolin-theme)

(autothemer-deftheme kaolin-eclipse "Dark purple Kaolin theme variant."

    ((((class color) (min-colors 32000)) ((class color) (min-colors 89)) t)

     ;; Palette
     (reddish "#dd6611" nil "#FF0000")
     (yellowish "#dddd22" "#FFFF00")
     (not-quite-yellowish "#dcdc22" yellowish yellowish))

     ;; Face specifications
    (
     (default (:background "black" :foreground yellowish))
     (button (:underline t :weight 'bold :foreground yellowish))
     (error (:foreground reddish))))


;;; kaolin-eclipse-theme.el ends here
