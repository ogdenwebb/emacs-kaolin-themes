;;; kaolin-themes.el --- A set of eye pleasing themes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2018 ogdenwebb

;; Author: Ogden Webb <ogdenwebb@gmail.com>
;; URL: https://github.com/ogdenwebb/emacs-kaolin-themes
;; Package-Requires: ((emacs "25.1") (autothemer "0.2.2") (cl-lib "0.6"))
;; Version: 1.3.5

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Kaolin is a set of eye pleasing themes for GNU Emacs
;; With support a large number of modes and external packages.
;; Kaolin themes are based on the pallete that was originally
;; inspired by Sierra.vim with adding some extra colors.
;;
;; =======  This package includes the following themes  =======
;;
;;  * kaolin-dark - a dark jade variant inspired by Sierra.vim.
;;  * kaolin-light - light variant of the original kaolin-dark.
;;  * kaolin-eclipse - a dark purple variant.
;;  * kaolin-ocean - dark blue variant.
;;  * kaolin-galaxy - bright theme based on one of the Sebastian Andaur arts.
;;  * kaolin-aurora - Kaolin meets polar lights.
;;  * kaolin-bubblegum - Kaolin colorful theme with dark blue background.
;;  * kaolin-valley-dark - colorful Kaolin theme with brown background.
;;  * kaolin-valley-light - light version of kaolin-valley-dark theme.
;;  * kaolin-mono-dark - almost monochrome dark green Kaolin theme.
;;
;;
;; =======  Configuration example  =======
;;
;; (require 'kaolin-themes)
;;
;; (load-theme 'kaolin-dark)
;;
;; =======  Custom theme settings  =======
;;
;;  ;; The following set to t by default
;;  (setq kaolin-themes-bold t       ; If nil, disable the bold style.
;;        kaolin-themes-italic t     ; If nil, disable the italic style.
;;        kaolin-themes-underline t) ; If nil, disable the underline style.
;;
;; =======  Some extra theme features, disabled by default  =======
;;
;;  ;; If t, use the wave underline style instead of regular underline.
;;  (setq kaolin-themes-underline-wave t)
;;
;;  ;; When t, will display colored hl-line style
;;  (setq kaolin-themes-hl-line-colored t)
;;
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)
(require 'map)
(require 'color)

(require 'kaolin-themes-lib)


(defgroup kaolin-themes nil
  "Kaolin theme properties."
  :group 'faces)

(defvar kaolin-themes-current-palette nil
  "List with colors from current Kaolin theme.")

(defcustom kaolin-themes-bold t
  "If nil, disable the bold style."
  :group 'kaolin-themes)

(defcustom kaolin-themes-italic t
  "If nil, disable the italic style."
  :group 'kaolin-themes)

(defcustom kaolin-themes-underline t
  "If nil, disable the underline style."
  :group 'kaolin-themes)

(defcustom kaolin-themes-underline-wave nil
  "When t, use the wave underline style instead of regular underline."
  :group 'kaolin-themes)


(defcustom kaolin-themes-hl-line-colored nil
  "When t, will display colored hl-line style instead dim gray."
  :group 'kaolin-themes)

(defcustom kaolin-themes-italic-comments nil
  "If t, enable italic style in comments."
  :group 'kaolin-themes)

(defcustom kaolin-themes-comments-style 'normal
  "Sets the style of comments: normal, bright or color."
  :options '(normal bright dark color)
  :group 'kaolin-themes)

(defcustom kaolin-themes-git-gutter-solid nil
  "If t, display solid line to highlight git-gutter changes in fringe."
  :group 'kaolin-themes)

(defcustom kaolin-themes-distinct-fringe nil
  "Enable distinct background for fringe and line numbers."
  :group 'kaolin-themes)

(defcustom kaolin-themes-distinct-company-scrollbar nil
  "Enable distinct colors for company popup scrollbar."
  :group 'kaolin-themes)

(defface kaolin-themes-boolean nil
  "Face to highlight boolean values"
  :group 'kaolin-themes)

(define-obsolete-variable-alias 'kaolin-bold 'kaolin-themes-bold "1.3.4")
(define-obsolete-variable-alias 'kaolin-italic 'kaolin-themes-italic "1.3.4")
(define-obsolete-variable-alias 'kaolin-underline 'kaolin-themes-underline "1.3.4")
(define-obsolete-variable-alias 'kaolin-wave 'kaolin-themes-underline-wave "1.3.4")
(define-obsolete-variable-alias 'kaolin-hl-line-colored 'kaolin-themes-hl-line-colored "1.3.4")
(define-obsolete-variable-alias 'kaolin-italic-comments 'kaolin-themes-italic-comments "1.3.4")
(define-obsolete-variable-alias 'kaolin-git-gutter-solid 'kaolin-themes-git-gutter-solid "1.3.4")
(define-obsolete-variable-alias 'kaolin-wave 'kaolin-themes-underline-wave "1.3.4")

(defun kaolin-themes--make-name (sym)
  "Format kaolin-<sym> from SYM."
  (intern (format "kaolin-%s" (symbol-name sym))))

(defun kaolin-themes--merge-alist (base-alist add-alist)
  "Add elements to BASE-LIST from ADD-LIST without dublicates."
  (let ((res (copy-alist base-alist)))
    (cl-loop for el in add-alist
             do (map-put res (car el) (cdr el)))
    res))

(defun kaolin-themes--extract-let-block (palette n)
  "Extract a variable definition block from PALETTE containing all color definitions corresponding to display type #N."
  (cl-loop for row in palette
           collect (list (car row) (elt row (1+ n)))))

(defun kaolin-themes-palette-value-p (palette name)
  "Check if palette contains value by key. Return nil if not."
  (map-contains-key palette name))

;; TODO: support for display variable
(defun kaolin-themes-palette-get (name palette &optional display)
  "Return value in kaolin-pallete by NAME if defined, otherwise nil."
  (when (and (mapp palette) (symbolp name))
    (map-elt palette name)))

(defun kaolin-themes-palette-names (palette)
  (when (mapp palette)
    (map-keys palette)))

;; (defmacro kaolin-themes-get-value (name &optional default-palette)
;;   "Return final value"
;;   (let* ((palette (if default-palette kaolin-palette kaolin-themes-current-palette))
;;         (value (kaolin-themes-palette-get name palette)))
;;     (when value
;;       (cond ((listp value) (let)))

(defun kaolin-themes-get-hex (name &optional default-palette)
  "Return hex value of color from `kaolin-pallete' by NAME"
  (let ((value (kaolin-themes-get-color name default-palette)))
      (cond ((nested-alist-p value) (message "write func to replace nil values"))
            ((stringp value) value)
            ((and (boundp value) (symbolp value)) (symbol-value value))
            (t (kaolin-themes-get-hex value default-palette)))))

(defmacro kaolin-themes-name-to-rgb (name &optional default-palette)
  "Convert color from `kaolin-palette' by name to a list of normalized RGB components."
  `(color-name-to-rgb (kaolin-themes-get-hex ,name ,default-palette)))

(defmacro kaolin-themes-complement (name &optional default-palette)
  "Return the color that is the complement of color with NAME in palette."
  `(color-complement (kaolin-themes-get-hex ,name ,default-palette)))

(defmacro kaolin-themes-complement-hex (name &optional default-palette)
  "Return the color that is the complement of NAME, in hexadecimal format."
  `(apply 'kaolin-rgb-to-hex (kaolin-themes-complement ,name ,default-palette)))

(defmacro kaolin-themes-saturate-name (name percent &optional default-palette)
  "Make a color with NAME in `kaolin-palette' more saturated by PERCENT."
  `(color-saturate-name (kaolin-themes-get-hex ,name ,default-palette) ,percent))

(defmacro kaolin-themes-desaturate-name (name percent &optional default-palette)
  "Make a color with NAME in `kaolin-palette' less saturated by PERCENT."
  `(color-desaturate-name (kaolin-themes-get-hex ,name ,default-palette) ,percent))

(defmacro kaolin-themes-lighten-name (name percent &optional default-palette)
  "Make a color with NAME in `kaolin-palette' lighten by PERCENT."
  `(color-lighten-name (kaolin-themes-get-hex ,name ,default-palette) ,percent))

(defmacro kaolin-themes-darken-name (name percent &optional default-palette)
  "Make a color with NAME in `kaolin-palette' darker by PERCENT."
  `(color-lighten-name (kaolin-themes-get-hex ,name ,default-palette) (- ,percent)))

(defalias 'kaolin-color          'kaolin-themes-get-hex)
(defalias 'kaolin-complement     'kaolin-themes-complement)
(defalias 'kaolin-complement-hex 'kaolin-themes-complement-hex)
(defalias 'kaolin-rgb-to-hex     'color-rgb-to-hex)
(defalias 'kaolin-rgb-to-hsv     'color-rgb-to-hsv)
(defalias 'kaolin-hsl-to-rgb     'color-hsl-to-rgb)
(defalias 'kaolin-rgb-to-hsl     'color-rgb-to-hsl)

(defalias 'kaolin-saturate 'kaolin-themes-saturate-name)
(defalias 'kaolin-saturate 'kaolin-themes-desaturate-name)
(defalias 'kaolin-lighten  'kaolin-themes-lighten-name)
(defalias 'kaolin-darken   'kaolin-themes-darken-name)

(defalias 'kaolin-themes--current-theme 'autothemer--current-theme)

;; Taken from autothemer package
(defun kaolin-themes--reduced-spec-to-facespec (display reduced-specs)
  "Create a face spec for DISPLAY, with specs REDUCED-SPECS.
E.g., (kaolin--spec-to-facespec '(min-colors 60)
'(button (:underline t :foreground red)))
-> `(button (((min-colors 60) (:underline ,t :foreground
,red))))."
  (let* ((face (elt reduced-specs 0))
         (properties (elt reduced-specs 1))
         (spec (kaolin-themes--demote-heads `(list (,display ,properties)))))
    `(list ',face ,spec)))

(defun kaolin-themes--demote-heads (expr)
  "Demote every list head within EXPR by one element.
E.g., (a (b c d) e (f g)) -> (list a (list b c d) e (list f g))."
  (if (listp expr)
      `(list ,@(mapcar (lambda (it) (if (and (listp it) (not (eq (car it) 'quote)))
                                        (kaolin-themes--demote-heads it) it))
                       expr))
    expr))

;; (defmacro kaolin-themes--make-faces (&rest faces)
;;   (let* ((temp-colorname (make-symbol "colorname"))
;;          (temp-color (make-symbol "color")))
;;   `(let (,@(kaolin-themes--extract-let-block kaolin-themes-current-palette 0))
;;     ,(cl-loop for face in faces
;;              collect (kaolin-themes--reduced-spec-to-facespec t face))))
;;     )


;; (kaolin-themes-set-faces
;;  'kaolin-dark
;;  '(font-lock-keyword-face (:foreground red3 :bold t)))

;; (kaolin-themes--make-faces
;;  '(font-lock-keyword-face (:foreground red3 :bold t)))

;; (kaolin-themes--reduced-spec-to-facespec
;;  t
;;  '(font-lock-keyword-face (:foreground red3 :bold t)))

;; => (list (quote font-lock-keyword-face) (list list (list t (list :foreground red3 :bold t))))



;;;###autoload
(defmacro kaolin-themes-set-faces (name &rest faces)
  "Call `custom-themes-set-faces' for Kaolin NAME theme with reduced spec list of FACES."
  `(custom-theme-set-faces
    ,name
    ,@(mapcar #'kaolin-themes--make-faces faces)))

;;;###autoload
(defmacro define-kaolin-theme (name doc &optional opt-palette opt-faces &rest body)
  "Define new Kaolin theme, using NAME as part of full kaolin-<name> theme name.
OPT-PALETTE and OPT-FACES specify optional extra palette and faces for a theme.
BODY can evaluate ordinary elisp code"
  (let* ((kaolin-theme-name (kaolin-themes--make-name name))
         (kaolin-theme-faces (if opt-faces
                                   (kaolin-themes--merge-alist kaolin-faces opt-faces)
                               kaolin-faces)))

    (setq kaolin-themes-current-palette (if opt-palette
                                               (kaolin-themes--merge-alist kaolin-palette opt-palette)
                                             kaolin-palette))

    `(autothemer-deftheme ,kaolin-theme-name ,doc

                          ((((class color) (min-colors #xFFFFFF)) ; 24bit gui
                            ((class color) (min-colors #xFF))     ; 256
                            t)                                    ; tty

                           ;; Set palette
                           ,@kaolin-themes-current-palette)

                          ;; Set faces
                          ,kaolin-theme-faces

                          ;; Set vars or execute an arbitrary function body
                           ,@body

                           ;; Provide theme
                           (provide-theme ',kaolin-theme-name))))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'kaolin-themes)

;;; kaolin-themes.el ends here
