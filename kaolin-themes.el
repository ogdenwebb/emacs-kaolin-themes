;;; kaolin-themes.el --- A set of eye pleasing themes  -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019 ogdenwebb

;; Author: Ogden Webb <ogdenwebb@gmail.com>
;; URL: https://github.com/ogdenwebb/emacs-kaolin-themes
;; Package-Requires: ((emacs "25.1") (autothemer "0.2.2") (cl-lib "0.6"))
;; Version: 1.5.4
;; Keywords: dark light teal blue violet purple brown theme faces

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
;; -------  This package includes the following themes  -------
;;
;;  * kaolin-dark - a dark jade variant inspired by Sierra.vim.
;;  * kaolin-light - light variant of the original kaolin-dark.
;;  * kaolin-aurora - Kaolin meets polar lights.
;;  * kaolin-bubblegum - Kaolin colorful theme with dark blue background.
;;  * kaolin-eclipse - a dark purple variant.
;;  * kaolin-temple - dark brown background with syntax highlighting based on orange and cyan shades.
;;  * kaolin-galaxy - bright theme based on one of the Sebastian Andaur arts.
;;  * kaolin-ocean - dark blue variant.
;;  * kaolin-valley-dark - colorful Kaolin theme with brown background.
;;  * kaolin-valley-light - light version of kaolin-valley-dark theme.
;;  * kaolin-mono-dark - almost monochrome dark green Kaolin theme.
;;
;;
;; -------  Configuration example  -------
;;
;;  (require 'kaolin-themes)
;;  (load-theme 'kaolin-dark t)
;;
;;  ;; Apply treemacs customization for Kaolin themes, requires the all-the-icons package.
;;  (kaolin-treemacs-theme)

;;  ;; Or if you have use-package installed
;;  (use-package kaolin-themes
;;    :config
;;    (load-theme 'kaolin-dark t)
;;    (kaolin-treemacs-theme))
;;
;;  ;;  Custom theme settings
;;
;;  ;; The following set to t by default
;;  (setq kaolin-themes-bold t       ; If nil, disable the bold style.
;;        kaolin-themes-italic t     ; If nil, disable the italic style.
;;        kaolin-themes-underline t) ; If nil, disable the underline style.
;;
;; -------  Some extra theme features, disabled by default  -------
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

(defcustom kaolin-themes-bold t
  "If nil, disable the bold style."
  :group 'kaolin-themes)

(defcustom kaolin-themes-italic t
  "If nil, disable the italic style."
  :group 'kaolin-themes)

(defcustom kaolin-themes-underline t
  "If nil, disable the underline style."
  :group 'kaolin-themes)

(defcustom kaolin-themes-underline-wave t
  "When t, use the wave underline style to highlight warnings and error."
  :group 'kaolin-themes)

(defcustom kaolin-themes-hl-line-colored nil
  "When t, will display colored hl-line style instead dim gray."
  :group 'kaolin-themes)

(defcustom kaolin-theme-linum-hl-line-style nil
  "When t, enable same style for hl-line and line number faces.")

(defcustom kaolin-themes-italic-comments nil
  "If t, enable italic style in comments."
  :group 'kaolin-themes)

(defcustom kaolin-themes-comments-style 'normal
  "Sets the style of comments: normal, alt(darker for dark theme and lighter for light themes) or colored."
  :options '(normal bright color)
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

(defcustom kaolin-themes-org-scale-headings t
  "If not-nil, scale heading size in org-mode."
  :group 'kaolin-themes)

(defcustom kaolin-themes-modeline-border t
  "If not-nil, enable distinct border in mode-line."
  :group 'kaolin-themes)

(defcustom kaolin-themes-distinct-metakeys t
  "If not-nil, enable distinct color for metadata key (e.g. metakeys in org-mode).
Otherwise inherit from comments."
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
  "Add elements to BASE-LIST from ADD-LIST without dublicates. Returns a new list as result."
  (let ((res (copy-alist base-alist)))
    (cl-loop for el in add-alist
             do (map-put res (car el) (cdr el)))
    res))

;; (defun kaolin-themes-palette-get (name)
;;   "Return hex value of color in kaolin-pallete by NAME"
;;   (let ((val (car-safe (map-elt kaolin-palette name 'missing))))
;;     (if (and (not (null val))
;;              (not (eql val 'missing))
;;              (not (stringp val)))
;;         (kaolin-themes-palette-get val)
;;       val)))

(defmacro define-kaolin-theme (name doc &optional opt-palette opt-faces &rest body)
  "Define new Kaolin theme, using NAME as part of full kaolin-<name>,
the DOC argument provides a short description for new theme.

OPT-PALETTE is a list marks a optional theme palette which will be merged with the `kaolin-palette',
and OPT-FACES is a list for new theme faces. Any color defined within OPT-PALETTE
will override the original one,similar with faces from OPT-PALETTE and `kaolin-faces'.
In terms of kaolin-themes GNU Emacs, palette contains both colors
(such as blue1, orange2 and etc) and variables(bg1, var, functions, etc)
to highlight specific part of GNU Emacs.

Palette is a ordinary association list, e.g. ((color1 \"#ffffff\") (color2 \"#ff0000\")).
You can define your own color/variable (my-own-red \"#ff0000\") in HEX
or inherit a value from another variable (my-own-color red3).

Use kaolin-valley-dark-theme.el as example."
  (let* ((kaolin-theme-name (kaolin-themes--make-name name))
         (kaolin-theme-palette (if opt-palette
                                   (kaolin-themes--merge-alist kaolin-palette opt-palette)
                                 kaolin-palette))
         (kaolin-theme-faces (if opt-faces
                                   (kaolin-themes--merge-alist kaolin-faces opt-faces)
                               kaolin-faces)))

    `(autothemer-deftheme ,kaolin-theme-name ,doc

                          ((((class color) (min-colors #xFFFFFF)) ; 24bit gui
                            ((class color) (min-colors #xFF))     ; 256
                            t)                                    ; tty

                           ;; Set palette
                           ,@kaolin-theme-palette)

                          ;; Set faces
                          ,kaolin-theme-faces

                          ;; Set vars or execute an arbitrary function body
                           ,@body

                           ;; (custom-theme-set-faces ',kaolin-theme-name
                           ;;                         ,@kaolin-common-vars)

                           ;; Provide theme
                           (provide-theme ',kaolin-theme-name)
                           )))


;;;###autoload
(defun kaolin-treemacs-theme ()
  "Enable kaolin-themes treemacs theme with all-the-icons package."
  (require 'kaolin-themes-treemacs))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'kaolin-themes)

;;; kaolin-themes.el ends here
