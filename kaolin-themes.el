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

(defcustom kaolin-themes-underline-wave nil
  "When t, use the wave underline style instead of regular underline."
  :group 'kaolin-themes)


(defcustom kaolin-themes-hl-line-colored nil
  "When t, will display colored hl-line style instead dim gray."
  :group 'kaolin-themes)

(defcustom kaolin-themes-italic-comments nil
  "If t, enable italic style in comments."
  :group 'kaolin-themes)

;; TODO: implement
;; (defcustom kaolin-themes-comment-style 'normal
;;   "Sets the style of comments: normal, alt(darker for dark theme and lighter for light themes) or colored."
;;   :options '(bright normal color)
;;   :group 'kaolin-themes)

;; (pcase kaolin-themes-comment-style
;;   ('normal (message "Normal!"))
;;   ('bright (message "bright!")))

(defcustom kaolin-themes-git-gutter-solid nil
  "If t, display solid line to highlight git-gutter changes in fringe."
  :group 'kaolin-themes)

(defcustom kaolin-themes-distinct-fringe nil
  "Enable distinct background for fringe and line numbers."
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

(defun kaolin-themes-get-hex (name)
  (car (map-elt kaolin-palette name)))

;; TODO (??) add ability to create own variables in themes with vars from lib
;; like (my-new-theme-color cyan1)
(defmacro define-kaolin-theme (name doc &optional opt-palette opt-faces &rest body)
  "Define new Kaolin theme, using NAME as part of full kaolin-<name> theme name."
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
