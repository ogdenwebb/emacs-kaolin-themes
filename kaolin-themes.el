;;; kaolin-themes.el --- A set of eye pleasing themes

;; Copyright (C) 2017 ogdenwebb

;; Author: Ogden Webb <ogdenwebb@gmail.com>
;; URL: https://github.com/ogdenwebb/emacs-kaolin-themes
;; Package-Requires: ((emacs "24.3") (autothemer "0.2.2") (cl-lib "0.6"))
;; Version: 1.0.5

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
;;  * kaolin-dark - a dark jade variant inspired by Sierra.vim
;;  * kaolin-light - light variant of the original kaolin-dark
;;  * kaolin-eclipse - a dark purple variant
;;  * kaolin-ocean - dark blue variant
;;  * kaolin-tribal - theme based on Tribal color scheme by Dayle Rees.
;;  * kaolin-galaxy - bright theme based on one of the Sebastian Andaur arts.
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
;;  (setq kaolin-bold t       ; If nil, disable the bold style.
;;        kaolin-italic t     ; If nil, disable the italic style.
;;        kaolin-underline t) ; If nil, disable the underline style.
;;
;; =======  Some extra theme features, disabled by default  =======
;;
;;  ;; If t, use the wave underline style instead of regular underline.
;;  (setq kaolin-wave t)
;;
;;  ;; When t, will display colored hl-line style instead dim gray
;;  (setq kaolin-hl-line-colored t)
;;
;;
;;                           The end of the path is the beginning.
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(require 'kaolin-themes-lib)


(defgroup kaolin-themes nil
  "Kaolin theme properties"
  :group 'faces)

(defcustom kaolin-bold t
  "If nil, disable the bold style."
  :group 'kaolin-themes)

(defcustom kaolin-italic t
  "If nil, disable the italic style."
  :group 'kaolin-themes)

(defcustom kaolin-underline t
  "If nil, disable the underline style."
  :group 'kaolin-themes)

;; TODO: rename to kaolin-underline-wave
(defcustom kaolin-wave nil
  "When t, use the wave underline style instead of regular underline."
  :group 'kaolin-themes)

(defcustom kaolin-hl-line-colored nil
  "When t, will display colored hl-line style instead dim gray."
  :group 'kaolin-themes)

;; TODO:
(defcustom kaolin-comment-style 'normal
  "Sets the style of comments: normal, bright or colored."
  :options '(bright normal color)
  :group 'kaolin-themes)

(defcustom kaolin-git-gutter-solid nil
  "If t, display solid line to highlight git-gutter changes in fringe."
  :group 'kaolin-themes)

(defface kaolin-boolean nil
  "Face to highlight boolean values"
  :group 'kaolin-themes)

(defun kaolin-themes--make-name (sym)
  "Format kaolin-<sym> from SYM."
  (intern (format "kaolin-%s" (symbol-name sym))))

;; Literally it's evil-add-to-alist.
(defun kaolin-themes--add-to-alist (list-var key val &rest elements)
  "Add the assocation of KEY and VAL to the value of LIST-VAR.
If the list already contains an entry for KEY, update that entry;
otherwise add at the end of the list."
  (let ((tail (symbol-value list-var)))
    (while (and tail (not (equal (car-safe (car-safe tail)) key)))
      (setq tail (cdr tail)))
    (if tail
        (setcar tail (cons key val))
      (set list-var (append (symbol-value list-var)
                            (list (cons key val)))))
    (if elements
        (apply #'kaolin-themes--add-to-alist list-var elements)
      (symbol-value list-var))))

;; TODO: preasubmly cant add extra vars from theme file that doesn't exist in const
;; TODO: rewrite adding in pure style
(defun kaolin-themes--merge-alist (base-alist add-alist)
  "Add elements to BASE-LIST from ADD-LIST to BASE-LIST without dublicates."
  (let ((res (copy-alist base-alist)))
    (cl-loop for el in add-alist
             do (kaolin-themes--add-to-alist 'res (car el) (cdr el)))
    res))

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
