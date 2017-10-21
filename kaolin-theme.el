;;; kaolin-theme.el --- A dark jade theme inspired by Sierra.vim
;; TODO: fix title

;; Copyright (C) 2017 ogdenwebb

;; Author: ogdenwebb <ogdenwebb@gmail.com>
;; URL: https://github.com/ogdenwebb/kaolin-theme
;; Package-Requires: ((emacs "24") (autothemer "0.2.2"))
;; TODO: add release notes
;; Version: 1.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;;; Commentary:
;;
;; TODO: (REWRITE)
;; Kaolin is a dark jade, eye pleasing theme for Emacs with support
;; a large number of specific modes and external packages.
;;
;;; Code:

(require 'autothemer)

(require 'kaolin-theme-lib)

(defgroup kaolin-theme nil
  "Kaolin theme properties"
  :group 'faces)

(defcustom kaolin-bold t
  "If nil, disable the bold style."
  :group 'kaolin-theme)

(defcustom kaolin-italic t
  "If nil, disable the italic style."
  :group 'kaolin-theme)

(defcustom kaolin-underline t
  "If nil, disable the underline style."
  :group 'kaolin-theme)

(defcustom kaolin-wave nil
  "When t, use the wave underline style instead of regular underline."
  :group 'kaolin-theme)

;; TODO: add colored selection option
(defcustom kaolin-hl-line-colored nil
  "When t, will display colored hl-line style instead dim gray"
  :group 'kaolin-theme)

(defface kaolin-boolean nil
  "Face to highlight boolean values"
  :group 'kaolin-theme)

;; TODO: message about kaolin -> kaolin-dark
;; TODO: (!!) use var instead of hardcoded colors if possible
;; TODO: (??) add base(terminal) colors
;; TODO: (??) colorful comments
;; TODO: treemacs
;; TODO: (??) color cornflower blue
;; TODO: (??) add -pkg.el
;; TODO: Add theme based on nim aporia: 400 roses
;; TODO: make a simple text logo
;; TODO: choose a response to use as slogan:
;; That which is arises from that which is not.
;; I am shaped to a higher order.
;; For each tool, a purpose.
;; Life's wheel goes round and round.
;; The end of the path is the beginning.
;; Like a beaten blade, I come back sharper.

(defun kaolin-theme--make-name (sym)
  "Format kaolin-<sym> from SYM."
  (intern (format "kaolin-%s" (symbol-name sym))))

;; Literally it's evil-add-to-alist.
(defun kaolin-theme--add-to-alist (list-var key val &rest elements)
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
        (apply #'kaolin-theme--add-to-list list-var elements)
      (symbol-value list-var))))

;; TODO: read about lexical bindings
(defun kaolin-theme--merge-alist (base-alist add-alist)
  "Add elements to BASE-LIST from ADD-LIST to BASE-LIST without dublicates."
  (let ((res (copy-alist base-alist)))
    (cl-loop for el in add-alist
            do (kaolin-theme--add-to-alist 'res (car el) (cdr el))
            return res)))

;;;###autoload
;; TODO: try to fix color/face overwriting after load-theme
(defmacro define-kaolin-theme (name doc &optional opt-palette opt-faces &rest body)
  "Define new Kaolin theme, using NAME as part of full kaolin-<name> theme name."
  (let* ((kaolin-theme-name (kaolin-theme--make-name name))
         (kaolin-theme-palette (if opt-palette
                                   (kaolin-theme--merge-alist kaolin-palette opt-palette)
                                 kaolin-palette))
         (kaolin-theme-faces (if opt-faces
                                   (kaolin-theme--merge-alist kaolin-faces opt-faces)
                               kaolin-faces)))

    `(autothemer-deftheme ,kaolin-theme-name ,doc
                          ;; TODO: choose classes what I need
                          ((((class color) (min-colors 32000)) ((class color) (min-colors 89)) t)

                           ;; Set palette
                           ,@kaolin-theme-palette)

                          ;; Set faces
                          ,kaolin-theme-faces

                          ;; Set vars or execute an arbitrary function body
                          ;; TODO: add/test colors vars support
                           ,@body)))


;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (let* ((base (file-name-directory load-file-name))
         (dir (expand-file-name "themes/" base)))
    (add-to-list 'custom-theme-load-path
                 (or (and (file-directory-p dir) dir)
                     base))))

(provide 'kaolin-theme)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; kaolin-theme.el ends here
