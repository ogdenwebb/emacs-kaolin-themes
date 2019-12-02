;;; kaolin-themes-treemacs.el --- treemacs customization for Kaolin themes -*- lexical-binding: t; no-byte-compile: t; -*-
;;; Commentary:

;;; Code:
(unless (require 'all-the-icons nil t)
  (error "Kaolin treemacs theme requires the all-the-icons package."))

(defgroup kaolin-treemacs nil
  "Settings for Kaolin's treemacs theme."
  :group 'kaolin-themes)

(defcustom kaolin-themes-treemacs-hl-line nil
  "Remap hl-line face for treemacs, uses distinct foreground and default background."
  :type 'boolean
  :group 'kaolin-treemacs)

(defcustom kaolin-themes-treemacs-icons t
  "Use predefined icons for Kaolin themes from the all-the-icons package."
  :type 'boolean
  :group 'kaolin-treemacs)

(defcustom kaolin-themes-treemacs-line-spacing 1
  "Line-spacing for treemacs buffer."
  :type 'number
  :group 'kaolin-treemacs)

(defun kaolin-treemacs--remove-fringes ()
  "Remove fringes in treemacs window."
  (when (display-graphic-p)
    (setq left-fringe-width 0)
    (setq right-fringe-width 0)))

(defun kaolin-treemacs--remove-modeline ()
  "Disable mode-line in treemacs buffer "
  (setq mode-line-format nil))

;; TODO: remap hl-line after load-theme
(defun kaolin-treemacs--remap-hl-line ()
  "Remap hl-line face."
  (face-remap-add-relative 'hl-line `(:background ,(face-background 'default) :foreground ,(face-foreground 'lazy-highlight))))

(defun kaolin-treemacs--hook ()
  (setq line-spacing kaolin-themes-treemacs-line-spacing
        tab-width 1))

(with-eval-after-load 'treemacs
  (add-hook 'treemacs-mode-hook #'kaolin-treemacs--hook)
  (add-hook 'treemacs-mode-hook #'kaolin-treemacs--remove-fringes)
  (advice-add 'treemacs-select-window :after #'kaolin-treemacs--remove-fringes)
  (add-hook 'treemacs-mode-hook #'kaolin-treemacs--remove-modeline)

  (when kaolin-themes-treemacs-hl-line
    (add-hook 'treemacs-mode-hook #'kaolin-treemacs--remap-hl-line))

  (setq treemacs-indentation 1
        treemacs-indentation-string "  ")

  (when kaolin-themes-treemacs-icons

    (treemacs-create-theme "kaolin"
      :config
      (progn
        ;; Set fallback icon
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-text"
                                                    :height 1.10
                                                    :v-adjust 0.1))
         :extensions (fallback))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "subject"
                                                   :v-adjust -0.2
                                                   :height 1.3
                                                   :face 'font-lock-variable-name-face))
         :extensions (root))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "folder_open"
                                               ;; :v-adjust 0.05
                                               :height 1.1))
         ;; :face 'font-lock-doc-face))
         :extensions (dir-open))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "folder"
                                                   ;; :v-adjust 0.05
                                                   :height 1.1))

         :extensions (dir-closed))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "close"
                                             :size 1.0
                                             ;; :v-adjust 0.1
                                             :face 'font-lock-keyword-face))
         :extensions (tag-open))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-faicon "chevron-down"
                                             :size 0.9
                                             :v-adjust 0.1
                                             :face 'font-lock-keyword-face))
         :extensions (tag-closed))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-faicon "tag"
                                             :height 0.9
                                             :face 'font-lock-type-face))
         :extensions (tag-leaf))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "error"
                                             :height 0.9
                                             :face 'error))
                              :extensions (error)
                              :fallback (propertize "• " 'face 'font-lock-warning-face))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "warning"
                                                     :height 0.9
                                                     :face 'error))
         :extensions (warning)
         :fallback (propertize "• " 'face 'font-lock-string-face))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "info"
                                                     :height 0.9
                                                     :face 'font-lock-string-face))
         :extensions (info)
         :fallback (propertize "• " 'face 'font-lock-string-face))

        ;; Icons for filetypes
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "csharp-line"))
                              :extensions ("cs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "css3")) :extensions ("css"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "git")) :extensions ("gitignore" "git" "gitconfig" "gitmodules"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "html5")) :extensions ("html" "htm"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "java")) :extensions ("java" "jar"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "python")) :extensions ("py"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "rust")) :extensions ("rs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "haskell")) :extensions ("hs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "c")) :extensions ("c" "h"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "crystal")) :extensions ("cr"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "cplusplus")) :extensions ("cpp" "cxx" "hpp" "tpp" "cc" "hh"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "ruby-alt")) :extensions ("rb"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "scala")) :extensions ("scala"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "elixir")) :extensions ("ex" "exs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "erlang")) :extensions ("erl" "hrl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "clojure")) :extensions ("clj" "cljs" "cljc"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "cabal")) :extensions ("cabal"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "clisp")) :extensions ("lisp"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "go")) :extensions ("go"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "elisp" :v-adjust -0.15)) :extensions ("el" "elc"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "julia")) :extensions ("jl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "kotlin")) :extensions ("kt" "kts"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "hy")) :extensions ("hy"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "javascript-badge")) :extensions ("js"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jsx2-alt")) :extensions ("jsx"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "ocaml")) :extensions ("ml" "mli"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "org")) :extensions ("org"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "php")) :extensions ("php"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "terminal")) :extensions ("sh" "zsh" "fish"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "typescript")) :extensions ("ts"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "nimrod")) :extensions ("nim" "nims"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "perl")) :extensions ("pl" "pm" "perl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "perl6")) :extensions ("pm6"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "R")) :extensions ("r"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "tex")) :extensions ("tex"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "rst")) :extensions ("rst"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vue")) :extensions ("vue"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "markdown" :v-adjust 0.05)) :extensions ("md" "markdown"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "file-pdf")) :extensions ("pdf"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "database")) :extensions ("sql"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-material "style")) :extensions ("styles"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "lua")) :extensions ("lua"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "asciidoc")) :extensions ("adoc" "asciidoc"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "sbt")) :extensions ("sbt"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "puppet")) :extensions ("pp"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jinja")) :extensions ("j2" "jinja2"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "dockerfile")) :extensions ("dockerfile"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vagrant")) :extensions ("vagrantfile"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "racket")) :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "reason")) :extensions ("re" "rei"))


        ;; Media files icon
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-media" :v-adjust 0.1))
         :extensions ("jpg" "jpeg" "png" "gif" "ico" "tif" "tiff" "svg" "bmp"
                      "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "webp"
                      "mkv" "wav" "mp3" "ogg" "midi"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-text"
                       :height 1.1
                       :v-adjust 0.05))
         :extensions ("rst" "log" "txt" "contribute" "license" "readme" "changelog"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "cogs"))
         :extensions ("conf" "cfg" "yaml" "yml" "json" "xml" "toml" "cson" "ini"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "code"))
         :extensions ("tpl" "erb" "mustache" "twig" "ejs" "mk" "haml" "pug" "jade"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-zip"))
         :extensions ("zip" "xz" "tar" "gz" "7z" "rar"))
        ))

    (treemacs-load-theme "kaolin"))
  )


(provide 'kaolin-themes-treemacs)
;;; kaolin-themes-treemacs.el ends here
