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

(defcustom kaolin-themes-treemacs-modeline nil
  "Whether display mode-line in treemacs buffer, by default is nil."
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
  (unless kaolin-themes-treemacs-modeline
    (add-hook 'treemacs-mode-hook #'kaolin-treemacs--remove-modeline))

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
         :icon (format " %s " (all-the-icons-octicon "book"
                                                    :height 1.10
                                                    :v-adjust 0.0))
         :extensions (fallback)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "subject"
                                                   :v-adjust -0.2
                                                   :height 1.3
                                                   :face 'font-lock-variable-name-face))
         :extensions (root-closed root-open)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "folder_open"
                                               ;; :v-adjust 0.05
                                               :height 1.1))
         ;; :face 'font-lock-doc-face))
         :extensions (dir-open)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "folder"
                                                   ;; :v-adjust 0.05
                                                   :height 1.1))
         :extensions (dir-closed)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "close"
                                             :size 1.0
                                             ;; :v-adjust 0.1
                                             :face 'font-lock-keyword-face))
         :extensions (tag-open)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-faicon "chevron-down"
                                             :size 0.9
                                             :v-adjust 0.1
                                             :face 'font-lock-keyword-face))
         :extensions (tag-closed)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-faicon "tag"
                                             :height 0.9
                                             :face 'font-lock-type-face))
         :extensions (tag-leaf)
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "book"))
         :extensions (license)
         :fallback'same-as-icon)

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
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "assembly")) :extensions ("asm") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "apache")) :extensions ("apache") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "apple")) :extensions ("scpt") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "audacity")) :extensions ("aup" "aup3") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-faicon "bar-chart")) :extensions ("dat") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "csharp-line")) :extensions ("cs" "csx") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "css3")) :extensions ("css") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "fsharp")) :extensions ("fs" "fsi" "fsx" "fsscript") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "git")) :extensions ("gitignore" "git" "gitattributes" "gitconfig" "gitmodules") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "git-merge")) :extensions ("MERGE_") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "git-commit")) :extensions ("COMMIT_EDITMSG") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "haml")) :extensions ("inky-haml" "haml") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "html5")) :extensions ("html" "htm") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "moustache")) :extensions ("hbs") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "java" :v-adjust 0.02)) :extensions ("java" "jar") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jade")) :extensions ("jade") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "python" :v-adjust 0.02)) :extensions ("py") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "prolog")) :extensions ("prol" "prolog") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "rust" :v-adjust 0.02)) :extensions ("rs" "rlib") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "haskell" :v-adjust 0.05)) :extensions ("hs" "chs" "lhs" "hsc") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "c" :v-adjust 0.02)) :extensions ("c" "h") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "cmake")) :extensions ("cmake" "CMakeCache.txt" "CMakeLists.txt") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "crystal")) :extensions ("cr") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "cplusplus" :v-adjust 0.02)) :extensions ("cpp" "cxx" "hpp" "tpp" "cc" "hh") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "ruby-alt" :v-adjust 0.05)) :extensions ("rb" "Gemfile" "Gemfile.lock" "gem") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "test-ruby")) :extensions ("test.rb" "test_helper.rb" "spec.rb" "spec_helper.rb") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "sbt")) :extensions ("sbt") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "scala" :v-adjust 0.05)) :extensions ("scala") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "scheme")) :extensions ("scm") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "swift" :v-adjust 0.05)) :extensions ("swift") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "elixir")) :extensions ("ex" "exs" "eex" "leex" "mix.lock") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "elm")) :extensions ("elm") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "erlang" :v-adjust 0.02)) :extensions ("erl" "hrl") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "eslint")) :extensions ("eslint" "eslintignore") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "cabal")) :extensions ("cabal" "cabal.project") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "clisp")) :extensions ("lisp") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "clojure-line" :v-adjust 0.02)) :extensions ("clj" "cljc") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "cljs")) :extensions ("cljs") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "coffeescript")) :extensions ("coffee" "iced") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "go")) :extensions ("go") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "graphql")) :extensions ("graphql" "gql") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "graphviz")) :extensions ("dot" "gv") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "elisp" :v-adjust -0.15)) :extensions ("el" "elc" "eln") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "julia")) :extensions ("jl") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jupyter")) :extensions ("ipynb") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "kotlin")) :extensions ("kt" "kts") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "hy")) :extensions ("hy") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "ocaml")) :extensions ("ml" "mli") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "org" :v-adjust 0.02)) :extensions ("org") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "php")) :extensions ("php") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "pony")) :extensions ("pony") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "powershell")) :extensions ("ps1") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "tcl")) :extensions ("tcl") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "terraform")) :extensions ("tf" "tfvars" "tfstate") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "terminal" :v-adjust 0.05)) :extensions ("sh" "zsh" "fish") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "nginx" :v-adjust 0.05)) :extensions ("nginx") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "nimrod")) :extensions ("nim" "nims" "nimble") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "nix")) :extensions ("nix") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "nvidia")) :extensions ("cu" "cuh") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "perl")) :extensions ("pl" "plx" "pm" "perl") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "perl6" :v-adjust 0.01)) :extensions ("pm6" "p6" "t6" "raku" "rakumod" "rakudoc" "rakutest") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "pug-alt")) :extensions ("pug") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "R")) :extensions ("r" "rdata" "rds" "rda") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "tex")) :extensions ("tex") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "rst")) :extensions ("rst") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "markdown" :v-adjust 0.05)) :extensions ("md" "markdown") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "file-pdf")) :extensions ("pdf") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "database" :v-adjust 0.02)) :extensions ("sql") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "tools")) :extensions ("dmg") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-material "style")) :extensions ("styles") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "lua")) :extensions ("lua") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "asciidoc")) :extensions ("adoc" "asciidoc") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "sbt")) :extensions ("sbt") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "puppet")) :extensions ("pp") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jinja")) :extensions ("j2" "jinja2") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "dockerfile")) :extensions ("dockerfile" "dockerignore") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "racket")) :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "reason" :v-adjust -0.15)) :extensions ("re" "rei") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "sass" :v-adjust 0.02)) :extensions ("scss" "sass") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "stylus")) :extensions ("styl") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "less")) :extensions ("less") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-material "style")) :extensions ("styles") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vagrant")) :extensions ("vagrantfile") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "verilog")) :extensions ("v" "vams" "sv" "sva" "svh" "svams") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vertex-shader")) :extensions ("glsl" "vert" "tesc" "tese" "geom" "frag" "comp") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "zig")) :extensions ("zig" "zir") :fallback 'same-as-icon)

        ;; Javascript related
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "babel")) :extensions ("babelrc") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "bower")) :extensions ("bowerrc" "bower.json") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "ejs")) :extensions ("ejs") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "gulp")) :extensions ("gulpfile") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "grunt")) :extensions ("gruntfile") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "javascript-badge" :v-adjust 0.05)) :extensions ("js" "es0" "es1" "es2" "es3" "es4" "es5" "es6" "es7" "es8" "es9") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jsx2-alt")) :extensions ("jsx") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "nodejs")) :extensions ("node") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "npm")) :extensions ("npmignore" "package.json" "package-lock.json") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "react")) :extensions ("react") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "typescript")) :extensions ("ts" "tsx") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vue")) :extensions ("vue") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "webpack")) :extensions ("webpack") :fallback 'same-as-icon)
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "yarn")) :extensions ("yarn.lock") :fallback 'same-as-icon)

        ;; Media files icon
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "paint-brush" :v-adjust 0.05))
         :extensions ("jpg" "jpeg" "png" "gif" "ico" "tif" "tiff" "svg" "bmp"
                      "psd" "ai" "eps" "indd" "webp")
         :fallback 'same-as-icon)

        ;; Font files
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "font" :v-adjust 0.05))
         :extensions ("ttf" "otf" "woff2")
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "film" :height 0.95 :v-adjust 0.05))
         :extensions ("mkv" "avi" "mov" "mp4" "webm")
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "headphones" :v-adjust 0.05))
         :extensions ("mp3" "flac" "opus" "au" "aac" "ogg" "wav" "m4a" "midi")
         :fallback 'same-as-icon)


        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "list" :v-adjust 0.02))
         :extensions ("log" "zsh_history" "bash_history")
         :fallback 'same-as-icon)

        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "checklist" :v-adjust 0.05))
         :extensions ("TODO")
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "book"
                       :height 1.1
                       :v-adjust 0.0))
         :extensions ("rst" "txt" "contribute" "license" "readme" "changelog")
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "cogs" :v-adjust 0.05))
         :extensions ("conf" "cfg" "yaml" "yml" "json" "xml" "toml" "cson" "ini" "iml" "dll" "DS_STORE"
                      ;; *nix related stuff
                      "xprofile" "dircolors" "Xresources" "config")
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "package" :v-adjust 0.05))
         :extensions ("deb" "pkg" "rpm")
         :fallback 'same-as-icon)

        ;; Tag files
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-alltheicon "script" :v-adjust 0.05))
         :extensions ("bashrc" "bash_profile" "profile" "zshrc" "zshenv" "zpofile" "zlogin" "zlogout")
         :fallback 'same-as-icon)

        ;; Tag files
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-fileicon "tag" :v-adjust 0.02))
         :extensions ("tags" "TAGS")
         :fallback 'same-as-icon)

        ;; TODO use it for binary files?
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "code" :v-adjust 0.05))
         :extensions ("a.out" "tpl" "erb" "twig" "ejs" "mk")
         :fallback 'same-as-icon)

        ;; Keys
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "key" :v-adjust 0.05))
         :extensions ("key" "pem" "p12" "crt" "pub" "gpg")
         :fallback 'same-as-icon)

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "server" :v-adjust 0.0))
         :extensions ("zip" "xz" "tar" "gz" "7z" "rar")
         :fallback 'same-as-icon)
        ))

    (treemacs-load-theme "kaolin")))


(provide 'kaolin-themes-treemacs)
;;; kaolin-themes-treemacs.el ends here
