# Kaolin themes for GNU Emacs
[![MELPA Stable](https://stable.melpa.org/packages/kaolin-themes-badge.svg)](https://stable.melpa.org/#/kaolin-themes)
[![MELPA](https://melpa.org/packages/kaolin-themes-badge.svg)](https://melpa.org/#/kaolin-themes)
[![Emacs](https://img.shields.io/badge/Emacs-25.1%2B-d24b83.svg)](https://www.gnu.org/software/emacs/)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-green.svg)](http://www.gnu.org/licenses/gpl-3.0)

Kaolin is a set of eye pleasing themes for GNU Emacs with support for a large number of modes and external packages.

![kaolin-logo](https://user-images.githubusercontent.com/9018005/31884317-5715a32c-b7f5-11e7-8dce-0416051f55ce.png)

# Kaolin theme variants

![kaolin-banner](https://user-images.githubusercontent.com/9018005/61442971-41a09980-a951-11e9-8631-f84be6933656.png)

All screenshots are available in the wiki on the following pages:

* [kaolin-dark](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-dark-theme) - a dark jade variant inspired by [Sierra.vim](https://github.com/AlessandroYorba/Sierra)
* [kaolin-light](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-light-theme) - light variant of the original kaolin-dark.
* [kaolin-aurora](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-aurora-theme) - Kaolin meets polar lights.
* [kaolin-bubblegum](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-bubblegum-theme) - Kaolin colorful theme with dark blue background.
* [kaolin-eclipse](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-eclipse-theme) - a dark purple variant
* [kaolin-galaxy](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-galaxy-theme) - bright theme based on one of the Sebastian Andaur arts.
* [kaolin-ocean](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-ocean-theme) - a dark blue variant.
* [kaolin-temple](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-temple-theme) - dark background with syntax highlighting focus on blue, green and pink shades
* [kaolin-valley-dark](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-valley-dark-theme) - colorful Kaolin theme with brown background.
* [kaolin-valley-light](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki/Kaolin-valley-light-theme) - light variant of kaolin-valley theme.

### Further themes

#### WIP

These themes might be changed in the future.

* **kaolin-blossom** - theme focus on orange and purple with dark background.
* **kaolin-breeze** - Light Kaolin theme with soft colors.
* **kaolin-mono-dark** - almost monochrome dark green Kaolin theme.
* **kaolin-mono-light** - light variant of monochrome theme.
* **kaolin-shiva** - Kaolin theme with autumn colors and melanzane background.

### Planned

TBD

###### p.s. Bold means that a theme was added to master.

# Installation
## MELPA
To install the Kaolin themes pack via package.el: `M-x package-install RET kaolin-themes RET`

## Quick start
```emacs-lisp
(require 'kaolin-themes)
(load-theme 'kaolin-dark t)
;; Apply treemacs customization for Kaolin themes, requires the all-the-icons package.
(kaolin-treemacs-theme)

;; Or if you have use-package installed
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-dark t)
  (kaolin-treemacs-theme))
```

# Configuration
Information about configuring Kaolin themes is available in [the project wiki](https://github.com/ogdenwebb/emacs-kaolin-themes/wiki#configuration-example).

# Advanced highlighting

* [highlight-numbers](https://github.com/Fanael/highlight-numbers) — highlight numbers in source code.
* [highlight-quoted](https://github.com/Fanael/highlight-quoted) — highlight Lisp quotes and quoted symbols.
* [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) — mode which highlights delimiters such as parentheses, brackets or braces according to their depth.
* [highlight-defined](https://github.com/Fanael/highlight-defined) — highlight known Emacs Lisp symbols.

# Mode-line

The mode-line config isn't a part of the Kaolin theme, you can find my telephone-line config [here](https://github.com/ogdenwebb/elmax/tree/master/modeline).
