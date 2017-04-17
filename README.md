# Kaolin theme
[![MELPA](https://melpa.org/packages/kaolin-theme-badge.svg)](https://melpa.org/#/kaolin-theme)

A dark jade Emacs theme inspired by [Sierra.vim](https://github.com/AlessandroYorba/Sierra)

# Screenshots
![kaolin-elisp](https://raw.githubusercontent.com/0rdy/kaolin-theme/master/screenshots/kaolin-elisp.png)
![kaolin-modes](https://raw.githubusercontent.com/0rdy/kaolin-theme/master/screenshots/kaolin-modes.png)
![kaolin-company](https://raw.githubusercontent.com/0rdy/kaolin-theme/master/screenshots/kaolin-company.png)

# Installation
## MELPA
To install the theme via package.el: `M-x package-install RET kaolin-theme RET`
## Manually
Copy the `kaolin-theme.el` file to your `~/.emacs.d/themes` directory and add the following to Emacs config:
```emacs-lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'kaolin t)
```
# Advanced highliginting

* [highlight-numbers](https://github.com/Fanael/highlight-numbers) — Highlight numbers in source code.
* [highlight-quoted](https://github.com/Fanael/highlight-quoted) — highlight Lisp quotes and quoted symbols
* [rainbow-delimiters](https://github.com/Fanael/rainbow-delimiters) — mode which highlights delimiters such as parentheses, brackets or braces according to their depth.
* [highlight-defined](https://github.com/Fanael/highlight-defined) — highlight known Emacs Lisp symbols
