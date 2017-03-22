[![MELPA](https://melpa.org/packages/kaolin-theme-badge.svg)](https://melpa.org/#/kaolin-theme)

# Kaolin theme
A dark jade Emacs theme inspired by [Sierra.vim](https://github.com/AlessandroYorba/Sierra)

![kaolin-logo](https://raw.githubusercontent.com/0rdy/kaolin-theme/master/screenshots/kaolin-theme.png)

# Installation
## MELPA
To install the theme via package.el: `M-x package-install RET kaolin-theme RET`
## Manually
Copy the `kaolin-theme.el` file to your `~/.emacs.d/themes` directory and add the following to Emacs config:
```emacs-lisp
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'kaolin t)
```
