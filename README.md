Emacs shell-switcher
==========

shell-switcher provides fast switching between shell buffers.

# Installation

Just download from
[github](https://github.com/DamienCassou/shell-switcher
"shell-switcher project page") and add to your `load-path`. Then, add
the following to your init file:

    (require 'shell-switcher)

You have to customize `shell-switcher-new-shell-function` if eshell
is not your shell of choice.

To easily switch between shell buffers, you can bind keys as follows:

    (global-set-key (kbd "C-'")     'shell-switcher-switch-buffer)
    (global-set-key (kbd "C-x 4 '") 'shell-switcher-switch-buffer-other-window)
    (global-set-key (kbd "C-M-'")   'shell-switcher-new-shell)


# Usage

Using the previously defined key bindings:

- `C-'` opens the first 2 buffers and switch between them
- Repeating `'` continues switching after an initial `C-'`
- `C-M-'` forces the creation of a new shell
- `C-x 4 '` is similar to `C-'` but within another window
