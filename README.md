Emacs shell-switcher
==========

[![Build Status](https://travis-ci.org/DamienCassou/shell-switcher.png?branch=master)](https://travis-ci.org/DamienCassou/shell-switcher)

shell-switcher provides fast switching between shell buffers.

<figure>
<a href="http://www.youtube.com/watch?feature=player_embedded&v=jNSrrQwcCr4" target="_blank"><img src="http://img.youtube.com/vi/jNSrrQwcCr4/0.jpg" 
alt="shell-switcher screencast" width="480" height="360" border="10" /></a>
<figcaption>A shell-switcher <a href="http://www.youtube.com/watch?feature=player_embedded&v=jNSrrQwcCr4">screencast</a></ficaption>
</figure>

# Installation

## With Emacs' packaging system

Make sure you have [marmalade repository](http://marmalade-repo.org/)
ready to use and type `M-x package-install shell-switcher`.

## Manually

Download shell-switcher from
[github](https://github.com/DamienCassou/shell-switcher
"shell-switcher project page") and add it to your `load-path`. Then,
add the following to your init file:

    (require 'shell-switcher)

# Configuration

To activate shell-switcher, you have to set the variable
`shell-switcher-mode` to t, either through `customize-variable` or
with:

   (setq shell-switcher-mode t)

You have to customize `shell-switcher-new-shell-function` if eshell is
not your shell of choice.

# Usage

The standard key bindings are:

- `C-'` opens the first 2 buffers (one after the other) and switch between them
- Repeating `'` continues switching after an initial `C-'`
- `C-M-'` forces the creation of a new shell
- `C-x 4 '` is similar to `C-'` but within another window

You can change them by adapting and adding the following to your
configuration file:

    (define-key shell-switcher-mode-map (kbd "C-'")
	            'shell-switcher-switch-buffer)
    (define-key shell-switcher-mode-map (kbd "C-x 4 '")
	            'shell-switcher-switch-buffer-other-window)
    (define-key shell-switcher-mode-map (kbd "C-M-'")
	            'shell-switcher-new-shell)

If you tend to create shells without using shell-switcher but still
would like to easily switch between them, write the following (to be
adapted if eshell is not your preferred shell):

    (add-hook 'eshell-mode-hook 'shell-switcher-manually-register-shell)
