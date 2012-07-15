;;; shell-switcher.el --- Provide fast switching between shell buffers
;;
;; Copyright (C) 2012 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; GIT: https://github.com/DamienCassou/shell-switcher
;; Version: 0.5
;; Created: 2012-06-27
;; Keywords: emacs package elisp shell eshell term switcher
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;;
;;; commentary:
;;
;; After you add the following line to your Emacs init file you will
;; switch easily between shell buffers:
;;
;; (require 'shell-switcher)
;;
;; You have to customize `shell-switcher-new-shell-function' if
;; `eshell' is not your shell of choice.
;;
;; To easily switch between shell buffers, you can bind keys as
;; follows:
;;
;; (global-set-key (kbd "C-'")     'shell-switcher-switch-buffer)
;; (global-set-key (kbd "C-x 4 '") 'shell-switcher-switch-buffer-other-window)
;; (global-set-key (kbd "C-M-'")   'shell-switcher-new-shell)
;;
;; After pressing one of these keys, you can subsequently press the
;; last character of the key (a quote in this case) to continue
;; switching. For example, with the previous bindings, you can type
;; "C-' ' '" to switch to the 3rd shell buffer. Like ALT+tab in
;; standard window managers, the list of shell buffers is arranged so
;; that recently accessed ones are at the beginning of the list.
;;
;;; code:
;;
;; The following code uses the "sswitcher" prefix for all "private"
;; functions (and an addition dash "-") and the "shell-switcher" prefix
;; for all "public" functions.

(require 'rswitcher)

(defconst sswitcher-ring (rswitcher-make))

(defgroup shell-switcher nil
  "Handling multiple shells"
  :group 'extensions
  :group 'convenience)

;;;###autoload
(defcustom shell-switcher-new-shell-function 'shell-switcher-make-eshell
  "This variable references a function used to create new shells.
The function must take 0 arguments and return a newly created
shell buffer. `shell-switcher-make-shell' and
`shell-switcher-make-eshell' are possible functions."

  :group 'shell-switcher)

(defvar shell-switcher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-'") 'shell-switcher-switch-buffer)
    (define-key map (kbd "C-x 4 '") 'shell-switcher-switch-buffer-other-window)
    (define-key map (kbd "C-M-'") 'shell-switcher-new-shell)
    map)
 "Keymap to use in shell-switcher mode.")

(define-minor-mode shell-switcher-mode
  "Toggle shell-switcher mode.
Interactively with no argument, this command toggles the mode. A
positive prefix argument enables the mode, any other prefix
argument disables it. From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

When shell-switcher mode is enabled, switching and creating shell
buffers is just a matter of calling
\\[shell-switcher-switch-buffer]. Then, you can type the last key
of this key binding again to continue switching. Additionally,
see commands \\[shell-switcher-switch-buffer-other-window] and
\\[shell-switcher-new-shell]."
  :group 'shell-switcher
  :global t
  ;; disabled by default:
  :init-value nil
  ;; nothing in the mode line:
  :lighter nil
  :keymap shell-switcher-mode-map)

(defun shell-switcher-make-shell ()
  "Ensure the creation of a new `shell'.
This function is to be used as value for
`shell-switcher-new-shell-function'."
  (shell (generate-new-buffer-name "*shell*")))

(defun shell-switcher-make-eshell ()
  "Ensure the creation of a new `eshell'.
This function is to be used as value for
`shell-switcher-new-shell-function'."
  (eshell t))

(defun sswitcher--most-recent ()
  "Return the most recently accessed shell."
  (rswitcher-most-recent sswitcher-ring))

(defun sswitcher--most-recent-shell-valid-p ()
  "Check that the most recently created shell can still be accessed."
  (buffer-live-p (sswitcher--most-recent)))

(defun sswitcher--clean-buffers ()
  "Remove all shell buffers until we find a valid one."
  (while (and (not (rswitcher-empty-p sswitcher-ring))
	      (not (sswitcher--most-recent-shell-valid-p)))
    (rswitcher-delete-most-recent sswitcher-ring)))

(defun sswitcher--shell-exist-p ()
  "Check that there is at least one valid shell to switch to."
  (sswitcher--clean-buffers)
  (not (rswitcher-empty-p sswitcher-ring)))

(defun sswitcher--in-shell-buffer-p ()
  "Check that the current buffer is a shell buffer."
  (rswitcher-memq sswitcher-ring (current-buffer)))

(defun sswitcher--new-shell (&optional other-window)
  "Create and display a new shell.
If OTHER-WINDOW is nil (the default), the new shell buffer is
displayed in the current window. If OTHER-WINDOW is t, change
another window.

This function uses `shell-switcher-new-shell-function' to decide
what kind of shell to create."
  (save-window-excursion
    (rswitcher-add sswitcher-ring
		   (funcall shell-switcher-new-shell-function)))
  (sswitcher--display-shell-buffer other-window))

(defun sswitcher--no-more-shell-buffers (&optional other-window)
  "Propose to create a new shell as there is no more to switch to.
If user answers positively, a new shell buffer is created. If
OTHER-WINDOW is nil (the default), the shell buffer is displayed
in the current window. If OTHER-WINDOW is t, change another
window.
"
  (if (y-or-n-p "No active shell buffer, create new one? ")
      (sswitcher--new-shell other-window)))

(defun sswitcher--prepare-for-fast-key ()
  "Set a keymap so that one can switch buffers by pressing 1 key.
The key to be pressed to continue switching buffers is the last
key of the most recent key sequence. See
`shell-switcher-switch-buffer' for more information. When this
key is pressed, calls `sswitcher-switch-partially'."
  (let* ((repeat-key (event-basic-type last-input-event))
	 (repeat-key-str (format-kbd-macro (vector repeat-key)))
	 (message (format "Type %s again to continue switching"
			  (format-kbd-macro (vector repeat-key)))))
    (set-temporary-overlay-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector repeat-key)
	 `(lambda () (interactive)
	    (sswitcher-switch-partially)
	    (message ,message)))
       map) t)
    (message message)))

(defun sswitcher--display-shell-buffer (&optional other-window)
  "Display the most recently accessed shell buffer.
If OTHER-WINDOW is nil (the default), change the current window.
If OTHER-WINDOW is t, change another window."
  (if (sswitcher--shell-exist-p)
      (if other-window
	  (switch-to-buffer-other-window (sswitcher--most-recent) t)
	(switch-to-buffer (sswitcher--most-recent) t))
    (message "No shell buffer to display")))

(defun sswitcher-switch-buffer (&optional other-window)
  "Switch to the most recently accessed buffer.
Switch to the most recently accessed shell buffer that is not the
current one. Pressing the last key of the key sequence that call
this command will result in switching to the next shell buffer :
for example, if `C-'' is bound to this command, repeatedly
pressing `'' (quote) will let the user visit all shell
buffers (this is actually done by `sswitcher-switch-partially'.
If OTHER-WINDOW is nil (the default), the current window is used
to display the shell buffer. If OTHER-WINDOW is t, the buffer is
displayed in the other window.

If there is no shell buffer or if the only shell buffer is the
current buffer, propose the creation of a new shell buffer and
display it in the current window (if OTHER-WINDOW is nil, the
default) or the other window (if OTHER-WINDOW is t)."
  (if (or (not (sswitcher--shell-exist-p))
	  (and (= (rswitcher-length sswitcher-ring) 1)
	       (sswitcher--in-shell-buffer-p)))
      (sswitcher--no-more-shell-buffers other-window)
    (when (sswitcher--in-shell-buffer-p)
      (rswitcher-switch-full sswitcher-ring))
    (sswitcher--display-shell-buffer other-window)
    (sswitcher--prepare-for-fast-key)))

;;;###autoload
(defun shell-switcher-switch-buffer ()
  "Switch to the most recently accessed buffer.
Switch to the most recently accessed shell buffer that is not the
current one. Pressing the last key of the key sequence that call
this command will result in switching to the next shell buffer :
for example, if `C-'' is bound to this command, repeatedly
pressing `'' (quote) will let the user visit all shell
buffers (this is actually done by `sswitcher-switch-partially'.

If there is no shell buffer or if the only shell buffer is the
current buffer, propose the creation of a new shell buffer."
  (interactive)
  (sswitcher-switch-buffer))

(defun sswitcher-switch-partially ()
  "Switch to the next most recently accessed buffer.

This function is indirectly called by
`shell-switcher-switch-buffer' after pressingthe last key of the
most recent key sequence."
  (sswitcher--clean-buffers)
  (if (< (rswitcher-length sswitcher-ring) 2)
      (sswitcher--no-more-shell-buffers)
    (rswitcher-switch-partial sswitcher-ring)
    (sswitcher--display-shell-buffer)))

;;;###autoload
(defun shell-switcher-switch-buffer-other-window ()
  "Switch to the most recently accessed buffer in another window.
Same as `shell-switcher-switch-buffer' but change another
window."
  (interactive)
  (sswitcher-switch-buffer t))

;;;###autoload
(defun shell-switcher-new-shell ()
  "Unconditionaly create and display a new shell buffer."
  (interactive)
  (sswitcher--new-shell))

(provide 'shell-switcher)

;;; shell-switcher.el ends here
