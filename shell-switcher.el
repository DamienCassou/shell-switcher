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
;;; Description:
;;
;; After you add the following line to your emacs init file you will
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
;; (global-set-key (kbd "C-'")   'shell-switcher-switch-buffer)
;; (global-set-key (kbd "C-M-'") 'shell-switcher-new-shell)
;;
;; After pressing one of these keys, you can subsequently press the
;; last character of the key (a quote in this case) to continue
;; switching. For example, with the previous bindings, you can type
;; "C-' ' '" to switch to the 3rd shell buffer. Like ALT+tab in
;; standard window managers, the list of shell buffers is arranged so
;; that recently accessed ones are at the beginning of the list.
;;
;;; Code:
;;
;; The following code uses the "sswitcher" prefix for all "private"
;; functions (and an addition dash "-") and the "shell-switcher" prefix
;; for all "public" functions.

(require 'rswitcher)

(defconst sswitcher-ring (rswitcher-make))

(defgroup shell-switcher nil
  "Handling multiple shells")

(defcustom shell-switcher-new-shell-function 'shell-switcher-make-eshell
  "This variable references a function used to create new shells.

The function must take 0 arguments and return a newly created
shell buffer. `shell-switcher-make-shell' and
`shell-switcher-make-eshell' are possible functions."

  :group 'shell-switcher)

(defun shell-switcher-make-shell ()
  "Ensure the creation of a new `shell'.

This function is to be used as value for `shell-switcher-new-shell-function'."
  (shell (generate-new-buffer-name "*shell*")))

(defun shell-switcher-make-eshell ()
  "Ensure the creation of a new `eshell'.

This function is to be used as value for `shell-switcher-new-shell-function'."
  (eshell t))

(defun sswitcher--current ()
  (rswitcher-current sswitcher-ring))

(defun sswitcher--current-shell-valid-p ()
  ;; we could potentially add the fact that the shell is still
  ;; accepting commands, but I don't know how to that for all possible
  ;; shells. And I'm not sure it's a good idea either :-)
  (buffer-live-p (sswitcher--current)))

(defun sswitcher--clean-buffers ()
  "Remove all shell buffers until we find a valid one."
  (while (and (sswitcher--shell-exist-p)
	      (not (sswitcher--current-shell-valid-p)))
    (rswitcher-delete-current sswitcher-ring)))

(defun sswitcher--shell-exist-p ()
  (not (rswitcher-empty-p sswitcher-ring)))

(defun sswitcher--in-shell-buffer-p ()
  (rswitcher-memq sswitcher-ring (current-buffer)))

(defun sswitcher--new-shell ()
  (rswitcher-add sswitcher-ring (funcall shell-switcher-new-shell-function))
  (sswitcher--display-shell-buffer))

(defun sswitcher--no-more-shell-buffers ()
  (if (y-or-n-p "No more buffers, create new one?? ")
      (sswitcher--new-shell)))

(defun sswitcher--prepare-for-fast-key ()
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

(defun sswitcher--display-shell-buffer ()
  (sswitcher--clean-buffers)
  (if (sswitcher--shell-exist-p)
      (switch-to-buffer (sswitcher--current))
    (message "No shell buffer to display")))

(defun shell-switcher-switch-buffer ()
  (interactive)
  (sswitcher--clean-buffers)
  (if (or (not (sswitcher--shell-exist-p))
	  (and (= (rswitcher-length sswitcher-ring) 1)
	       (sswitcher--in-shell-buffer-p)))
      (sswitcher--no-more-shell-buffers)
    (when (sswitcher--in-shell-buffer-p)
      (rswitcher-switch-full sswitcher-ring))
    (sswitcher--display-shell-buffer)
    (sswitcher--prepare-for-fast-key)))

(defun sswitcher-switch-partially ()
  (sswitcher--clean-buffers)
  (if (< (rswitcher-length sswitcher-ring) 2)
      (sswitcher--no-more-shell-buffers)
    (rswitcher-switch-partial sswitcher-ring)
    (sswitcher--display-shell-buffer)))

(defun shell-switcher-new-shell ()
  (interactive)
  (sswitcher--new-shell))

(defun shell-switcher-switch-or-new-shell ()
  (interactive)
  (sswitcher--clean-buffers)
  (if (and (sswitcher--shell-exist-p)
	   (not (sswitcher--in-shell-buffer-p)))
      (sswitcher--display-shell-buffer)
    (sswitcher--new-shell))
  (sswitcher--prepare-for-fast-key))

(provide 'shell-switcher)
