;;; rswitcher.el --- Structure and functions for switching elements
;;
;; Copyright (C) 2012 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; GIT: https://github.com/DamienCassou/shell-switcher
;; Version: 0.5
;; Created: 2012-06-27
;; Keywords: emacs package elisp switcher
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
;; Install
;;     Please see the README.md file from the same distribution
;;
;;; Commentary:
;;
;; This code provides a data structure and functions to mimic ALT+TAB
;; behavior of window managers within Emacs and with Emacs feeling.
;; `rswitcher-switch-full' is similar to the initial ALT+TAB while
;; `rswitcher-switch-partial' is similar to subsequent TABs.
;;
;;; Code:

(eval-when-compile (require 'cl))

(defun rswitcher-make ()
  (cons nil nil))

(defun rswitcher--elements (switcher)
  (car switcher))

(defun rswitcher--last-pos (switcher)
  (cdr switcher))

(defun rswitcher--set-last-pos (switcher last-pos)
  (setcdr switcher last-pos))

(defun rswitcher--increment-last-pos (switcher)
  (let ((last-pos (rswitcher--last-pos switcher)))
    (rswitcher--set-last-pos switcher
				 (% (1+ last-pos)
				    (rswitcher-length switcher)))))

(defun rswitcher--reset-last-pos (switcher)
  (rswitcher--set-last-pos switcher nil))

(defun rswitcher-length (switcher)
  (length (rswitcher--elements switcher)))

(defun rswitcher-empty-p (switcher)
  (zerop (rswitcher-length switcher)))

(defun rswitcher--current-pos (switcher)
  (or (rswitcher--last-pos switcher) 0))

(defun rswitcher-current (switcher)
  (elt (rswitcher--elements switcher) (rswitcher--current-pos switcher)))

(defun rswitcher--push (switcher elt)
  (setcar switcher (cons elt (car switcher))))

(defun rswitcher--pop (switcher)
  (prog1
      (caar switcher)
    (setcar switcher (cdar switcher))))

(defun rswitcher-memq (switcher elt)
  (memq elt (rswitcher--elements switcher)))

(defun rswitcher--delete (switcher pos)
  (assert (and (>= pos 0) (< pos (rswitcher-length switcher))))
  (prog1
      ;; return the deleted element
      (elt (rswitcher--elements switcher) pos)
    (if (zerop pos)
	(rswitcher--pop switcher)
      (let ((tail (nthcdr (1+ pos) (rswitcher--elements switcher)))
	    (head (nthcdr (1- pos) (rswitcher--elements switcher))))
	(setcdr head tail)))
    (if (= pos (rswitcher--current-pos switcher))
	;; we just removed the element that was current
	(rswitcher--reset-last-pos switcher))))

(defun rswitcher-delete-current (switcher)
  (rswitcher--delete switcher (rswitcher--current-pos switcher)))

(defun rswitcher-make-current-elt-the-first (switcher)
  ;; put current element to the beginning of the list
  (unless (zerop (rswitcher--current-pos switcher))
    (rswitcher--push
     switcher
     (rswitcher--delete switcher (rswitcher--current-pos switcher)))))

(defun rswitcher-add (switcher elt)
  (rswitcher-make-current-elt-the-first switcher)
  (rswitcher--push switcher elt)
  (rswitcher--reset-last-pos switcher))

(defun rswitcher--swap-first-two-elts (switcher)
  (let ((current (rswitcher--pop switcher))
	(new (rswitcher--pop switcher)))
    (rswitcher--push switcher current)
    (rswitcher--push switcher new)))

(defun rswitcher-switch-full (switcher)
  (when (>= (rswitcher-length switcher) 2)
    (rswitcher-make-current-elt-the-first switcher)
    (rswitcher--swap-first-two-elts switcher)
    (rswitcher--reset-last-pos switcher)))

(defun rswitcher-switch-partial (switcher)
  (when (>= (rswitcher-length switcher) 2)
    (if (rswitcher--last-pos switcher)
	(rswitcher--increment-last-pos switcher)
      ;; Reverse swap done in previous execution of rswitcher-switch-full
      (rswitcher--swap-first-two-elts switcher)
      (rswitcher--set-last-pos switcher
	    (if (> (rswitcher-length switcher) 2) 2 0)))))

(provide 'rswitcher)
