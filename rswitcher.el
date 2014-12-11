;;; rswitcher.el --- Structure and functions for switching elements  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2012-2013 Damien Cassou
;;
;; Author: Damien Cassou <damien.cassou@gmail.com>
;; GIT: https://github.com/DamienCassou/shell-switcher
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


(require 'names)

(define-namespace rswitcher-

(defun make ()
  "Return a new switcher.
The switcher is internally implemented as a cons. The `car' of
this cons contains the list of elements in the rswitcher (see
`rswitcher--elements') while its `cdr' contains the index of the
most-recent element (see `rswitcher--last-pos')."
  (cons nil nil))

(defun -elements (switcher)
  "Return the list of elements in SWITCHER.
See `rswitcher-length', `rswitcher-empty-p', `rswitcher-memq',
`rswitcher-add', and `rswitcher-delete-most-recent' for ways to query
and manipulate SWITCHER."
  (car switcher))

(defun -last-pos (switcher)
  "Return the index of SWITCHER's most-recent element.
A value of 0 means the first element of SWITCHER's elements. This
function returns nil in all cases except after a
`rswitcher-switch-partial' (and the function
`rswitcher--reset-last-pos' will make this function return nil
again). See `rswitcher--most-recent-pos' and
`rswitcher--most-recent' for higher-level functions. Change the
value returned by this function by using
`rswitcher--reset-last-pos', `rswitcher--increment-last-pos', and
`rswitcher--set-last-pos'."
  (cdr switcher))

(defun -set-last-pos (switcher last-pos)
  "Set the last pos field of SWITCHER to be LAST-POS.

A value of 0 means the first element of SWITCHER's elements."
  (setcdr switcher last-pos))

(defun -increment-last-pos (switcher)
  "Add 1 (modulo SWITCHER's number of elements) to LAST-POS field."
  (let ((last-pos (-last-pos switcher)))
    (-set-last-pos switcher
                             (% (1+ last-pos)
                                (length switcher)))))

(defun -reset-last-pos (switcher)
  "Set last pos field of SWITCHER to nil.
See `rswitcher--last-pos'."
  (-set-last-pos switcher nil))

(defun length (switcher)
  "Return the number of elements in SWITCHER.
See `rswitcher--elements'."
  (::length (-elements switcher)))

(defun empty-p (switcher)
  "Check if there is no more elements in SWITCHER."
  (zerop (length switcher)))

(defun -most-recent-pos (switcher)
  "Return the index of the most recent element.
The most recent element is always the first of SWITCHER's
elements (in which case the function returns 0) except after a
call to `rswitcher-switch-partial' in which case the most recent
element is pointed to by the last pos field of SWITCHER."
  (or (-last-pos switcher) 0))

(defun most-recent (switcher)
  "Return the most recently accessed element in SWITCHER."
  (elt (-elements switcher) (-most-recent-pos switcher)))

(defun -push (switcher elt)
  "Update SWITCHER's elements by adding ELT in front."
  (unless (member elt (-elements switcher))
    (setcar switcher (cons elt (-elements switcher)))))

(defun make-most-recent-elt-the-first (switcher)
  "Move most recent element to the beginning of SWITCHER's elements."
  (unless (zerop (-most-recent-pos switcher))
    (-push
     switcher
     (-delete switcher (-most-recent-pos switcher)))))

(defun add (switcher elt)
  "Reorganize SWITCHER with most-recent element on front and push ELT."
  (make-most-recent-elt-the-first switcher)
  (-push switcher elt)
  (-reset-last-pos switcher))

(defun -pop (switcher)
  "Remove and return the first element of SWITCHER's elements."
  (let ((elements (-elements switcher)))
    (prog1
        (car elements)
      (setcar switcher (cdr elements)))))

(defun memq (switcher elt)
  "Check if SWITCHER's elements include ELT. Comparison done with `eq'."
  (::memq elt (-elements switcher)))

(defun -delete (switcher pos)
  "Delete and return the element in SWITCHER at position POS.

This function only accepts valid values for POS between 0 and the
number of SWITCHER's elements minus 1."
  (prog1
      ;; return the deleted element
      (elt (-elements switcher) pos)
    (if (zerop pos)
	(-pop switcher)
      (let ((tail (nthcdr (1+ pos) (-elements switcher)))
	    (head (nthcdr (1- pos) (-elements switcher))))
	(setcdr head tail)))
    (if (= pos (-most-recent-pos switcher))
	;; we just removed the element that was most-recent
	(-reset-last-pos switcher))))

(defun delete-all (switcher)
  "Remove all elements from SWITCHER."
  (while (not (empty-p switcher))
    (delete-most-recent switcher)))

(defun delete-most-recent (switcher)
  "Remove the most recent element from SWITCHER."
  (-delete switcher (-most-recent-pos switcher)))

(defun -swap-first-two-elts (switcher)
  "Reorganize SWITCHER by swapping first and second elements."
  (let ((first (-pop switcher))
	(new (-pop switcher)))
    (-push switcher first)
    (-push switcher new)))

(defun switch-full (switcher)
  "Select the next most recent element in SWITCHER.
This function is similar to pressing and releasing ALT+TAB in
standard window managers. Repeatedly calling this function will
always select the two most recent elements alternatively."
  (when (>= (length switcher) 2)
    (make-most-recent-elt-the-first switcher)
    (-swap-first-two-elts switcher)
    (-reset-last-pos switcher)))

(defun switch-partial (switcher)
  "Continue switching after 1 full switch and many partial switches.
This function is similar to pressing TAB after pressing ALT+TAB.
Repeatedly calling this function will alternatively select all
elements of SWITCHER, most recent elements first."
  (when (>= (length switcher) 2)
    (if (-last-pos switcher)
	(-increment-last-pos switcher)
      ;; Reverse swap done in previous execution of
      ;; `switch-full'
      (-swap-first-two-elts switcher)
      (-set-last-pos switcher
                     (if (> (length switcher) 2) 2 0))))))

(provide 'rswitcher)

;;; rswitcher.el ends here
