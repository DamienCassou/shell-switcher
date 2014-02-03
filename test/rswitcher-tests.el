;;; shell-switcher-tests.el --- Tests for shell-switcher.el

;; Copyright (C) 2013 Damien Cassou

;; Author: Damien Cassou <damien.cassou@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for rswitcher.el

;;; Code:

(require 'rswitcher)
(require 'ert)

(ert-deftest rswitcher-test-add-increment-length ()
  "rswitcher-add increases length"
  (let ((s (rswitcher-make)))
    (should (equal 0 (rswitcher-length s)))
    (rswitcher-add s 'c)
    (should (equal 1 (rswitcher-length s)))
    (rswitcher-add s 'b)
    (should (equal 2 (rswitcher-length s)))))

(ert-deftest rswitcher-test-add-no-duplicate ()
  "rswitcher-add does not add duplicates"
  (let ((s (rswitcher-make)))
    (should (equal 0 (rswitcher-length s)))
    (rswitcher-add s 'c)
    (should (equal 1 (rswitcher-length s)))
    (rswitcher-add s 'c)
    (should (equal 1 (rswitcher-length s)))))

(ert-deftest rswitcher-test-delete-all ()
  "rswitcher-delete-all removes all correctly"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'c)
    (rswitcher-add s 'b)
    (rswitcher-add s 'a)
    (should (not (rswitcher-empty-p s)))
    (rswitcher-delete-all s)
    (should (rswitcher-empty-p s))))

(ert-deftest rswitcher-test-delete-head ()
  "rswitcher--delete removes head correctly"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'c)
    (rswitcher-add s 'b)
    (rswitcher-add s 'a)
    (should (equal '(a b c) (rswitcher--elements s)))
    (should (equal 'a (rswitcher-most-recent s)))
    (should (equal 'a (rswitcher--delete s 0)))
    (should (equal '(b c) (rswitcher--elements s)))
    (should (equal 'b (rswitcher-most-recent s))))
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'a)
    (should (equal '(a) (rswitcher--elements s)))
    (should (equal 'a (rswitcher--delete s 0)))
    (should (equal '() (rswitcher--elements s)))
    (should (equal nil (rswitcher-most-recent s)))))

(ert-deftest rswitcher-test-delete-last ()
  "rswitcher--delete removes last correctly"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'c)
    (rswitcher-add s 'b)
    (rswitcher-add s 'a)
    (should (equal '(a b c) (rswitcher--elements s)))
    (should (equal 'a (rswitcher-most-recent s)))
    (should (equal 'c (rswitcher--delete s 2)))
    (should (equal 'a (rswitcher-most-recent s)))
    (should (equal '(a b) (rswitcher--elements s)))))

(ert-deftest rswitcher-test-delete-middle ()
  "rswitcher--delete removes middle correctly"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'c)
    (rswitcher-add s 'b)
    (rswitcher-add s 'a)
    (should (equal '(a b c) (rswitcher--elements s)))
    (should (equal 'a (rswitcher-most-recent s)))
    (should (equal 'b (rswitcher--delete s 1)))
    (should (equal 'a (rswitcher-most-recent s)))
    (should (equal '(a c) (rswitcher--elements s)))))

(ert-deftest rswitcher-test-delete-after-switch ()
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'c)
    (rswitcher-add s 'b)
    (rswitcher-add s 'a)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (rswitcher-switch-partial s)
    (should (equal 'c (rswitcher-most-recent s)))
    (should (equal '(a b c) (rswitcher--elements s)))
    (rswitcher--delete s 2)
    (should (equal '(a b) (rswitcher--elements s)))
    (should (equal 'a (rswitcher-most-recent s)))))

(ert-deftest rswitcher-test-add-make-most-recent ()
  "rswitcher-add makes new element most-recent"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'a)
    (should (equal (rswitcher-most-recent s) 'a))
    (rswitcher-add s 'b)
    (should (equal (rswitcher-most-recent s) 'b))))

(ert-deftest rswitcher-test-switch-full ()
  "simple scenario for switch-full"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'a)
    (rswitcher-add s 'b)
    (should (equal (rswitcher-most-recent s) 'b))
    (rswitcher-switch-full s)
    (should (equal (rswitcher-most-recent s) 'a))
    (rswitcher-switch-full s)
    (should (equal (rswitcher-most-recent s) 'b))
    (rswitcher-add s 'c)
    (should (equal (rswitcher-most-recent s) 'c))
    (rswitcher-switch-full s)
    (should (equal (rswitcher-most-recent s) 'b))
    (rswitcher-switch-full s)
    (should (equal (rswitcher-most-recent s) 'c))
    (rswitcher-switch-full s)
    (should (equal (rswitcher-most-recent s) 'b))))

(ert-deftest rswitcher-test-switch-partial ()
  "simple scenario for switch-partial"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'a)
    (rswitcher-add s 'b)
    (should (equal (rswitcher-most-recent s) 'b))
    (rswitcher-switch-full s)
    (should (equal (rswitcher-most-recent s) 'a))
    (rswitcher-switch-partial s)
    (should (equal (rswitcher-most-recent s) 'b))
    (rswitcher-switch-partial s)
    (should (equal (rswitcher-most-recent s) 'a))))

(ert-deftest rswitcher-test-mixing-partial-and-full-with-2-elts ()
  "mixing use of partial and full"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'a)
    (rswitcher-add s 'b)
    (should (equal 'b (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'b (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'b (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'b (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'a (rswitcher-most-recent s)))))

(ert-deftest rswitcher-test-mixing-partial-and-full ()
  "mixing use of partial and full"
  (let ((s (rswitcher-make)))
    (rswitcher-add s 'a)
    (rswitcher-add s 'b)
    (rswitcher-add s 'c)
    (should (equal 'c (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'b (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'c (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'b (rswitcher-most-recent s)))
    (rswitcher-switch-partial s)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'c (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'a (rswitcher-most-recent s)))
    (rswitcher-switch-full s)
    (should (equal 'c (rswitcher-most-recent s)))))
