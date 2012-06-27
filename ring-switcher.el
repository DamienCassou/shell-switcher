(defvar multi-shell-shells (make-ring 100))

(defstruct ring-switcher-
  last-pos
  elements)

(defun ring-switcher-make ()
  (make-ring-switcher-))

(defun ring-switcher-reset-last-pos (switcher)
  (setf (ring-switcher--last-pos switcher) nil))

(defun ring-switcher-length (switcher)
  (length (ring-switcher--elements switcher)))

(defun ring-switcher--push (switcher elt)
  (push elt (ring-switcher--elements switcher)))

(defun ring-switcher--pop (switcher)
  (pop (ring-switcher--elements switcher)))

(defun ring-switcher--delete (switcher pos)
  (assert (and (>= pos 0) (< pos (ring-switcher-length switcher))))
  (let ((elt (elt (ring-switcher--elements switcher) pos)))
    (if (zerop pos)
	(setf (ring-switcher--elements switcher) (cdr (ring-switcher--elements switcher)))
      (let ((tail (nthcdr (1+ pos) (ring-switcher--elements switcher)))
	    (head (nthcdr (1- pos) (ring-switcher--elements switcher))))
	(setcdr head tail)))
    ;; return the element we deleted
    elt))

(defun ring-switcher-current-pos (switcher)
  (or (ring-switcher--last-pos switcher) 0))

(defun ring-switcher-current (switcher)
  (elt (ring-switcher--elements switcher) (ring-switcher-current-pos switcher)))

(defun ring-switcher-make-current-elt-the-first (switcher)
  ;; put current element to the beginning of the list
  (unless (zerop (ring-switcher-current-pos switcher))
    (ring-switcher--push switcher (ring-switcher--delete switcher (ring-switcher-current-pos switcher)))))

(defun ring-switcher-add (switcher elt)
  (ring-switcher-make-current-elt-the-first switcher)
  (ring-switcher--push switcher elt)
  (ring-switcher-reset-last-pos switcher))

(defun ring-switcher-swap-first-two-elts (switcher)
  (let ((current (ring-switcher--pop switcher))
	(new (ring-switcher--pop switcher)))
    (ring-switcher--push switcher current)
    (ring-switcher--push switcher new)))

(defun ring-switcher-switch-full (switcher)
  (when (>= (ring-switcher-length switcher) 2)
    (ring-switcher-make-current-elt-the-first switcher)
    (ring-switcher-swap-first-two-elts switcher)
    (ring-switcher-reset-last-pos switcher)))

(defun ring-switcher--increment-last-pos (switcher)
  (incf (ring-switcher--last-pos switcher))
  (setf (ring-switcher--last-pos switcher) (% (ring-switcher--last-pos switcher) (ring-switcher-length switcher))))

(defun ring-switcher-switch-partial (switcher)
  (when (>= (ring-switcher-length switcher) 2)
    (if (ring-switcher--last-pos switcher)
	(ring-switcher--increment-last-pos switcher)
      ;; Reverse swap done in previous execution of ring-switcher-switch-full
      (ring-switcher-swap-first-two-elts switcher)
      (setf (ring-switcher--last-pos switcher)
	    (if (> (ring-switcher-length switcher) 2) 2 0)))))

(require 'ert)

(ert-deftest ring-switcher-test-delete-head ()
  "ring-switcher--delete removes head correctly"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'c)
    (ring-switcher-add s 'b)
    (ring-switcher-add s 'a)
    (should (equal '(a b c) (ring-switcher--elements s)))
    (should (equal 'a (ring-switcher--delete s 0)))
    (should (equal '(b c) (ring-switcher--elements s))))
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'a)
    (should (equal '(a) (ring-switcher--elements s)))
    (should (equal 'a (ring-switcher--delete s 0)))
    (should (equal '() (ring-switcher--elements s)))))

(ert-deftest ring-switcher-test-delete-last ()
  "ring-switcher--delete removes last correctly"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'c)
    (ring-switcher-add s 'b)
    (ring-switcher-add s 'a)
    (should (equal '(a b c) (ring-switcher--elements s)))
    (should (equal 'c (ring-switcher--delete s 2)))
    (should (equal '(a b) (ring-switcher--elements s)))))

(ert-deftest ring-switcher-test-delete-middle ()
  "ring-switcher--delete removes middle correctly"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'c)
    (ring-switcher-add s 'b)
    (ring-switcher-add s 'a)
    (should (equal '(a b c) (ring-switcher--elements s)))
    (should (equal 'b (ring-switcher--delete s 1)))
    (should (equal '(a c) (ring-switcher--elements s)))))

(ert-deftest ring-switcher-test-add-make-current ()
  "ring-switcher-add makes new element current"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'a)
    (should (equal (ring-switcher-current s) 'a))
    (ring-switcher-add s 'b)
    (should (equal (ring-switcher-current s) 'b))))

(ert-deftest ring-switcher-test-switch-full ()
  "simple scenario for switch-full"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'a)
    (ring-switcher-add s 'b)
    (should (equal (ring-switcher-current s) 'b))
    (ring-switcher-switch-full s)
    (should (equal (ring-switcher-current s) 'a))
    (ring-switcher-switch-full s)
    (should (equal (ring-switcher-current s) 'b))
    (ring-switcher-add s 'c)
    (should (equal (ring-switcher-current s) 'c))
    (ring-switcher-switch-full s)
    (should (equal (ring-switcher-current s) 'b))
    (ring-switcher-switch-full s)
    (should (equal (ring-switcher-current s) 'c))
    (ring-switcher-switch-full s)
    (should (equal (ring-switcher-current s) 'b))))

(ert-deftest ring-switcher-test-switch-partial ()
  "simple scenario for switch-partial"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'a)
    (ring-switcher-add s 'b)
    (should (equal (ring-switcher-current s) 'b))
    (ring-switcher-switch-full s)
    (should (equal (ring-switcher-current s) 'a))
    (ring-switcher-switch-partial s)
    (should (equal (ring-switcher-current s) 'b))
    (ring-switcher-switch-partial s)
    (should (equal (ring-switcher-current s) 'a))))

(ert-deftest ring-switcher-test-mixing-partial-and-full-with-2-elts ()
  "mixing use of partial and full"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'a)
    (ring-switcher-add s 'b)
    (should (equal 'b (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'a (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'b (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'a (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'b (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'a (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'b (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'a (ring-switcher-current s)))))

(ert-deftest ring-switcher-test-mixing-partial-and-full ()
  "mixing use of partial and full"
  (let ((s (ring-switcher-make)))
    (ring-switcher-add s 'a)
    (ring-switcher-add s 'b)
    (ring-switcher-add s 'c)
    (should (equal 'c (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'b (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'a (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'c (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'b (ring-switcher-current s)))
    (ring-switcher-switch-partial s)
    (should (equal 'a (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'c (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'a (ring-switcher-current s)))
    (ring-switcher-switch-full s)
    (should (equal 'c (ring-switcher-current s)))))
