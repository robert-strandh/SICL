(cl:in-package #:sicl-sequence-test)

(defun relevant-elements-1 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 0 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1 c 2)))))

(defun relevant-elements-2 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 0 2 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0 b 1)))))

(defun relevant-elements-3 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 0 1 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(a 0)))))

(defun relevant-elements-4 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 0 0 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-elements-5 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 1 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(b 1 c 2)))))

(defun relevant-elements-6 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 2 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '(c 2)))))

(defun relevant-elements-7 ()
  (let ((v1 #(a b c))
	(result '()))
    (sicl-sequence::for-each-relevant-element
	(element index v1 3 3 nil)
      (push element result)
      (push index result))
    (assert (equal (reverse result) '()))))

(defun relevant-elements ()
  (relevant-elements-1)
  (relevant-elements-2)
  (relevant-elements-3)
  (relevant-elements-4)
  (relevant-elements-5)
  (relevant-elements-6)
  (relevant-elements-7))
