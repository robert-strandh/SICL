;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 21 00:11:24 2002
;;;; Contains: Tests for SORT

(in-package #:sicl-sequence-test)

(deftest sort-list.1
  (let ((a (list 1 4 2 5 3)))
    (sort a #'<))
  (1 2 3 4 5))

(deftest sort-list.2
  (let ((a (list 1 4 2 5 3)))
    (sort a #'< :key #'-))
  (5 4 3 2 1))

(deftest sort-list.3
  (let ((a (list 1 4 2 5 3)))
    (sort a #'(lambda (x y) nil))
    (sort a #'<))
  (1 2 3 4 5))

;;;
;;; Confirm that sort only permutes the sequence, even when given
;;; a comparison function that does not define a total order.
;;;
(deftest sort-list.4
  (loop
   repeat 100
   always
   (let ((a (list 1 2 3 4 5 6 7 8 9 0))
         (cmp (make-array '(10 10))))
     (loop for i from 0 to 9 do
           (loop for j from 0 to 9 do
                 (setf (aref cmp i j) (zerop (logand (random 1024) 512)))))
     (setq a (sort a #'(lambda (i j) (aref cmp i j))))
     (and (eqlt (length a) 10)
          (equalt (sort a #'<) '(0 1 2 3 4 5 6 7 8 9)))))
  t)

(deftest sort-vector.1
  (let ((a (copy-seq #(1 4 2 5 3))))
    (sort a #'<))
  #(1 2 3 4 5))

(deftest sort-vector.2
  (let ((a (copy-seq #(1 4 2 5 3))))
    (sort a #'< :key #'-))
  #(5 4 3 2 1))

(deftest sort-vector.3
  (let ((a (copy-seq #(1 4 2 5 3))))
    (sort a #'(lambda (x y) nil))
    (sort a #'<))
  #(1 2 3 4 5))

(deftest sort-vector.4
  (let ((a (make-array 10 :initial-contents '(10 40 20 50 30 15 45 25 55 35)
                       :fill-pointer 5)))
    (sort a #'<))
  #(10 20 30 40 50))

(deftest sort-vector.5
  (loop
   repeat 100
   always
   (let ((a (vector 1 2 3 4 5 6 7 8 9 0))
         (cmp (make-array '(10 10))))
     (loop for i from 0 to 9 do
           (loop for j from 0 to 9 do
                 (setf (aref cmp i j) (zerop (logand (random 1024) 512)))))
     (setq a (sort a #'(lambda (i j) (aref cmp i j))))
     (and (eqlt (length a) 10)
          (equalpt (sort a #'<) #(0 1 2 3 4 5 6 7 8 9)))))
  t)

(deftest sort-vector.6
  (do-special-integer-vectors
   (v #(1 4 7 3 2 6 5) nil)
   (let ((sv (sort v #'<)))
     (assert (equalp sv #(1 2 3 4 5 6 7)))))
  nil)

(deftest sort-vector.7
  (do-special-integer-vectors
   (v #(0 1 1 0 1 1 0 1 0) nil)
   (let ((sv (sort v #'<)))
     (assert (equalp sv #(0 0 0 0 1 1 1 1 1)))))
  nil)

(deftest sort-vector.8
  (do-special-integer-vectors
   (v #(0 -1 -1 0 -1 -1 0 -1 0) nil)
   (let ((sv (sort v #'>)))
     (assert (equalp sv #(0 0 0 0 -1 -1 -1 -1 -1)))))
  nil)

(deftest sort-vector.9
  (let* ((ivals '(1 4 7 3 2 6 5))
         (sivals '(1 2 3 4 5 6 7))
         (len (length ivals)))
    (loop for etype in '(short-float single-float double-float long-float rational)
          for vals = (loop for i in ivals collect (coerce i etype))
          for svals = (loop for i in sivals collect (coerce i etype))
          for vec = (make-array len :element-type etype :initial-contents vals)
          for svec = (sort vec #'<)
          unless (and (eql (length svec) len)
                      (every #'eql svals svec))
          collect (list etype vals svec)))
  nil)

(deftest sort-vector.10
  (let* ((ivals '(1 4 7 3 2 6 5))
         (sivals '(1 2 3 4 5 6 7))
         (len (length ivals)))
    (loop for cetype in '(short-float single-float double-float long-float rational)
          for etype = `(complex ,cetype)
          for vals = (loop for i in ivals collect (complex (coerce i cetype)
                                                           (coerce (- i) cetype)))
          for svals = (loop for i in sivals collect (complex (coerce i cetype)
                                                             (coerce (- i) cetype)))
          for vec = (make-array len :element-type etype :initial-contents vals)
          for svec = (sort vec #'(lambda (x y) (< (abs x) (abs y))))
          unless (and (eql (length svec) len)
                      (every #'eql svals svec))
          collect (list etype vals svec)))
  nil)

;;; Bit vectors

(deftest sort-bit-vector.1
  (let ((a (copy-seq #*10011101)))
    (sort a #'<))
  #*00011111)

(deftest sort-bit-vector.2
  (let ((a (copy-seq #*10011101)))
    (values (sort a #'< :key #'-) a))
  #*11111000
  #*11111000)

(deftest sort-bit-vector.3
  (let ((a (make-array 10 :initial-contents '(1 0 0 1 1 1 1 0 1 1)
                       :element-type 'bit
                       :fill-pointer 5)))
    (sort a #'<))
  #*00111)

(deftest sort-string.1
  (let ((a (copy-seq "10011101")))
    (values (sort a #'char<) a))
  "00011111"
  "00011111")

(deftest sort-string.2
  (let ((a (copy-seq "10011101")))
    (values (sort a #'char< :key #'(lambda (c) (if (eql c #\0) #\1 #\0))) a))
  "11111000"
  "11111000")

(deftest sort-string.3
  (let ((a (make-array 10 :initial-contents "1001111011"
                       :element-type 'character
                       :fill-pointer 5)))
    (sort a #'char<))
  "00111")

(deftest sort-string.4
  (do-special-strings
   (s "aebdc" nil)
   (let ((s2 (sort s #'char<)))
     (assert (eq s s2))
     (assert (string= s2 "abcde"))))
  nil)

;;; Order of evaluation tests

(deftest sort.order.1
  (let ((i 0) x y)
    (values
     (sort (progn (setf x (incf i)) (list 1 7 3 2))
           (progn (setf y (incf i)) #'<))
     i x y))
  (1 2 3 7) 2 1 2)

(deftest sort.order.2
  (let ((i 0) x y z)
    (values
     (sort (progn (setf x (incf i)) (list 1 7 3 2))
           (progn (setf y (incf i)) #'<)
           :key (progn (setf z (incf i)) #'-))
     i x y z))
  (7 3 2 1) 3 1 2 3)


;;; Error cases

(deftest sort.error.1
  (signals-error (sort) program-error)
  t)

(deftest sort.error.2
  (signals-error (sort nil) program-error)
  t)

(deftest sort.error.3
  (signals-error (sort nil #'< :key) program-error)
  t)

(deftest sort.error.4
  (signals-error (sort nil #'< 'bad t) program-error)
  t)

(deftest sort.error.5
  (signals-error (sort nil #'< 'bad t :allow-other-keys nil) program-error)
  t)

(deftest sort.error.6
  (signals-error (sort nil #'< 1 2) program-error)
  t)

(deftest sort.error.7
  (signals-error (sort (list 1 2 3 4) #'identity) program-error)
  t)

(deftest sort.error.8
  (signals-error (sort (list 1 2 3 4) #'< :key #'cons) program-error)
  t)

(deftest sort.error.9
  (signals-error (sort (list 1 2 3 4) #'< :key #'car) type-error)
  t)

(deftest sort.error.10
  (signals-error (sort (list 1 2 3 4) #'elt) type-error)
  t)

(deftest sort.error.11
  (check-type-error #'(lambda (x) (sort x #'<)) #'sequencep)
  nil)
