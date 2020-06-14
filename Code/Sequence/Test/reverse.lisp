;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 23:47:28 2002
;;;; Contains: Tests for REVERSE

(in-package #:sicl-sequence-test)

(deftest reverse-list.1
  (reverse nil)
  nil)

(deftest reverse-list.2
  (let ((x '(a b c)))
    (values (reverse x) x))
  (c b a)
  (a b c))

(deftest reverse-vector.1
  (reverse #())
  #())

(deftest reverse-vector.2
  (let ((x #(a b c d e)))
    (values (reverse x) x))
  #(e d c b a)
  #(a b c d e))

(deftest reverse-vector.3
  (let ((x (make-array 0 :fill-pointer t :adjustable t)))
    (reverse x))
  #())

(deftest reverse-vector.4
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)
                        :fill-pointer t :adjustable t))
         (y (reverse x)))
    (values y x))
  #(5 4 3 2 1)
  #(1 2 3 4 5))

(deftest reverse-vector.5
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 10)
                        :fill-pointer 5))
         (y (reverse x)))
    y)
  #(5 4 3 2 1))

;;; Other unusual vectors

(deftest reverse-vector.6
  (do-special-integer-vectors
   (v #(1 1 0 1 1 0) nil)
   (let ((nv (reverse v)))
     (assert (typep nv 'simple-array))
     (assert (not (eql v nv)))
     (assert (equalp nv #(0 1 1 0 1 1)))
     (assert (equalp v #(1 1 0 1 1 0)))))
  nil)

(deftest reverse-vector.7
  (do-special-integer-vectors
   (v #(-1 -1 0 -1 -1 0) nil)
   (let ((nv (reverse v)))
     (assert (typep nv 'simple-array))
     (assert (not (eql v nv)))
     (assert (equalp nv #(0 -1 -1 0 -1 -1)))
     (assert (equalp v #(-1 -1 0 -1 -1 0)))))
  nil)

(deftest reverse-vector.8
  (let ((len 10))
    (loop for etype in '(short-float single-float double-float long-float rational)
          for vals = (loop for i from 1 to len collect (coerce i etype))
          for vec = (make-array len :element-type etype :initial-contents vals)
          for nvec = (reverse vec)
          unless (and (eql (length nvec) len)
                      (typep nvec 'simple-array)
                      (not (eql vec nvec))
                      (every #'eql (reverse vals) nvec)
                      (every #'eql vals vec))
          collect (list etype vals vec nvec)))
  nil)

(deftest reverse-vector.9
  (let ((len 10))
    (loop for cetype in '(short-float single-float double-float long-float rational integer)
          for etype = `(complex ,cetype)
          for vals = (loop for i from 1 to len collect (complex (coerce i cetype)
                                                                (coerce (- i) cetype)))
          for vec = (make-array len :element-type etype :initial-contents vals)
          for nvec = (reverse vec)
          unless (and (eql (length nvec) len)
                      (typep nvec 'simple-array)
                      (not (eql vec nvec))
                      (every #'eql (reverse vals) nvec)
                      (every #'eql vals vec))
          collect (list etype vals vec nvec)))
  nil)

;;; Bit vectors

(deftest reverse-bit-vector.1
  (reverse #*)
  #*)

(deftest reverse-bit-vector.2
  (let ((x #*000110110110))
    (values (reverse x) x))
  #*011011011000
  #*000110110110)

(deftest reverse-bit-vector.3
  (let* ((x (make-array 10 :initial-contents '(0 0 0 1 1 0 1 0 1 0)
                        :fill-pointer 5
                        :element-type 'bit))
         (y (reverse x)))
    y)
  #*11000)

;;; Strings

(deftest reverse-string.1
  (reverse "")
  "")

(deftest reverse-string.2
  (let ((x "000110110110"))
    (values (reverse x) x))
  "011011011000"
  "000110110110")

(deftest reverse-string.3
  (let* ((x (make-array 10 :initial-contents "abcdefghij"
                        :fill-pointer 5
                        :element-type 'character))
         (y (reverse x)))
    y)
  "edcba")

(deftest reverse-string.4
  (let* ((x (make-array 10 :initial-contents "abcdefghij"
                        :fill-pointer 5
                        :element-type 'base-char))
         (y (reverse x)))
    y)
  "edcba")

;;; Specialized string tests

(deftest reverse-string.5
  (do-special-strings
   (s (copy-seq "12345") nil)
   (let ((s2 (reverse s)))
     (assert (typep s2 'simple-array))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= "12345" s))
     (assert (string= "54321" s2))))
  nil)

;;; Order, number of times of evaluation

(deftest reverse.order.1
  (let ((i 0))
    (values
     (reverse (progn (incf i) (list 'a 'b 'c 'd)))
     i))
  (d c b a) 1)

;;; Constant folding tests

(def-fold-test reverse.fold.1 (reverse '(a b c)))
(def-fold-test reverse.fold.2 (reverse #(a b c)))
(def-fold-test reverse.fold.3 (reverse #*00111101011011))
(def-fold-test reverse.fold.4 (reverse "abcdefgh"))

;;; Error cases

(deftest reverse.error.1
  (check-type-error #'reverse #'sequencep)
  nil)

(deftest reverse.error.6
  (signals-error (reverse) program-error)
  t)

(deftest reverse.error.7
  (signals-error (reverse nil nil) program-error)
  t)

(deftest reverse.error.8
  (signals-error (locally (reverse 'a) t) type-error)
  t)
