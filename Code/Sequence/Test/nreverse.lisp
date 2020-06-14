;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 21 00:04:57 2002
;;;; Contains: Tests for NREVERSE

(in-package #:sicl-sequence-test)

(deftest nreverse-list.1
  (nreverse nil)
  nil)

(deftest nreverse-list.2
  (let ((x (copy-seq '(a b c))))
    (nreverse x))
  (c b a))

(deftest nreverse-vector.1
  (nreverse #())
  #())

(deftest nreverse-vector.2
  (let ((x (copy-seq #(a b c d e))))
    (nreverse x))
  #(e d c b a))

(deftest nreverse-vector.4
  (let ((x (make-array 0 :fill-pointer t :adjustable t)))
    (nreverse x))
  #())

(deftest nreverse-vector.5
  (let* ((x (make-array 5 :initial-contents '(1 2 3 4 5)
                        :fill-pointer t :adjustable t))
         (y (nreverse x)))
    (values y (equalt (type-of x) (type-of y))))
  #(5 4 3 2 1)
  t)

(deftest nreverse-vector.6
  (let* ((x (make-array 10 :initial-contents '(1 2 3 4 5 6 7 8 9 10)
                        :fill-pointer 5))
         (y (nreverse x)))
    (values y (equalt (type-of x) (type-of y))))
  #(5 4 3 2 1)
  t)

;;; Unusual vectors

(deftest nreverse-vector.7
  (do-special-integer-vectors
   (v #(0 0 1 0 1 1) nil)
   (let ((nv (nreverse v)))
     (assert (= (length nv) 6))
     (assert (every #'= nv #(1 1 0 1 0 0)))))
  nil)

(deftest nreverse-vector.8
  (do-special-integer-vectors
   (v #(0 0 -1 0 -1 -1 0 -1) nil)
   (let ((nv (nreverse v)))
     (assert (= (length nv) 8))
     (assert (every #'= nv #(-1 0 -1 -1 0 -1 0 0)))))
  nil)

(deftest nreverse-vector.9
  (let ((len 10))
    (loop for etype in '(short-float single-float double-float long-float rational)
          for vals = (loop for i from 1 to len collect (coerce i etype))
          for vec = (make-array len :element-type etype :initial-contents vals)
          for nvec = (nreverse vec)
          unless (and (eql (length nvec) len)
                      (every #'eql (reverse vals) nvec))
          collect (list etype vals nvec)))
  nil)

(deftest nreverse-vector.10
  (let ((len 10))
    (loop for cetype in '(short-float single-float double-float long-float rational integer)
          for etype = `(complex ,cetype)
          for vals = (loop for i from 1 to len collect (complex (coerce i cetype)
                                                                (coerce (- i) cetype)))
          for vec = (make-array len :element-type etype :initial-contents vals)
          for nvec = (nreverse vec)
          unless (and (eql (length nvec) len)
                      (every #'eql (reverse vals) nvec))
          collect (list etype vals nvec)))
  nil)


;;; Bit vectors

(deftest nreverse-bit-vector.1
  (nreverse #*)
  #*)

(deftest nreverse-bit-vector.2
  (let ((x (copy-seq #*000110110110)))
    (nreverse x))
  #*011011011000)

(deftest nreverse-bit-vector.3
  (let* ((x (make-array 10 :initial-contents '(0 0 0 1 1 0 1 0 1 0)
                        :fill-pointer 5
                        :element-type 'bit))
         (y (nreverse x)))
    y)
  #*11000)

;;; Strings

(deftest nreverse-string.1
  (nreverse "")
  "")

(deftest nreverse-string.2
  (let ((x (copy-seq "000110110110")))
    (nreverse x))
  "011011011000")

(deftest nreverse-string.3
  (let* ((x (make-array 10 :initial-contents "abcdefghij"
                        :fill-pointer 5
                        :element-type 'character))
         (y (nreverse x)))
    y)
  "edcba")

(deftest nreverse-string.4
  (let* ((x (make-array 10 :initial-contents "abcdefghij"
                        :fill-pointer 5
                        :element-type 'base-char))
         (y (nreverse x)))
    y)
  "edcba")

(deftest nreverse-string.5
  (do-special-strings
   (s (copy-seq "12345") nil)
   (let ((s2 (nreverse s)))
     (assert (stringp s2))
     (assert (string= s2 "54321"))
     (assert (equal (array-element-type s) (array-element-type s2)))))
  nil)

;;; Argument is evaluated only once

(deftest nreverse.order.1
  (let ((i 0))
    (values
     (nreverse (progn (incf i) (list 'a 'b 'c 'd)))
     i))
  (d c b a) 1)

;;; Error tests

(deftest nreverse.error.1
  (check-type-error #'nreverse #'sequencep)
  nil)

(deftest nreverse.error.6
  (signals-error (nreverse) program-error)
  t)

(deftest nreverse.error.7
  (signals-error (nreverse nil nil) program-error)
  t)

(deftest nreverse.error.8
  (signals-error (locally (nreverse 'a) t) type-error)
  t)
