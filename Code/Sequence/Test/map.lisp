;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 17 20:54:48 2002
;;;; Contains: Tests for the MAP function

(in-package #:sicl-sequence-test)

(deftest map-array.1
  (map 'list #'1+ #(1 2 3 4))
  (2 3 4 5))

(deftest map-array.2
  (map 'vector #'+ #(1 2 3 4) #(6 6 6 6))
  #(7 8 9 10))

(deftest map-array.3
  (map 'vector #'+ #(1 2 3 4 5) #(6 6 6 6))
  #(7 8 9 10))

(deftest map-array.4
  (map 'vector #'+ #(1 2 3 4) #(6 6 6 6 6))
  #(7 8 9 10))

(deftest map-array.5
  (map '(vector *) #'+ #(1 2 3 4) #(6 6 6 6))
  #(7 8 9 10))

(deftest map-array.6
  (map '(vector * 4) #'+ #(1 2 3 4) #(6 6 6 6))
  #(7 8 9 10))

;;; (deftest map-array.7
;;;  (map 'array #'identity '(a b c d e f))
;;;  #(a b c d e f))

;;; (deftest map-array.8
;;;   (map 'simple-array #'identity '(a b c d e f))
;;;   #(a b c d e f))

(deftest map-array.9
  (map 'simple-vector #'identity '(a b c d e f))
  #(a b c d e f))

(deftest map-array.10
  (map 'simple-vector #'cons '(a b c d e f) #(1 2 3 4 5 6))
  #((a . 1) (b . 2) (c . 3) (d . 4) (e . 5) (f . 6)))

(deftest map-array.11
  (map 'vector #'identity '(#\a #\b #\c #\d #\e))
  #(#\a #\b #\c #\d #\e))

(deftest map-array.12
  (map 'vector #'identity "abcde")
  #(#\a #\b #\c #\d #\e))

(deftest map-array.13
  (map 'vector #'identity #*000001)
  #(0 0 0 0 0 1))

(deftest map-array.14
  (map 'list #'identity #*000001)
  (0 0 0 0 0 1))

(deftest map-bit-vector.15
  (map 'bit-vector #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.16
  (map 'simple-bit-vector #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.17
  (map '(vector bit) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.18
  (map '(simple-vector *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.19
  (map '(bit-vector 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.20
  (map '(bit-vector *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.21
  (map '(simple-bit-vector 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.22
  (map '(simple-bit-vector *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.23
  (map '(vector bit 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.24
  (map '(vector bit *) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-bit-vector.25
  (map '(simple-vector 6) #'identity '(0 0 0 0 0 1))
  #*000001)

(deftest map-string.26
  (map 'string #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

(deftest map-string.27
  (map 'string #'identity "abcde")
  "abcde")

(deftest map-string.28
  (map '(vector character) #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

(deftest map-string.29
  (map '(vector character 5) #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

(deftest map-string.30
  (map '(simple-vector 5) #'identity '(#\a #\b #\c #\d #\e))
  "abcde")

;;; Use a more elaborate form of the simple-array type specifier
;;; (deftest map-string.31
;;;  (map '(simple-array character *) #'identity "abcde")
;;;  "abcde")

;;; Use a more elaborate form of the simple-array type specifier
;;; (deftest map-string.32
;;;  (map '(simple-array character 5) #'identity "abcde")
;;;   "abcde")

(deftest map-nil.33
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) "abcdef") a))
  nil (#\f #\e #\d #\c #\b #\a))

(deftest map-nil.34
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) '(a b c d e)) a))
  nil (e d c b a))

(deftest map-nil.35
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) #(a b c d e)) a))
  nil (e d c b a))

(deftest map-nil.36
  (let ((a nil))
    (values (map nil #'(lambda (x) (push x a)) #*001011110) a))
  nil (0 1 1 1 1 0 1 0 0))

(deftest map-null.1
  (map 'null #'identity nil)
  nil)

(deftest map-cons.1
  (map 'cons #'identity '(a b c))
  (a b c))

(deftest map.37
  (map 'simple-string #'identity '(#\a #\b #\c))
  "abc")

(deftest map.38
  (map '(simple-string) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.39
  (map '(simple-string *) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.40
  (map '(simple-string 3) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.41
  (map '(base-string) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.42
  (map '(base-string *) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.43
  (map '(base-string 3) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.44
  (map 'simple-base-string #'identity '(#\a #\b #\c))
  "abc")

(deftest map.45
  (map '(simple-base-string) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.46
  (map '(simple-base-string *) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.47
  (map '(simple-base-string 3) #'identity '(#\a #\b #\c))
  "abc")

(deftest map.48
  :notes (:result-type-element-type-by-subtype)
  (let ((type '(or (vector t 10) (vector t 5))))
    (if (subtypep type '(vector t))
        (equalpt (map type #'identity '(1 2 3 4 5)) #(1 2 3 4 5))
      t))
  t)

;;; Error tests

(deftest map.error.1
  (signals-error-always (map 'symbol #'identity '(a b c)) type-error)
  t t)

(deftest map.error.1a
  (signals-error (map 'symbol #'identity '(a b c)) type-error)
  t)

(deftest map.error.2
  (signals-error (map '(vector * 8) #'identity '(a b c)) type-error)
  t)

(deftest map.error.3
  (signals-error (map 'list #'identity '(a b . c)) type-error)
  t)

(deftest map.error.4
  (signals-error (map) program-error)
  t)

(deftest map.error.5
  (signals-error (map 'list) program-error)
  t)

(deftest map.error.6
  (signals-error (map 'list #'null) program-error)
  t)

(deftest map.error.7
  (signals-error (map 'list #'cons '(a b c d)) program-error)
  t)

(deftest map.error.8
  (signals-error (map 'list #'cons '(a b c d) '(1 2 3 4) '(5 6 7 8))
                 program-error)
  t)

(deftest map.error.9
  (signals-error (map 'list #'car '(a b c d)) type-error)
  t)

(deftest map.error.10
  :notes (:result-type-element-type-by-subtype)
  (let ((type '(or (vector bit) (vector t))))
    (if (subtypep type 'vector)
        (eval `(signals-error-always (map ',type #'identity '(1 0 1)) error))
      (values t t)))
  t t)

(deftest map.error.11
  (let ((type '(or (vector t 5) (vector t 10))))
    (if (subtypep type 'vector)
        (eval `(signals-error (map ',type #'identity '(1 2 3 4 5 6)) type-error))
      t))
  t)

(deftest map.error.12
  (check-type-error #'(lambda (x) (map 'list #'identity x)) #'sequencep)
  nil)

(deftest map.error.13
  (check-type-error #'(lambda (x) (map 'vector #'cons '(a b c d) x)) #'sequencep)
  nil)

;;; Test mapping on arrays with fill pointers

(deftest map.fill.1
  (let ((s1 (make-array '(10) :initial-contents '(a b c d e f g h i j)
                        :fill-pointer 8)))
    (map 'list #'identity s1))
  (a b c d e f g h))

(deftest map.fill.2
  (let ((s1 (make-array '(10) :initial-contents '(a b c d e f g h i j)
                        :fill-pointer 8)))
    (map 'list #'(lambda (x y) x) '(1 2 3 4 5 6 7 8 9 10) s1))
  (1 2 3 4 5 6 7 8))

(deftest map.fill.3
  (let ((s1 (make-array '(10) :initial-element #\a
                        :element-type 'character
                        :fill-pointer 8)))
    (map 'string #'identity s1))
  "aaaaaaaa")

(deftest map.fill.4
  (let ((s1 (make-array '(10) :initial-element #\a
                        :element-type 'base-char
                        :fill-pointer 8)))
    (map 'list #'(lambda (x y) x) '(1 2 3 4 5 6 7 8 9 10) s1))
  (1 2 3 4 5 6 7 8))

(deftest map.fill.5
  (let ((s1 (make-array '(10) :initial-element 0
                        :element-type 'bit
                        :fill-pointer 8)))
    (map 'bit-vector #'identity s1))
  #*00000000)

(deftest map.fill.6
  (let ((s1 (make-array '(10) :initial-element 1
                        :element-type 'bit
                        :fill-pointer 8)))
    (map 'list #'(lambda (x y) x) '(1 2 3 4 5 6 7 8 9 10) s1))
  (1 2 3 4 5 6 7 8))

;;; Specialized string tests

(deftest map.specialized-string.1
  (do-special-strings
   (s "abcde" nil)
   (let ((s2 (map 'list #'identity s)))
     (assert (equal s2 '(#\a #\b #\c #\d #\e)))))
  nil)

(deftest map.specialized-string.2
  (do-special-strings
   (s "abcde" nil)
   (let ((s2 (map 'list #'(lambda (x y) y) '(1 2 3 4 5) s)))
     (assert (equal s2 '(#\a #\b #\c #\d #\e)))))
  nil)

(deftest map.specialized-string.3
  (let ((s (map 'base-string #'identity '(#\a #\b #\c))))
    (assert (typep s 'base-string))
    s)
  "abc")

;;; FIXME: Add tests for building strings of other character types

;;; Special vector types

(deftest map.specialized-vector.1
  (do-special-integer-vectors
   (v #(0 1 1 0 0 1) nil)
   (assert (equal (map 'list #'list v v) '((0 0) (1 1) (1 1) (0 0) (0 0) (1 1)))))
  nil)

(deftest map.specialized-vector.2
  (do-special-integer-vectors
   (v #(1 2 3 4 5 6 7) nil)
   (assert (equal (map 'list #'identity v) '(1 2 3 4 5 6 7))))
  nil)

(deftest map.specialized-vector.3
  (do-special-integer-vectors
   (v #(-1 -2 -3 -4 -5 -6 -7) nil)
   (assert (equal (map 'list #'- v) '(1 2 3 4 5 6 7))))
  nil)

(deftest map.specialized-vector.4
  (loop for i from 1 to 40
        for type = `(unsigned-byte ,i)
        for bound = (ash 1 i)
        for len = 10
        for vals = (loop repeat len collect (random i))
        for result = (map `(vector ,type) #'identity vals)
        unless (and (= (length result) len)
                    (every #'eql vals result))
        collect (list i vals result))
  nil)

(deftest map.specialized-vector.5
  (loop for i from 1 to 40
        for type = `(signed-byte ,i)
        for bound = (ash 1 i)
        for len = 10
        for vals = (loop repeat len collect (- (random i) (/ bound 2)))
        for result = (map `(vector ,type) #'identity vals)
        unless (and (= (length result) len)
                    (every #'eql vals result))
        collect (list i vals result))
  nil)

(deftest map.specialized-vector.6
  (loop for type in '(short-float single-float long-float double-float)
        for len = 10
        for vals = (loop for i from 1 to len collect (coerce i type))
        for result = (map `(vector ,type) #'identity vals)
        unless (and (= (length result) len)
                    (every #'eql vals result))
        collect (list type vals result))
  nil)

(deftest map.specialized-vector.7
  (loop for etype in '(short-float single-float long-float double-float
                       integer rational)
        for type = `(complex ,etype)
        for len = 10
        for vals = (loop for i from 1 to len collect (complex (coerce i etype)
                                                              (coerce (- i) etype)))
        for result = (map `(vector ,type) #'identity vals)
        unless (and (= (length result) len)
                    (every #'eql vals result))
        collect (list type vals result))
  nil)

;;; Order of evaluation tests

(deftest map.order.1
  (let ((i 0) a b c d)
    (values
     (map (progn (setf a (incf i)) 'list)
          (progn (setf b (incf i)) #'list)
          (progn (setf c (incf i)) '(a b c))
          (progn (setf d (incf i)) '(b c d)))
     i a b c d))
  ((a b)(b c)(c d)) 4 1 2 3 4)

;;; Constant folding test

(def-fold-test map.fold.1 (map 'vector #'identity '(a b c)))
