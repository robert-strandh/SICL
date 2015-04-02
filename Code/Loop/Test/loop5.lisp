;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov  2 13:52:50 2002
;;;; Contains: Tests of LOOP clause FOR-AS-ACROSS

(cl:in-package :sicl-loop-test)

(deftest loop.5.1
  (let ((x "abcd")) (loop for e across x collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.2
  (let ((x "abcd")) (loop for e across (the string x) collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.3
  (let ((x "abcd")) (loop for e across (the simple-string x) collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.4
  (loop for e across "abcd" collect e)
  (#\a #\b #\c #\d))

(deftest loop.5.5
  (loop for e across "abcd"
        for i from 1 to 3 collect e)
  (#\a #\b #\c))

(deftest loop.5.6
  (loop for e of-type base-char across "abcd"
        for i from 1 to 3 collect e)
  (#\a #\b #\c))

(deftest loop.5.7
  (let ((x (make-array '(4) :initial-contents "abcd" :element-type 'base-char)))
    (loop for e across (the base-string x) collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.8
  (let ((x "abcd")) (loop for e of-type character across x collect e))
  (#\a #\b #\c #\d))

(deftest loop.5.10
  (let ((x #*00010110))
    (loop for e across x collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.11
  (let ((x #*00010110))
    (loop for e across (the bit-vector x) collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.12
  (let ((x #*00010110))
    (loop for e across (the simple-bit-vector x) collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.13
  (let ((x #*00010110))
    (loop for e of-type bit across (the simple-bit-vector x) collect e))
  (0 0 0 1 0 1 1 0))

(deftest loop.5.14
  (let ((x #*00010110))
    (loop for e of-type bit across x
          for i from 1 to 4 collect e))
  (0 0 0 1))

(deftest loop.5.20
  (let ((x (vector 'a 'b 'c 'd)))
    (loop for e across x collect e))
  (a b c d))

(deftest loop.5.21
  (let ((x (vector 'a 'b 'c 'd)))
    (loop for e across (the vector x) collect e))
  (a b c d))

(deftest loop.5.22
  (let ((x (vector 'a 'b 'c 'd)))
    (loop for e across (the simple-vector x) collect e))
  (a b c d))

(deftest loop.5.23
  (let ((x (vector '(a) '(b) '(c) '(d))))
    (loop for (e) across x collect e))
  (a b c d))

(deftest loop.5.30
  (let ((x (make-array '(5) :initial-contents '(a b c d e)
                  :adjustable t)))
    (loop for e across x collect e))
  (a b c d e))

(deftest loop.5.32
  (let* ((x (make-array '(5) :initial-contents '(a b c d e)))
         (y (make-array '(3) :displaced-to x
                   :displaced-index-offset 1)))
    (loop for e across y collect e))
  (b c d))

;;; tests of 'as' form

(deftest loop.5.33
  (loop as e across "abc" collect e)
  (#\a #\b #\c))

(deftest loop.5.34
  (loop as e of-type character across "abc" collect e)
  (#\a #\b #\c))

(deftest loop.5.35
  (loop as e of-type integer across (the simple-vector (coerce '(1 2 3) 'simple-vector))
        sum e)
  6)

;;; Loop across displaced vectors

(deftest loop.5.36
  (let* ((a (make-array '(10) :initial-contents '(a b c d e f g h i j)))
         (da (make-array '(5) :displaced-to a
                         :displaced-index-offset 2)))
    (loop for e across da collect e))
  (c d e f g))

(deftest loop.5.37
  (let* ((a (make-array '(10) :element-type 'base-char
                        :initial-contents "abcdefghij"))
         (da (make-array '(5) :element-type 'base-char
                         :displaced-to a
                         :displaced-index-offset 2)))
    (loop for e across da collect e))
  (#\c #\d #\e #\f #\g))

(deftest loop.5.38
  (let* ((a (make-array '(10) :element-type 'bit
                        :initial-contents '(0 1 1 0 0 1 0 1 1 1)))
         (da (make-array '(5) :element-type 'bit
                         :displaced-to a
                         :displaced-index-offset 2)))
    (loop for e across da collect e))
  (1 0 0 1 0))

(deftest loop.5.39
  (let ((v (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
                       :fill-pointer 6)))
    (loop for x across v collect x))
  (1 2 3 4 5 6))

(deftest loop.5.40
  (loop for i from 1 to 40
        for type = `(unsigned-byte ,i)
        for v = (make-array '(10) :initial-contents '(0 0 1 1 0 1 1 1 0 0)
                            :element-type type)
        for r = (loop for x across v collect x)
        unless (equal r '(0 0 1 1 0 1 1 1 0 0))
        collect (list i r))
  nil)

(deftest loop.5.41
  (loop for i from 1 to 40
        for type = `(signed-byte ,i)
        for v = (make-array '(10) :initial-contents '(0 0 -1 -1 0 -1 -1 -1 0 0)
                            :element-type type)
        for r = (loop for x across v collect x)
        unless (equal r '(0 0 -1 -1 0 -1 -1 -1 0 0))
        collect (list i r))
  nil)

(deftest loop.5.42
  (let ((vals '(0 0 1 1 0 1 1 1 0 0)))
    (loop for type in '(short-float single-float double-float long-float)
          for fvals = (loop for v in vals collect (coerce v type))
          for v = (make-array '(10) :initial-contents fvals :element-type type)
          for r = (loop for x across v collect x)
          unless (equal r fvals)
          collect (list fvals r)))
  nil)

(deftest loop.5.43
  (let ((vals '(0 0 1 1 0 1 1 1 0 0)))
    (loop for etype in '(short-float single-float double-float long-float)
          for type = `(complex ,etype)
          for fvals = (loop for v in vals collect (coerce v type))
          for v = (make-array '(10) :initial-contents fvals :element-type type)
          for r = (loop for x across v collect x)
          unless (equal r fvals)
          collect (list fvals r)))
  nil)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.5.44
  (macrolet
   ((%m (z) z))
   (loop for x across (expand-in-current-env (%m "148X")) collect x))
  (#\1 #\4 #\8 #\X))

(deftest loop.5.45
  (macrolet
   ((%m (z) z))
   (loop as x across (expand-in-current-env (%m #*00110110)) collect x))
  (0 0 1 1 0 1 1 0))

;;; FIXME
;;; Add tests for other specialized array types (integer types, floats, complex)

;;; Error cases

;; (deftest loop.5.error.1
;;   (signals-error
;;    (loop for (e . e) across (vector '(x . y) '(u . v)) collect e)
;;    program-error)
;;   t)

;; (deftest loop.5.error.2
;;   (signals-error
;;    (loop for e across (vector '(x . y) '(u . v))
;;          for e from 1 to 5 collect e)
;;    program-error)
;;   t)

;; (deftest loop.5.error.3
;;   (signals-error
;;    (macroexpand
;;     '(loop for (e . e) across (vector '(x . y) '(u . v)) collect e))
;;    program-error)
;;   t)

;; (deftest loop.5.error.4
;;   (signals-error
;;    (macroexpand
;;     '(loop for e across (vector '(x . y) '(u . v))
;;            for e from 1 to 5 collect e))
;;    program-error)
;;   t)
