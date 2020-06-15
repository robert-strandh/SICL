;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 14 09:58:47 2002
;;;; Contains: Tests for MAKE-SEQUENCE

(in-package #:sicl-sequence-test)

(deftest make-sequence.1
  (let ((x (make-sequence 'list 4)))
    (and (eql (length x) 4)
         (listp x)
         #+:ansi-tests-strict-initial-element
         (loop for e in x always (eql (car x) e))
         t))
  t)

(deftest make-sequence.2
  (make-sequence 'list 4 :initial-element 'a)
  (a a a a))

(deftest make-sequence.3
  (let ((x (make-sequence 'cons 4)))
    (and (eql (length x) 4)
         (listp x)
         #+:ansi-tests-strict-initial-element
         (loop for e in x always (eql (car x) e))
         t))
  t)

(deftest make-sequence.4
  (make-sequence 'cons 4 :initial-element 'a)
  (a a a a))

(deftest make-sequence.5
  (make-sequence 'string 10 :initial-element #\a)
  "aaaaaaaaaa")

(deftest make-sequence.6
  (let ((s (make-sequence 'string 10)))
    (and (eql (length s) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across s always (eql e (aref s 0)))
         t))
  t)

(deftest make-sequence.7
  (make-sequence 'simple-string 10 :initial-element #\a)
  "aaaaaaaaaa")


(deftest make-sequence.8
  (let ((s (make-sequence 'simple-string 10)))
    (and (eql (length s) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across s always (eql e (aref s 0)))
         t))
  t)

(deftest make-sequence.9
  (make-sequence 'null 0)
  nil)

(deftest make-sequence.10
  (let ((x (make-sequence 'vector 10)))
    (and (eql (length x) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across x always (eql e (aref x 0)))
         t))
  t)

(deftest make-sequence.11
  (let* ((u (list 'a))
         (x (make-sequence 'vector 10 :initial-element u)))
    (and (eql (length x) 10)
         (loop for e across x always (eql e u))
         t))
  t)

(deftest make-sequence.12
  (let ((x (make-sequence 'simple-vector 10)))
    (and (eql (length x) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across x always (eql e (aref x 0)))
         t))
  t)

(deftest make-sequence.13
  (let* ((u (list 'a))
         (x (make-sequence 'simple-vector 10 :initial-element u)))
    (and (eql (length x) 10)
         (loop for e across x always (eql e u))
         t))
  t)

(deftest make-sequence.14
  (let ((x (make-sequence '(vector *) 10)))
    (and (eql (length x) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across x always (eql e (aref x 0)))
         t))
  t)

(deftest make-sequence.15
  (let* ((u (list 'a))
         (x (make-sequence '(vector *) 10 :initial-element u)))
    (and (eql (length x) 10)
         (loop for e across x always (eql e u))
         t))
  t)

(deftest make-sequence.16
  (let ((x (make-sequence '(simple-vector *)  10)))
    (and (eql (length x) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across x always (eql e (aref x 0)))
         t))
  t)

(deftest make-sequence.17
  (let* ((u (list 'a))
         (x (make-sequence '(simple-vector *) 10 :initial-element u)))
    (and (eql (length x) 10)
         (loop for e across x always (eql e u))
         t))
  t)

(deftest make-sequence.18
  (let ((x (make-sequence '(string *) 10)))
    (and (eql (length x) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across x always (eql e (aref x 0)))
         t))
  t)

(deftest make-sequence.19
  (let* ((u #\a)
         (x (make-sequence '(string *) 10 :initial-element u)))
    (and (eql (length x) 10)
         (loop for e across x always (eql e u))
         t))
  t)

(deftest make-sequence.20
  (let ((x (make-sequence '(simple-string *)  10)))
    (and (eql (length x) 10)
         #+:ansi-tests-strict-initial-element
         (loop for e across x always (eql e (aref x 0)))
         t))
  t)

(deftest make-sequence.21
  (let* ((u #\a)
         (x (make-sequence '(simple-string *) 10 :initial-element u)))
    (and (eql (length x) 10)
         (loop for e across x always (eql e u))
         t))
  t)

(deftest make-sequence.22
  (make-sequence '(vector * 5) 5 :initial-element 'a)
  #(a a a a a))

(deftest make-sequence.23
  (make-sequence '(vector fixnum 5) 5 :initial-element 1)
  #(1 1 1 1 1))

(deftest make-sequence.24
  (make-sequence '(vector (integer 0 255) 5) 5 :initial-element 17)
  #(17 17 17 17 17))

(deftest make-sequence.25
  (make-sequence '(simple-vector 5) 5 :initial-element 'a)
  #(a a a a a))

#+:ansi-tests-strict-initial-element
(deftest make-sequence.26
  (equalp (make-sequence 'string 5) (make-string 5))
  t)

(deftest make-sequence.27
  (let ((len 10))
    (loop for i from 1 to 40
          for etype = `(unsigned-byte ,i)
          for type = `(vector ,etype)
          for vec = (make-sequence type len :initial-element 0)
          unless (and (typep vec type)
                      (loop for i below len always (eql (elt vec i) 0)))
          collect (list i etype type vec)))
  nil)

(deftest make-sequence.28
  (let ((len 10))
    (loop for i from 1 to 40
          for etype = `(signed-byte ,i)
          for type = `(vector ,etype)
          for vec = (make-sequence type len :initial-element 0)
          unless (and (typep vec type)
                      (loop for i below len always (eql (elt vec i) 0)))
          collect (list i etype type vec)))
  nil)

(deftest make-sequence.29
  (let ((len 10))
    (loop for etype in '(short-float single-float double-float long-float)
          for type = `(vector ,etype)
          for elem = (coerce 1 etype)
          for vec = (make-sequence type len :initial-element elem)
          unless (and (typep vec type)
                      (loop for i below len always (eql (elt vec i) elem)))
          collect (list etype type vec)))
  nil)

(deftest make-sequence.30
  (let ((len 10))
    (loop for cetype in '(short-float single-float double-float long-float
                                      integer rational)
          for etype = `(complex ,cetype)
          for type = `(vector ,etype)
          for elem = (complex (coerce 1 cetype) (coerce -1 cetype))
          for vec = (make-sequence type len :initial-element elem)
          unless (and (typep vec type)
                      (loop for i below len always (eql (elt vec i) elem)))
          collect (list etype type vec)))
  nil)

;;; Other type specifiers

(deftest make-sequence.31
  (make-sequence '(simple-string) 10 :initial-element #\X)
  "XXXXXXXXXX")

(deftest make-sequence.32
  (make-sequence '(simple-string 10) 10 :initial-element #\X)
  "XXXXXXXXXX")

(deftest make-sequence.33
  (make-sequence '(string) 10 :initial-element #\X)
  "XXXXXXXXXX")

(deftest make-sequence.34
  (make-sequence '(vector) 10 :initial-element nil)
  #(nil nil nil nil nil nil nil nil nil nil))

(deftest make-sequence.35
  (make-sequence '(simple-vector) 10 :initial-element nil)
  #(nil nil nil nil nil nil nil nil nil nil))

(deftest make-sequence.36
  (make-sequence '(vector * *) 10 :initial-element nil)
  #(nil nil nil nil nil nil nil nil nil nil))

;;; Bit vectors

(deftest make-sequence.37
  (make-sequence 'bit-vector 5 :initial-element 0)
  #*00000)

(deftest make-sequence.38
  (make-sequence 'bit-vector 7 :initial-element 1)
  #*1111111)

(deftest make-sequence.39
  (make-sequence 'bit-vector 0)
  #*)

(deftest make-sequence.40
  (make-sequence '(bit-vector) 4 :initial-element 1)
  #*1111)

(deftest make-sequence.41
  (make-sequence '(bit-vector *) 10 :initial-element 0)
  #*0000000000)

(deftest make-sequence.42
  (make-sequence '(bit-vector 5) 5 :initial-element 0)
  #*00000)

(deftest make-sequence.43
  (make-sequence 'simple-bit-vector 5 :initial-element 0)
  #*00000)

(deftest make-sequence.44
  (make-sequence 'simple-bit-vector 7 :initial-element 1)
  #*1111111)

(deftest make-sequence.45
  (make-sequence 'simple-bit-vector 0)
  #*)

(deftest make-sequence.46
  (make-sequence '(simple-bit-vector) 4 :initial-element 1)
  #*1111)

(deftest make-sequence.47
  (make-sequence '(simple-bit-vector *) 10 :initial-element 0)
  #*0000000000)

(deftest make-sequence.48
  (make-sequence '(simple-bit-vector 5) 5 :initial-element 0)
  #*00000)

(deftest make-sequence.49
  (if (subtypep (class-of nil) 'sequence)
      (make-sequence (class-of nil) 0)
    nil)
  nil)

(deftest make-sequence.50
  (if (subtypep (class-of '(nil nil nil)) 'sequence)
      (make-sequence (class-of '(nil nil nil)) 3 :initial-element nil)
    '(nil nil nil))
  (nil nil nil))

(deftest make-sequence.51
  (loop for i from 1 to 40
        for vec = (make-array 1 :element-type `(unsigned-byte ,i)
                              :initial-element 1)
        for class = (class-of vec)
        nconc
        (if (subtypep class 'vector)
            (let ((vec2 (make-sequence class 1 :initial-element 1)))
              (unless (equalp vec vec)
                (list (list i vec class vec2))))
          nil))
  nil)

(deftest make-sequence.52
  (let ((class (class-of "aaaa")))
    (if (subtypep class 'vector)
        (make-sequence class 4 :initial-element #\a)
      "aaaa"))
  "aaaa")

(deftest make-sequence.53
  (let ((class (class-of (make-array 4 :element-type 'base-char
                                     :fill-pointer 4
                                     :adjustable t
                                     :initial-contents "aaaa"))))
    (if (subtypep class 'vector)
        (make-sequence class 4 :initial-element #\a)
      "aaaa"))
  "aaaa")

(deftest make-sequence.54
  (let ((class (class-of (make-array 4 :element-type 'character
                                     :fill-pointer 4
                                     :adjustable t
                                     :initial-contents "aaaa"))))
    (if (subtypep class 'vector)
        (make-sequence class 4 :initial-element #\a)
      "aaaa"))
  "aaaa")

(deftest make-sequence.55
  (let ((class (class-of (make-array 4 :element-type 'character
                                     :initial-contents "aaaa"))))
    (if (subtypep class 'vector)
        (make-sequence class 4 :initial-element #\a)
      "aaaa"))
  "aaaa")

(deftest make-sequence.56
  (loop for i from 1 to 40
        for vec = (make-array 1 :element-type `(unsigned-byte ,i)
                              :adjustable t :fill-pointer 1
                              :initial-element 1)
        for class = (class-of vec)
        nconc
        (if (subtypep class 'vector)
            (let ((vec2 (make-sequence class 1 :initial-element 1)))
              (unless (equalp vec vec)
                (list (list i vec class vec2))))
          nil))
  nil)

(deftest make-sequence.57
  (make-sequence (find-class 'list) 4 :initial-element 'x)
  (x x x x))

(deftest make-sequence.58
  (make-sequence (find-class 'cons) 4 :initial-element 'x)
  (x x x x))

;;; Keyword tests

(deftest make-sequence.allow-other-keys.1
  (make-sequence 'list 5 :allow-other-keys t :initial-element 'a :bad t)
  (a a a a a))

(deftest make-sequence.allow-other-keys.2
  (make-sequence 'list 5 :initial-element 'a :bad t :allow-other-keys t)
  (a a a a a))

(deftest make-sequence.allow-other-keys.3
  (make-sequence 'list 5 :initial-element 'a :allow-other-keys t)
  (a a a a a))

(deftest make-sequence.allow-other-keys.4
  (make-sequence 'list 5 :initial-element 'a :allow-other-keys nil)
  (a a a a a))

(deftest make-sequence.allow-other-keys.5
  (make-sequence 'list 5 :initial-element 'a :allow-other-keys t
                 :allow-other-keys nil :bad t)
  (a a a a a))

(deftest make-sequence.keywords.6
  (make-sequence 'list 5 :initial-element 'a :initial-element 'b)
  (a a a a a))

;;; Tests for errors

(deftest make-sequence.error.1
  (signals-error-always (make-sequence 'symbol 10) type-error)
  t t)

(deftest make-sequence.error.2
  (signals-error (make-sequence 'null 1) type-error)
  t)

(deftest make-sequence.error.3
  (signals-error (make-sequence '(vector * 4) 3) type-error)
  t)

(deftest make-sequence.error.4
  (signals-error (make-sequence '(vector * 2) 3) type-error)
  t)

(deftest make-sequence.error.5
  (signals-error (make-sequence '(string 4) 3) type-error)
  t)

(deftest make-sequence.error.6
  (signals-error (make-sequence '(simple-string 2) 3) type-error)
  t)

(deftest make-sequence.error.7
  (signals-error (make-sequence 'cons 0) type-error)
  t)

(deftest make-sequence.error.8
  (signals-error (make-sequence) program-error)
  t)

(deftest make-sequence.error.9
  (signals-error (make-sequence 'list) program-error)
  t)

(deftest make-sequence.error.10
  (signals-error (make-sequence 'list 10 :bad t) program-error)
  t)

(deftest make-sequence.error.11
  (signals-error (make-sequence 'list 10 :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest make-sequence.error.12
  (signals-error (make-sequence 'list 10 :initial-element)
                 program-error)
  t)

(deftest make-sequence.error.13
  (signals-error (make-sequence 'list 10 0 0) program-error)
  t)

(deftest make-sequence.error.14
  (signals-error-always (locally (make-sequence 'symbol 10) t)
                        type-error)
  t t)

(deftest make-sequence.error.15
  :notes (:result-type-element-type-by-subtype)
  (if (subtypep '(or (vector bit) (vector t)) 'vector)
      (signals-error (make-sequence '(or (vector bit) (vector t)) 10 :initial-element 0) error)
    t)
  t)

(deftest make-sequence.error.16
  (signals-error-always (make-sequence (find-class 'integer) 0) type-error)
  t t)

;;; Order of execution tests

(deftest make-sequence.order.1
  (let ((i 0) a b c)
    (values
     (make-sequence (progn (setf a (incf i)) 'list)
                    (progn (setf b (incf i)) 5)
                    :initial-element (progn (setf c (incf i)) 'a))
     i a b c))
  (a a a a a) 3 1 2 3)

(deftest make-sequence.order.2
  (let ((i 0) a b c d e)
    (values
     (make-sequence (progn (setf a (incf i)) 'list)
                    (progn (setf b (incf i)) 5)
                    :allow-other-keys (setf c (incf i))
                    :initial-element (progn (setf d (incf i)) 'a)
                    :foo (setf e (incf i)))
     i a b c d e))
  (a a a a a) 5 1 2 3 4 5)

;;; Const fold tests

(def-fold-test make-sequence.fold.1
  (make-sequence 'list 5 :initial-element 'a))
(def-fold-test make-sequence.fold.2
  (make-sequence 'vector 5 :initial-element 'a))
(def-fold-test make-sequence.fold.3
  (make-sequence 'bit-vector 5 :initial-element 0))
(def-fold-test make-sequence.fold.4
  (make-sequence 'string 5 :initial-element #\a))

;;; FIXME: Add tests for upgrading of character subtypes
