;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 12 19:38:29 2002
;;;; Contains: Tests of ELT

(in-package #:sicl-sequence-test)

(declaim (optimize (safety 3)))

;; elt on lists

(deftest elt.1
  (signals-error (elt nil  0) type-error)
  t)

(deftest elt.1a
  (signals-error (elt nil -10) type-error)
  t)

(deftest elt.1b
  (signals-error (locally (elt nil 0) t) type-error)
  t)

(deftest elt.2
  (signals-error (elt nil 1000000) type-error)
  t)

(deftest elt.3 (elt '(a b c d e) 0) a)
(deftest elt.4 (elt '(a b c d e) 2) c)
(deftest elt.5 (elt '(a b c d e) 4) e)
(deftest elt.5a
  (signals-error (elt '(a b c d e) -4) type-error)
  t)

(deftest elt.6
  (let ((x (make-int-list 1000)))
    (notnot-mv
     (every
      #'(lambda (i) (eql i (elt x i)))
      x)))
  t)

(deftest elt.7
  (let* ((x (list 'a 'b 'c 'd))
         (y (setf (elt x 0) 'e)))
    (list x y))
  ((e b c d) e))

(deftest elt.8
  (let* ((x (list 'a 'b 'c 'd))
         (y (setf (elt x 1) 'e)))
    (list x y))
  ((a e c d) e))

(deftest elt.9
  (let* ((x (list 'a 'b 'c 'd))
         (y (setf (elt x 3) 'e)))
    (list x y))
  ((a b c e) e))

(deftest elt.10
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (setf (elt x 4) 'd))
   type-error)
  t)

(deftest elt.11
  (let ((x (list 'a 'b 'c 'd 'e)))
    (let ((y (loop for c on x collect c)))
      (setf (elt x 2) 'f)
      (notnot-mv
       (every #'eq
              y
              (loop for c on x collect c)))))
  t)

(deftest elt.12
  (let ((x (make-int-list 100000)))
    (elt x 90000))
  90000)

(deftest elt.13
  (let ((x (make-int-list 100000)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
          (elt x 80000)
          (elt x 80001)))
  (79999 foo 80001))

(deftest elt.14
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (elt x 10))
   type-error)
  t)

(deftest elt.15
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (elt x 'a))
   type-error)
  t)

(deftest elt.16
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (elt x 10.0))
   type-error)
  t)

(deftest elt.17
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (elt x -1))
   type-error)
  t)

(deftest elt.18
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (elt x -100000000000000000))
   type-error)
  t)

(deftest elt.19
  (signals-error
   (let ((x (list 'a 'b 'c)))
     (elt x #\w))
   type-error)
  t)

(deftest elt.order.1
  (let ((i 0) x y)
    (values
     (elt (progn (setf x (incf i)) '(a b c d e))
          (progn (setf y (incf i)) 3))
     i x y))
  d 2 1 2)

(deftest elt.order.2
  (let ((i 0) x y z)
    (let ((a (make-array 1 :initial-element (list 'a 'b 'c 'd 'e))))
      (values
       (setf (elt (aref a (progn (setf x (incf i)) 0))
                  (progn (setf y (incf i)) 3))
             (progn (setf z (incf i)) 'k))
       (aref a 0)
       i x y z)))
  k (a b c k e) 3 1 2 3)

(deftest elt-v.1
  (signals-error (elt (make-array '(0)) 0) type-error)
  t)

;; (deftest elt-v.2 (elt (make-array '(1)) 0) nil)  ;; actually undefined
(deftest elt-v.3
  (elt (make-array '(5) :initial-contents '(a b c d e)) 0)
  a)

(deftest elt-v.4
  (elt (make-array '(5) :initial-contents '(a b c d e)) 2)
  c)

(deftest elt-v.5
  (elt (make-array '(5) :initial-contents '(a b c d e)) 4)
  e)

(deftest elt-v.6
    (elt-v-6-body)
  t)

(deftest elt-v.7
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
         (y (setf (elt x 0) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (e b c d e))

(deftest elt-v.8
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
         (y (setf (elt x 1) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a e c d e))

(deftest elt-v.9
  (let* ((x (make-array '(4) :initial-contents (list 'a 'b 'c 'd)))
         (y (setf (elt x 3) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a b c e e))

(deftest elt-v.10
  (signals-error
   (let ((x (make-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x 4) 'd))
   type-error)
  t)

(deftest elt-v.11
  (signals-error
   (let ((x (make-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x -100) 'd))
   type-error)
  t)

(deftest elt-v.12
    (let ((x (make-int-array 100000)))
      (elt x 90000))
  90000)

(deftest elt-v.13
  (let ((x (make-int-array 100000)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
          (elt x 80000)
          (elt x 80001)))
  (79999 foo 80001))

;;;  Adjustable arrays

(deftest elt-adj-array.1
  (signals-error (elt (make-adj-array '(0)) 0) type-error)
  t)

;;; (deftest elt-adj-array.2 (elt (make-adj-array '(1)) 0) nil) ;; actually undefined

(deftest elt-adj-array.3
 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 0)
  a)

(deftest elt-adj-array.4
 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 2)
  c)

(deftest elt-adj-array.5
 (elt (make-adj-array '(5) :initial-contents '(a b c d e)) 4)
  e)

(deftest elt-adj-array.6
    (elt-adj-array-6-body)
  t)

(deftest elt-adj-array.7
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
         (y (setf (elt x 0) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (e b c d e))

(deftest elt-adj-array.8
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
         (y (setf (elt x 1) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a e c d e))

(deftest elt-adj-array.9
  (let* ((x (make-adj-array '(4) :initial-contents (list 'a 'b 'c 'd)))
         (y (setf (elt x 3) 'e)))
    (list (elt x 0) (elt x 1) (elt x 2) (elt x 3) y))
  (a b c e e))

(deftest elt-adj-array.10
  (signals-error
   (let ((x (make-adj-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x 4) 'd))
   type-error)
  t)

(deftest elt-adj-array.11
  (signals-error
   (let ((x (make-adj-array '(3) :initial-contents (list 'a 'b 'c))))
     (setf (elt x -100) 'd))
   type-error)
  t)

(deftest elt-adj-array.12
    (let ((x (make-int-array 100000 #'make-adj-array)))
      (elt x 90000))
  90000)

(deftest elt-adj-array.13
    (let ((x (make-int-array 100000 #'make-adj-array)))
    (setf (elt x 80000) 'foo)
    (list (elt x 79999)
          (elt x 80000)
          (elt x 80001)))
  (79999 foo 80001))

;; displaced arrays

(deftest elt-displaced-array.1
  (signals-error (elt (make-displaced-array '(0) 100) 0) type-error)
  t)

(deftest elt-displaced-array.2
  (elt (make-displaced-array '(1) 100) 0)
  100)

(deftest elt-displaced-array.3
  (elt (make-displaced-array '(5) 100) 4)
  104)

;;; Arrays with fill points

(deftest elt-fill-pointer.1
  (let ((a (make-array '(5) :initial-contents '(a b c d e)
                       :fill-pointer 3)))
    (values (elt a 0) (elt a 1) (elt a 2)))
  a b c)

(deftest elt-fill-pointer.2
  (let ((a (make-array '(5)
                       :initial-contents '(0 0 1 0 0)
                       :element-type 'bit
                       :fill-pointer 3)))
    (values (elt a 0) (elt a 1) (elt a 2)))
  0 0 1)

(deftest elt-fill-pointer.3
  (signals-error
   (let ((a (make-array '(5)
                        :initial-contents '(0 0 1 0 0)
                        :fill-pointer 3)))
     (elt a 4))
   type-error)
  t)

(deftest elt-fill-pointer.4
  (signals-error
   (let ((a (make-array '(5)
                        :initial-contents '(0 0 1 0 0)
                        :element-type 'bit
                        :fill-pointer 3)))
     (elt a 4))
   type-error)
  t)

(deftest elt-fill-pointer.5
   (let ((a (make-array '(5)
                        :initial-contents '(#\a #\b #\c #\d #\e)
                        :element-type 'character
                        :fill-pointer 3)))
     (values (elt a 0) (elt a 1) (elt a 2)))
   #\a #\b #\c)

(deftest elt-fill-pointer.6
  (signals-error
   (let ((a (make-array '(5)
                        :initial-contents '(#\a #\b #\c #\d #\e)
                        :element-type 'character
                        :fill-pointer 3)))
     (elt a 4))
   type-error)
  t)

(deftest elt-fill-pointer.7
   (let ((a (make-array '(5)
                        :initial-contents '(#\a #\b #\c #\d #\e)
                        :element-type 'base-char
                        :fill-pointer 3)))
     (values (elt a 0) (elt a 1) (elt a 2)))
   #\a #\b #\c)

(deftest elt-fill-pointer.8
  (signals-error
   (let ((a (make-array '(5)
                        :initial-contents '(#\a #\b #\c #\d #\e)
                        :element-type 'base-char
                        :fill-pointer 3)))
     (elt a 4))
   type-error)
  t)

;;; Specialized strings

(deftest elt.special-strings.1
  (do-special-strings
   (s "abcde" nil)
   (assert (char= (elt s 0) #\a))
   (assert (char= (elt s 3) #\d))
   (assert (char= (elt s 4) #\e)))
  nil)

;;; Specialized integer vectors

(deftest elt.special-vectors.1
  (do-special-integer-vectors
   (v #(1 1 0 1 0 1) nil)
   (assert (= (elt v 0) 1))
   (assert (= (elt v 1) 1))
   (assert (= (elt v 2) 0))
   (assert (= (elt v 3) 1))
   (assert (= (elt v 4) 0))
   (assert (= (elt v 5) 1)))
  nil)

(deftest elt.special-vectors.2
  (do-special-integer-vectors
   (v #(1 2 0 -1 0 3) nil)
   (assert (= (elt v 0) 1))
   (assert (= (elt v 1) 2))
   (assert (= (elt v 2) 0))
   (assert (= (elt v 3) -1))
   (assert (= (elt v 4) 0))
   (assert (= (elt v 5) 3)))
  nil)

(deftest elt.special-vectors.3
  (loop for type in '(short-float single-float long-float double-float)
        for len = 10
        for vals = (loop for i from 1 to len collect (coerce i type))
        for vec = (make-array len :element-type type :initial-contents vals)
        unless (loop for i below len always (eql (elt vec i)
                                                 (coerce (1+ i) type)))
        collect (list type vals vec))
  nil)

(deftest elt.special-vectors.4
  (loop for etype in '(short-float single-float long-float double-float
                                   integer rational)
        for type = `(complex ,etype)
        for len = 10
        for vals = (loop for i from 1 to len collect (complex (coerce i etype)
                                                              (coerce (- i) etype)))
        for vec = (make-array len :element-type type :initial-contents vals)
        unless (loop for i below len always (eql (elt vec i)
                                                 (elt vals i)))
        collect (list type vals vec))
  nil)



;;; Error tests

(deftest elt.error.1
  (signals-error (elt) program-error)
  t)

(deftest elt.error.2
  (signals-error (elt nil) program-error)
  t)

(deftest elt.error.3
  (signals-error (elt nil 0 nil) program-error)
  t)

(deftest elt.error.4
  (do-special-integer-vectors
   (v #(1 1 0 1 0 1) nil)
   (assert (eql t (eval `(signals-error (elt ,v -1) type-error))))
   (assert (eql t (eval `(signals-error (elt ,v 6) type-error)))))
  nil)

(deftest elt.error.5
  (do-special-strings
   (s "ABCDEFGH" nil)
   (assert (eql t (eval `(signals-error (elt ,s -1) type-error))))
   (assert (eql t (eval `(signals-error (elt ,s 8) type-error)))))
  nil)
