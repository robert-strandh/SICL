;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 18 10:10:04 2002
;;;; Contains: Tests for the MAP-INTO function

(in-package #:sicl-sequence-test)

(deftest map-into-list.1
  (let ((a (copy-seq '(a b c d e f)))
        (b nil))
    (map-into a #'(lambda (x) (push x b) x)  '(1 2 3 4 5 6))
    (values a b))
  (1 2 3 4 5 6)
  (6 5 4 3 2 1))

(deftest map-into-list.2
  (let ((a (copy-seq '(a b c d e f g))))
    (map-into a #'identity '(1 2 3))
    a)
  (1 2 3 d e f g))

(deftest map-into-list.3
  (let ((a (copy-seq '(a b c))))
    (map-into a #'identity '(1 2 3 4 5 6))
    a)
  (1 2 3))

(deftest map-into-list.4
  (let ((a (copy-seq '(a b c d e f)))
        (b nil))
    (map-into a #'(lambda (x y) (let ((z (+ x y))) (push z b) z))
              '(1 2 3 4 5 6)
              '(10 11 12 13 14 15))
    (values a b))
  (11 13 15 17 19 21)
  (21 19 17 15 13 11))

(deftest map-into-list.5
  (let ((a (copy-seq '(a b c d e f))))
    (map-into a 'identity '(1 2 3 4 5 6))
    a)
  (1 2 3 4 5 6))

(deftest map-into-list.6
  (let ((b nil))
    (values
     (map-into nil #'(lambda (x y) (let ((z (+ x y))) (push z b) z))
               '(1 2 3 4 5 6)
               '(10 11 12 13 14 15))
     b))
  nil nil)

(deftest map-into-list.7
  (let ((a (copy-seq '(a b c d e f))))
    (map-into a #'(lambda () 1))
    a)
  (1 1 1 1 1 1))

(deftest map-into-list.8
  (let ((a (copy-seq '(a b c d e f)))
        (s2 (make-array '(6) :initial-element 'x
                        :fill-pointer 4)))
    (map-into a #'identity s2)
    a)
  (x x x x e f))

(deftest map-into-array.1
  (let ((a (copy-seq #(a b c d e f)))
        b)
    (map-into a #'(lambda (x) (push x b) x) '(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6)
  (6 5 4 3 2 1))

(deftest map-into-array.2
  (let ((a (copy-seq #(a b c d e f g h)))
        b)
    (map-into a #'(lambda (x) (push x b) x) '(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6 g h)
  (6 5 4 3 2 1))

(deftest map-into-array.3
  (let ((a (copy-seq #(a b c d)))
        b)
    (map-into a #'(lambda (x) (push x b) x) '(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4)
  (4 3 2 1))

(deftest map-into-array.4
  (let ((a (copy-seq #(a b c d e f)))
        b)
    (map-into a #'(lambda (x) (push x b) x) #(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6)
  (6 5 4 3 2 1))

(deftest map-into-array.5
  (let ((a (copy-seq #(a b c d e f g h)))
        b)
    (map-into a #'(lambda (x) (push x b) x) #(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4 5 6 g h)
  (6 5 4 3 2 1))

(deftest map-into-array.6
  (let ((a (copy-seq #(a b c d)))
        b)
    (map-into a #'(lambda (x) (push x b) x) #(1 2 3 4 5 6))
    (values a b))
  #(1 2 3 4)
  (4 3 2 1))

;;; Tests of mapping into arrays with fill pointers
(deftest map-into-array.7
  (let ((a (make-array 6 :initial-element 'x
                       :fill-pointer 3)))
    (map-into a #'identity '(1 2 3))
    a)
  #(1 2 3))

(deftest map-into-array.8
  (let ((a (make-array 6 :initial-element 'x
                       :fill-pointer 3)))
    (map-into a #'identity '(1 2))
    a)
  #(1 2))

(deftest map-into-array.9
  (let ((a (make-array 6 :initial-element 'x
                       :fill-pointer 3)))
    (map-into a #'identity '(1 2 3 4 5))
    (and (eqlt (fill-pointer a) 5)
         a))
  #(1 2 3 4 5))

(deftest map-into-array.10
  (let ((a (make-array 6 :initial-element 'x
                       :fill-pointer 3)))
    (map-into a #'(lambda () 'y))
    (and (eqlt (fill-pointer a) 6)
         a))
  #(y y y y y y))

(deftest map-into-array.11
  (let ((a (copy-seq #(a b c d e f)))
        (s2 (make-array '(6) :initial-element 'x
                        :fill-pointer 4)))
    (map-into a #'identity s2)
    a)
  #(x x x x e f))

;;; mapping into strings

(deftest map-into-string.1
  (let ((a (copy-seq "abcdef")))
    (map-into a #'identity "123456")
    (values (not (not (stringp a))) a))
  t
  "123456")

(deftest map-into-string.2
  (let ((a (copy-seq "abcdef")))
    (map-into a #'identity "1234")
    (values (not (not (stringp a))) a))
  t
  "1234ef")

(deftest map-into-string.3
  (let ((a (copy-seq "abcd")))
    (map-into a #'identity "123456")
    (values (not (not (stringp a))) a))
  t
  "1234")

(deftest map-into-string.4
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'character
                       :fill-pointer 3)))
    (map-into a #'identity "abcde")
    (values
     (fill-pointer a)
     (aref a 5)
     a))
  5
  #\x
  "abcde")

(deftest map-into-string.5
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'character
                       :fill-pointer 3)))
    (map-into a #'(lambda () #\y))
    (values (fill-pointer a)
            a))
  6
  "yyyyyy")

(deftest map-into-string.6
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'character)))
    (map-into a #'(lambda () #\y))
    a)
  "yyyyyy")

(deftest map-into-string.7
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'base-char
                       :fill-pointer 3)))
    (map-into a #'identity "abcde")
    (values (fill-pointer a)
            (aref a 5)
            a))
  5
  #\x
  "abcde")

(deftest map-into-string.8
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'base-char
                       :fill-pointer 3)))
    (map-into a #'(lambda () #\y))
    (values (fill-pointer a)
            a))
  6
  "yyyyyy")

(deftest map-into-string.9
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'base-char)))
    (map-into a #'(lambda () #\y))
    a)
  "yyyyyy")

(deftest map-into-string.10
  (let ((a (copy-seq "abcdef"))
        (s2 (make-array '(6) :initial-element #\x
                        :fill-pointer 4)))
    (map-into a #'identity s2)
    a)
  "xxxxef")

(deftest map-into-string.11
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'character
                       :fill-pointer 3)))
    (map-into a #'identity "abcd")
    (values
     (fill-pointer a)
     (aref a 4)
     (aref a 5)
     a))
  4
  #\x
  #\x
  "abcd")

(deftest map-into-string.12
  (let ((a (make-array 6 :initial-element #\x
                       :element-type 'character
                       :fill-pointer 3)))
    (map-into a #'identity "abcdefgh")
    (values
     (fill-pointer a)
     a))
  6
  "abcdef")

(deftest map-into-string.13
  (do-special-strings
   (s (copy-seq "12345") nil)
   (let ((s2 (map-into s #'identity "abcde")))
     (assert (eq s s2))
     (assert (string= s2 "abcde"))))
  nil)

(deftest map-into-string.14
  (do-special-strings
   (s "abcde" nil)
   (let* ((s1 (copy-seq "123456"))
          (s2 (map-into s1 #'identity s)))
     (assert (eq s1 s2))
     (assert (string= s2 "abcde6"))))
  nil)

;;; Tests on bit vectors

(deftest map-into.bit-vector.1
  (let ((v (copy-seq #*0100110)))
    (map-into v #'(lambda (x) (- 1 x)) v)
    (and (bit-vector-p v)
         v))
  #*1011001)

(deftest map-into.bit-vector.2
  (let ((v (copy-seq #*0100110)))
    (map-into v #'(lambda () 0))
    (and (bit-vector-p v)
         v))
  #*0000000)

(deftest map-into.bit-vector.3
  (let ((v (copy-seq #*0100110)))
    (map-into v #'identity '(0 1 1 1 0 0 1))
    (and (bit-vector-p v)
         v))
  #*0111001)

(deftest map-into.bit-vector.4
  (let ((v (copy-seq #*0100110)))
    (map-into v #'identity '(0 1 1 1))
    (and (bit-vector-p v)
         v))
  #*0111110)

(deftest map-into.bit-vector.5
  (let ((v (copy-seq #*0100110)))
    (map-into v #'identity '(0 1 1 1 0 0 1 4 5 6 7))
    (and (bit-vector-p v)
         v))
  #*0111001)

(deftest map-into.bit-vector.6
  (let ((v (make-array '(8) :initial-contents '(0 1 0 0 1 1 0 1)
                       :fill-pointer 4
                       :element-type 'bit)))
    (map-into v #'(lambda () 1))
    (and (bit-vector-p v)
         v))
  #*11111111)

(deftest map-into.bit-vector.7
  (let ((v (make-array '(8) :initial-contents '(0 1 0 0 1 1 0 1)
                       :fill-pointer 4
                       :element-type 'bit)))
    (map-into v  #'identity v)
    (and (bit-vector-p v)
         v))
  #*0100)

(deftest map-into.bit-vector.8
  (let ((v (make-array '(8) :initial-contents '(0 1 0 0 1 1 0 1)
                       :fill-pointer 4
                       :element-type 'bit)))
    (map-into v #'identity '(1 1 1 1 1 1))
    (and (bit-vector-p v)
         (values (fill-pointer v)
                 v)))
  6
  #*111111)

(deftest map-into.bit-vector.9
  (let ((v (make-array '(8) :initial-contents '(0 1 0 0 1 1 0 1)
                       :fill-pointer 4
                       :element-type 'bit)))
    (map-into v #'identity '(1 1 1 1 1 1 0 0 1 1 1))
    (and (bit-vector-p v)
         (values (fill-pointer v)
                 v)))
  8
  #*11111100)

;;; Other specialized vectors

(deftest map-into.specialized-vector.1
  (do-special-integer-vectors
   (v #(1 2 3 4) nil)
   (let ((result (list nil nil nil nil)))
     (assert (eq (map-into result #'identity v) result))
     (assert (equal result '(1 2 3 4)))))
  nil)

(deftest map-into.specialized-vector.2
  (do-special-integer-vectors
   (v #(1 2 3) nil)
   (let ((result (list nil nil nil nil)))
     (assert (eq (map-into result #'identity v) result))
     (assert (equal result '(1 2 3 nil)))))
  nil)

(deftest map-into.specialized-vector.3
  (do-special-integer-vectors
   (v #(1 1 0 1 1) nil)
   (let ((result (list nil nil nil nil)))
     (assert (eq (map-into result #'identity v) result))
     (assert (equal result '(1 1 0 1)))))
  nil)

(deftest map-into.specialized-vector.4
  (do-special-integer-vectors
   (v #(1 2 1 2 2) nil)
   (let ((v2 #(2 1 2 2 1)))
     (assert (eq (map-into v #'identity v2) v))
     (assert (equalp v #(2 1 2 2 1)))))
  nil)

(deftest map-into.specialized-vector.5
  (let ((len 10))
    (loop for etype in '(short-float single-float double-float long-float)
          for vals = (loop for i below len collect (coerce i etype))
          for vec = (make-array len :initial-contents vals :element-type etype)
          for target = (loop repeat len collect nil)
          for result = (map-into target #'identity vec)
          unless (and (eq target result)
                      (= (length result) len)
                      (= (length vec) len)
                      (equal vals result))
          collect (list etype vals vec result)))
  nil)

(deftest map-into.specialized-vector.6
  (let ((len 10))
    (loop for cetype in '(short-float single-float double-float long-float)
          for etype = `(complex ,cetype)
          for vals = (loop for i from 1 to len collect (complex (coerce i cetype)
                                                                (coerce (- i) cetype)))
          for vec = (make-array len :initial-contents vals :element-type etype)
          for target = (loop repeat len collect nil)
          for result = (map-into target #'identity vec)
          unless (and (eq target result)
                      (= (length result) len)
                      (= (length vec) len)
                      (equal vals result))
          collect (list etype vals vec result)))
  nil)

(deftest map-into.specialized-vector.7
  (let ((len 10))
    (loop for etype in '(short-float single-float double-float long-float)
          for vals = (loop for i below len collect (coerce i etype))
          for target = (make-array len :initial-contents vals :element-type etype)
          for result = (map-into target #'identity vals)
          unless (and (eq target result)
                      (= (length result) len)
                      (every #'= result vals))
          collect (list etype vals result)))
  nil)

(deftest map-into.specialized-vector.8
  (let ((len 10))
    (loop for cetype in '(short-float single-float double-float long-float)
          for etype = `(complex ,cetype)
          for vals = (loop for i from 1 to len collect (complex (coerce i cetype)
                                                                (coerce (- i) cetype)))
          for target = (make-array len :initial-contents vals :element-type etype)
          for result = (map-into target #'identity vals)
          unless (and (eq target result)
                      (= (length result) len)
                      (every #'= result vals))
          collect (list etype vals result)))
  nil)

;;; Error cases

(deftest map-into.error.1
  (check-type-error #'(lambda (x) (map-into x (constantly nil))) #'sequencep)
  nil)

;;; The next test was changed because if the first argument
;;; is NIL, map-into is said to 'return nil immediately', so
;;; the 'should be prepared' notation for the error checking
;;; means that error checking may be skipped.
(deftest map-into.error.2
  (and (locally (declare (optimize (safety 3)))
                (handler-case (eval '(map-into nil #'identity 'a))
                              (type-error () nil)))
       :bad)
  nil)

(deftest map-into.error.3
  (check-type-error #'(lambda (x) (map-into (copy-seq '(a b c)) #'cons '(d e f) x))
                    #'sequencep)
  nil)

(deftest map-into.error.4
  (signals-error (map-into) program-error)
  t)

(deftest map-into.error.5
  (signals-error (map-into (list 'a 'b 'c)) program-error)
  t)

(deftest map-into.error.6
  (signals-error (locally (map-into 'a #'(lambda () nil)) t)
                 type-error)
  t)

(deftest map-into.error.7
  (signals-error (map-into (list 'a 'b 'c) #'cons '(a b c)) program-error)
  t)

(deftest map-into.error.8
  (signals-error (map-into (list 'a 'b 'c) #'car '(a b c)) type-error)
  t)

;;; Order of evaluation tests

(deftest map-into.order.1
  (let ((i 0) a b c)
    (values
     (map-into (progn (setf a (incf i)) (list 1 2 3 4))
               (progn (setf b (incf i)) #'identity)
               (progn (setf c (incf i)) '(a b c d)))
     i a b c))
  (a b c d) 3 1 2 3)

(deftest map-into.order.2
  (let ((i 0) a b c d)
    (values
     (map-into (progn (setf a (incf i)) (list 1 2 3 4))
               (progn (setf b (incf i)) #'list)
               (progn (setf c (incf i)) '(a b c d))
               (progn (setf d (incf i)) '(e f g h)))
     i a b c d))
  ((a e) (b f) (c g) (d h)) 4 1 2 3 4)
