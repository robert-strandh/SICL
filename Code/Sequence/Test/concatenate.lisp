;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Sep  4 22:53:51 2002
;;;; Contains: Tests for CONCATENATE

(in-package #:sicl-sequence-test)

(deftest concatenate.1
  (concatenate 'list)
  nil)

(deftest concatenate.2
  (let* ((orig (list 'a 'b 'c 'd 'e))
         (copy (concatenate 'list orig)))
    (values
     copy
     (intersection (loop for e on orig collect e)
                   (loop for e on copy collect e)
                   :test #'eq)))
  (a b c d e)
  nil)

(deftest concatenate.3
  (concatenate 'list "")
  nil)

(deftest concatenate.4
  (concatenate 'list "abcd" '(x y z) nil #*1101 #())
  (#\a #\b #\c #\d x y z 1 1 0 1))

(deftest concatenate.5
  (concatenate 'vector)
  #())

(deftest concatenate.6
  (concatenate 'vector nil "abcd" '(x y z) nil #*1101 #())
  #(#\a #\b #\c #\d x y z 1 1 0 1))

(deftest concatenate.7
  (let* ((orig (vector 'a 'b 'c 'd 'e))
         (copy (concatenate 'vector orig)))
    (values
     copy
     (eqt copy orig)))
  #(a b c d e)
  nil)

(deftest concatenate.8
  (concatenate 'simple-vector '(a b c) #(1 2 3))
  #(a b c 1 2 3))

(deftest concatenate.9
  (concatenate 'simple-vector)
  #())

(deftest concatenate.10
  (concatenate 'bit-vector nil)
  #*)

(deftest concatenate.11
  (concatenate 'bit-vector)
  #*)

(deftest concatenate.12
  (concatenate 'bit-vector '(0 1 1) nil #(1 0 1) #())
  #*011101)

(deftest concatenate.13
  (concatenate 'simple-bit-vector nil)
  #*)

(deftest concatenate.14
  (concatenate 'simple-bit-vector)
  #*)

(deftest concatenate.15
  (concatenate 'simple-bit-vector '(0 1 1) nil #(1 0 1) #())
  #*011101)

(deftest concatenate.16
  (concatenate 'string "abc" '(#\d #\e) nil #() "fg")
  "abcdefg")

(deftest concatenate.17
  (concatenate 'simple-string "abc" '(#\d #\e) nil #() "fg")
  "abcdefg")

(deftest concatenate.18
  (concatenate '(vector * *) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.18a
  (concatenate '(vector *) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.18b
  (concatenate '(vector) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.18c
  (concatenate '(simple-vector *) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.18d
  (concatenate '(simple-vector) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.19
  (concatenate '(vector * 8) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.20
  (concatenate '(vector symbol 8) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.21
  (concatenate '(vector symbol) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.22
  (concatenate '(vector symbol *) '(a b c) '(d e f) #(g h))
  #(a b c d e f g h))

(deftest concatenate.23
  (concatenate 'cons '(a b c) '(d e f))
  (a b c d e f))

(deftest concatenate.24
  (concatenate 'null nil nil)
  nil)

;;; Tests on vectors with fill pointers

(deftest concatenate.25
  (let ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)
                       :fill-pointer 5)))
    (concatenate 'list x x))
  (a b c d e a b c d e))

(deftest concatenate.26
  (let ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)
                       :fill-pointer 5)))
    (concatenate 'list x))
  (a b c d e))

(deftest concatenate.27
  (let* ((x (make-array '(10) :initial-contents '(a b c d e f g h i j)
                       :fill-pointer 5))
         (result (concatenate 'vector x)))
    (values (not (simple-vector-p result))
            result))
  nil
  #(a b c d e))

(deftest concatenate.28
  (let* ((x (make-array '(10) :initial-contents "abcdefghij"
                        :fill-pointer 5 :element-type 'character)))
    (values
     (concatenate 'string x '(#\z))
     (concatenate 'string '(#\z) x)
     (concatenate 'string x x)
     (concatenate 'string x)
     (not (simple-string-p (concatenate 'string x)))
     ))
  "abcdez"
  "zabcde"
  "abcdeabcde"
  "abcde"
  nil)

(deftest concatenate.29
  (let* ((x (make-array '(10) :initial-contents "abcdefghij"
                        :fill-pointer 5 :element-type 'base-char)))
    (values
     (concatenate 'string x '(#\z))
     (concatenate 'string '(#\z) x)
     (concatenate 'string x x)
     (concatenate 'string x)
     (not (simple-string-p (concatenate 'string x)))
     ))
  "abcdez"
  "zabcde"
  "abcdeabcde"
  "abcde"
  nil)

(deftest concatenate.30
  (let* ((x (make-array '(10) :initial-contents #*0110010111
                        :fill-pointer 5 :element-type 'bit)))
    (values
     (concatenate 'bit-vector x '(0))
     (concatenate '(bit-vector) '(0) x)
     (concatenate '(bit-vector 10) x x)
     (concatenate '(bit-vector *) x)
     (not (simple-bit-vector-p (concatenate 'bit-vector x)))
     ))
  #*011000
  #*001100
  #*0110001100
  #*01100
  nil)

(deftest concatenate.30a
  (let* ((x (make-array '(10) :initial-contents #*0110010111
                        :fill-pointer 5 :element-type 'bit)))
    (values
     (concatenate 'simple-bit-vector x '(0))
     (concatenate 'simple-bit-vector '(0) x)
     (concatenate 'simple-bit-vector x x)
     (concatenate 'simple-bit-vector x)
     (not (simple-bit-vector-p (concatenate 'bit-vector x)))
     ))
  #*011000
  #*001100
  #*0110001100
  #*01100
  nil)

(deftest concatenate.31
  :notes (:nil-vectors-are-strings)
  (concatenate 'string "abc" (make-array '(0) :element-type nil) "def")
  "abcdef")

(deftest concatenate.32
  :notes (:nil-vectors-are-strings)
  (concatenate '(array nil (*)))
  "")

(deftest concatenate.33
  (do-special-strings
   (s "abc" nil)
   (assert (string= (concatenate 'string s s s) "abcabcabc"))
   (assert (string= (concatenate 'string "xy" s) "xyabc"))
   (assert (string= (concatenate 'simple-string s "z" s "w" s) "abczabcwabc"))
   (assert (string= (concatenate 'base-string s "z" s "w" s) "abczabcwabc"))
   (assert (string= (concatenate 'simple-base-string s "z" s "w" s) "abczabcwabc"))
   (assert (string= (concatenate '(vector character) s "z" s "w" s) "abczabcwabc")))
  nil)

(deftest concatenate.34
  (concatenate 'simple-string "abc" "def")
  "abcdef")

(deftest concatenate.35
  (concatenate '(simple-string) "abc" "def")
  "abcdef")

(deftest concatenate.36
  (concatenate '(simple-string *) "abc" "def")
  "abcdef")

(deftest concatenate.37
  (concatenate '(simple-string 6) "abc" "def")
  "abcdef")

(deftest concatenate.38
  (concatenate '(string) "abc" "def")
  "abcdef")

(deftest concatenate.39
  (concatenate '(string *) "abc" "def")
  "abcdef")

(deftest concatenate.40
  (concatenate '(string 6) "abc" "def")
  "abcdef")

;;; Order of evaluation tests

(deftest concatenate.order.1
  (let ((i 0) w x y z)
    (values
     (concatenate (progn (setf w (incf i)) 'string)
                  (progn (setf x (incf i)) "abc")
                  (progn (setf y (incf i)) "def")
                  (progn (setf z (incf i)) "ghi"))
     i w x y z))
  "abcdefghi" 4 1 2 3 4)

(deftest concatenate.order.2
  (let ((i 0) x y z)
    (values
     (concatenate 'string
                  (progn (setf x (incf i)) "abc")
                  (progn (setf y (incf i)) "def")
                  (progn (setf z (incf i)) "ghi"))
     i x y z))
  "abcdefghi" 3 1 2 3)

;;; Constant folding tests

(def-fold-test concatenate.fold.1 (concatenate 'list '(a b) '(c d)))
(def-fold-test concatenate.fold.2 (concatenate 'vector '(a b) '(c d)))
(def-fold-test concatenate.fold.3 (concatenate 'bit-vector '(0 0) '(1 0 1)))
(def-fold-test concatenate.fold.4 (concatenate 'string "ab" "cd"))
(def-fold-test concatenate.fold.5 (concatenate 'list '(a b c d)))
(def-fold-test concatenate.fold.6 (concatenate 'vector #(a b c d)))
(def-fold-test concatenate.fold.7 (concatenate 'bit-vector #*110101101))
(def-fold-test concatenate.fold.8 (concatenate 'string "abcdef"))

;;; Error tests

(deftest concatenate.error.1
  (signals-error (concatenate 'sequence '(a b c)) error)
  t)

(deftest concatenate.error.2
  (signals-error-always (concatenate 'fixnum '(a b c d e)) error)
  t t)

(deftest concatenate.error.3
  (signals-error (concatenate '(vector * 3) '(a b c d e))
                 type-error)
  t)

(deftest concatenate.error.4
  (signals-error (concatenate) program-error)
  t)

(deftest concatenate.error.5
  (signals-error (locally (concatenate '(vector * 3) '(a b c d e)) t)
                 type-error)
  t)

(deftest concatenate.error.6
  :notes (:result-type-element-type-by-subtype)
  (let ((type '(or (vector bit) (vector t))))
    (if (subtypep type 'vector)
        (eval `(signals-error-always (concatenate ',type '(0 1 0) '(1 1 0)) error))
      (values t t)))
  t t)

