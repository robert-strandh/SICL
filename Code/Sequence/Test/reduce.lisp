;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 18 14:08:57 2002
;;;; Contains: Tests for function REDUCE

(in-package #:sicl-sequence-test)

(deftest reduce-list.1
  (reduce #'cons '(a b c d e f))
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-list.2
  (reduce #'cons '(a b c d e f) :from-end t)
  (a b c d e . f))

(deftest reduce-list.3
  (reduce #'cons '(a b c d e f) :initial-value 'z)
  ((((((z . a) . b) . c) . d) . e) . f))

(deftest reduce-list.4
  (reduce #'cons '(a b c d e f) :from-end t :initial-value 'g)
  (a b c d e f . g))

(deftest reduce-list.5
  (reduce #'cons '(a b c d e f) :from-end nil)
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-list.6
  (reduce #'cons '(a b c d e f) :from-end 17)
  (a b c d e . f))

(deftest reduce-list.7
  (reduce #'cons '(a b c d e f) :end nil)
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-list.8
  (reduce #'cons '(a b c d e f) :end 3)
  ((a . b) . c))

(deftest reduce-list.9
  (reduce #'cons '(a b c d e f) :start 1 :end 4)
  ((b . c) . d))

(deftest reduce-list.10
  (reduce #'cons '(a b c d e f) :start 1 :end 4 :from-end t)
  (b c . d))

(deftest reduce-list.11
  (reduce #'cons '(a b c d e f) :start 1 :end 4 :from-end t
          :initial-value nil)
  (b c d))

(deftest reduce-list.12
  (reduce 'cons '(a b c d e f))
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-list.13
  (reduce #'+ nil)
  0)

(deftest reduce-list.14
  (reduce #'+ '(1 2 3) :start 0 :end 0)
  0)

(deftest reduce-list.15
  (reduce #'+ '(1 2 3) :key '1+)
  9)

(deftest reduce-list.16
  (reduce #'cons '(1 2 3) :key '1+ :from-end t :initial-value nil)
  (2 3 4))

(deftest reduce-list.17
  (reduce #'+ '(1 2 3 4 5 6 7) :key '1+ :start 2 :end 6)
  22)

;;;;;;;

(deftest reduce-array.1
  (reduce #'cons #(a b c d e f))
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-array.2
  (reduce #'cons #(a b c d e f) :from-end t)
  (a b c d e . f))

(deftest reduce-array.3
  (reduce #'cons #(a b c d e f) :initial-value 'z)
  ((((((z . a) . b) . c) . d) . e) . f))

(deftest reduce-array.4
  (reduce #'cons #(a b c d e f) :from-end t :initial-value 'g)
  (a b c d e f . g))

(deftest reduce-array.5
  (reduce #'cons #(a b c d e f) :from-end nil)
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-array.6
  (reduce #'cons #(a b c d e f) :from-end 17)
  (a b c d e . f))

(deftest reduce-array.7
  (reduce #'cons #(a b c d e f) :end nil)
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-array.8
  (reduce #'cons #(a b c d e f) :end 3)
  ((a . b) . c))

(deftest reduce-array.9
  (reduce #'cons #(a b c d e f) :start 1 :end 4)
  ((b . c) . d))

(deftest reduce-array.10
  (reduce #'cons #(a b c d e f) :start 1 :end 4 :from-end t)
  (b c . d))

(deftest reduce-array.11
  (reduce #'cons #(a b c d e f) :start 1 :end 4 :from-end t
          :initial-value nil)
  (b c d))

(deftest reduce-array.12
  (reduce 'cons #(a b c d e f))
  (((((a . b) . c) . d) . e) . f))

(deftest reduce-array.13
  (reduce #'+ #(1 2 3) :start 0 :end 0)
  0)

(deftest reduce-array.14
  (let ((a (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8)
                       :fill-pointer 4)))
    (reduce #'+ a))
  10)

(deftest reduce-array.15
  (let ((a (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8)
                       :fill-pointer 4)))
    (reduce #'+ a :end nil))
  10)

(deftest reduce-array.16
  (let ((a (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8)
                       :fill-pointer 4)))
    (reduce #'+ a :from-end t))
  10)

(deftest reduce-array.17
  (let ((a (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8)
                       :fill-pointer 4)))
    (reduce #'+ a :initial-value 1))
  11)

(deftest reduce-array.18
  (let ((a (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8)
                       :fill-pointer 4)))
    (reduce #'+ a :initial-value 1 :start 2))
  8)

(deftest reduce-array.19
  (let ((a (make-array '(8) :initial-contents '(1 2 3 4 5 6 7 8)
                       :fill-pointer 4)))
    (reduce #'+ a :end 3))
  6)

;;; Specialized vectors

(deftest reduce-array.20
  (do-special-integer-vectors
   (v #(1 0 0 1 1 0) nil)
   (assert (eql (reduce #'+ v) 3)))
  nil)

(deftest reduce-array.21
  (do-special-integer-vectors
   (v #(1 0 0 1 1 0) nil)
   (assert (equal (reduce #'cons v :from-end t :initial-value nil)
                  '(1 0 0 1 1 0))))
  nil)

(deftest reduce-array.22
  (do-special-integer-vectors
   (v #(1 2 3 4 5 6 7) nil)
   (assert (eql (reduce #'+ v) 28))
   (assert (eql (reduce #'+ v :from-end t) 28))
   (assert (eql (reduce #'+ v :start 1) 27))
   (assert (eql (reduce #'+ v :initial-value 10) 38))
   (assert (eql (reduce #'+ v :end 6) 21)))
  nil)

(deftest reduce-array.23
  (let* ((len 10)
         (expected (* 1/2 (1+ len) len)))
    (loop for etype in '(short-float single-float double-float long-float)
          for vals = (loop for i from 1 to len collect (coerce i etype))
          for vec = (make-array len :initial-contents vals :element-type etype)
          for result = (reduce #'+ vec)
          unless (= result (coerce expected etype))
          collect (list etype vals vec result)))
  nil)

(deftest reduce-array.24
  (let* ((len 10)
         (expected (* 1/2 (1+ len) len)))
    (loop for cetype in '(short-float single-float double-float long-float)
          for etype = `(complex ,cetype)
          for vals = (loop for i from 1 to len collect (complex (coerce i cetype)
                                                                (coerce (- i) cetype)))
          for vec = (make-array len :initial-contents vals :element-type etype)
          for result = (reduce #'+ vec)
          unless (= result (complex (coerce expected cetype) (coerce (- expected) cetype)))
          collect (list etype vals vec result)))
  nil)

(deftest reduce-array.25
  (do-special-integer-vectors
   (v (vector 0 most-positive-fixnum 0 most-positive-fixnum 0) nil)
   (assert (eql (reduce #'+ v) (* 2 most-positive-fixnum))))
  nil)

;;;;;;;;

(deftest reduce.error.1
  (check-type-error #'(lambda (x) (reduce 'cons x)) #'sequencep)
  nil)

(deftest reduce.error.2
  (signals-error (reduce) program-error)
  t)

(deftest reduce.error.3
  (signals-error (reduce #'list nil :start) program-error)
  t)

(deftest reduce.error.4
  (signals-error (reduce #'list nil 'bad t) program-error)
  t)

(deftest reduce.error.5
  (signals-error (reduce #'list nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest reduce.error.6
  (signals-error (reduce #'list nil 1 2) program-error)
  t)

(deftest reduce.error.7
  (signals-error (locally (reduce 'cons 'a) t) type-error)
  t)

(deftest reduce.error.8
  (signals-error (reduce #'identity '(a b c)) program-error)
  t)

(deftest reduce.error.9
  (signals-error (reduce #'cons '(a b c) :key #'cons) program-error)
  t)

(deftest reduce.error.10
  (signals-error (reduce #'cons '(a b c) :key #'car) type-error)
  t)


;;;;;;;;

(deftest reduce-string.1
  (reduce #'cons "abcdef")
  (((((#\a . #\b) . #\c) . #\d) . #\e) . #\f))

(deftest reduce-string.2
  (reduce #'cons "abcdef" :from-end t)
  (#\a #\b #\c #\d #\e . #\f))

(deftest reduce-string.3
  (reduce #'cons "abcdef" :initial-value 'z)
  ((((((z . #\a) . #\b) . #\c) . #\d) . #\e) . #\f))

(deftest reduce-string.4
  (reduce #'cons "abcdef" :from-end t :initial-value 'g)
  (#\a #\b #\c #\d #\e #\f . g))

(deftest reduce-string.5
  (reduce #'cons "abcdef" :from-end nil)
   (((((#\a . #\b) . #\c) . #\d) . #\e) . #\f))

(deftest reduce-string.6
  (reduce #'cons "abcdef" :from-end 17)
   (#\a #\b #\c #\d #\e . #\f))

(deftest reduce-string.7
  (reduce #'cons "abcdef" :end nil)
  (((((#\a . #\b) . #\c) . #\d) . #\e) . #\f))

(deftest reduce-string.8
  (reduce #'cons "abcdef" :end 3)
  ((#\a . #\b) . #\c))

(deftest reduce-string.9
  (reduce #'cons "abcdef" :start 1 :end 4)
  ((#\b . #\c) . #\d))

(deftest reduce-string.10
  (reduce #'cons "abcdef" :start 1 :end 4 :from-end t)
  (#\b #\c . #\d))

(deftest reduce-string.11
  (reduce #'cons "abcdef" :start 1 :end 4 :from-end t
          :initial-value nil)
  (#\b #\c #\d))

(deftest reduce-string.12
  (reduce 'cons "abcdef")
  (((((#\a . #\b) . #\c) . #\d) . #\e) . #\f))

(deftest reduce-string.13
  (reduce #'+ "abc" :start 0 :end 0)
  0)

(deftest reduce-string.14
  (let ((s (make-array '(8) :initial-contents "abcdefgh"
                       :fill-pointer 6
                       :element-type 'character)))
    (coerce (reduce #'(lambda (x y) (cons y x)) s :initial-value nil)
            'string))
  "fedcba")

(deftest reduce-string.15
  (let ((s (make-array '(8) :initial-contents "abcdefgh"
                       :fill-pointer 6
                       :element-type 'character)))
    (coerce (reduce #'(lambda (x y) (cons y x)) s :initial-value nil
                    :start 1)
            'string))
  "fedcb")

(deftest reduce-string.16
  (let ((s (make-array '(8) :initial-contents "abcdefgh"
                       :fill-pointer 6
                       :element-type 'character)))
    (coerce (reduce #'(lambda (x y) (cons y x)) s :end nil
                    :initial-value nil)
            'string))
  "fedcba")

(deftest reduce-string.17
  (let ((s (make-array '(8) :initial-contents "abcdefgh"
                       :fill-pointer 6
                       :element-type 'character)))
    (coerce (reduce #'(lambda (x y) (cons y x)) s :end 4
                    :initial-value nil)
            'string))
  "dcba")

(deftest reduce-string.18
  (do-special-strings
   (s "12345" nil)
   (let ((x (reduce #'(lambda (x y) (cons y x)) s)))
     (assert (equal x '(#\5 #\4 #\3 #\2 . #\1)))))
  nil)

(deftest reduce-string.19
  (do-special-strings
   (s "54321" nil)
   (let ((x (reduce #'cons s :from-end t)))
     (assert (equal x '(#\5 #\4 #\3 #\2 . #\1)))))
  nil)

(deftest reduce-string.20
  (do-special-strings
   (s "12345" nil)
   (let ((x (reduce #'(lambda (x y) (cons y x)) s :initial-value nil)))
     (assert (equal x '(#\5 #\4 #\3 #\2 #\1)))))
  nil)

;;;;;;;;

(deftest reduce-bitstring.1
  (reduce #'cons #*001101)
  (((((0 . 0) . 1) . 1) . 0) . 1))

(deftest reduce-bitstring.2
  (reduce #'cons #*001101 :from-end t)
  (0 0 1 1 0 . 1))

(deftest reduce-bitstring.3
  (reduce #'cons #*001101 :initial-value 'z)
  ((((((z . 0) . 0) . 1) . 1) . 0) . 1))

(deftest reduce-bitstring.4
  (reduce #'cons #*001101 :from-end t :initial-value 'g)
  (0 0 1 1 0 1 . g))

(deftest reduce-bitstring.5
  (reduce #'cons #*001101 :from-end nil)
  (((((0 . 0) . 1) . 1) . 0) . 1))

(deftest reduce-bitstring.6
  (reduce #'cons #*001101 :from-end 17)
  (0 0 1 1 0 . 1))

(deftest reduce-bitstring.7
  (reduce #'cons #*001101 :end nil)
  (((((0 . 0) . 1) . 1) . 0) . 1))

(deftest reduce-bitstring.8
  (reduce #'cons #*001101 :end 3)
  ((0 . 0) . 1))

(deftest reduce-bitstring.9
  (reduce #'cons #*001101 :start 1 :end 4)
  ((0 . 1) . 1))

(deftest reduce-bitstring.10
  (reduce #'cons #*001101 :start 1 :end 4 :from-end t)
  (0 1 . 1))

(deftest reduce-bitstring.11
  (reduce #'cons #*001101 :start 1 :end 4 :from-end t
          :initial-value nil)
  (0 1 1))

(deftest reduce-bitstring.12
  (reduce 'cons #*001101)
  (((((0 . 0) . 1) . 1) . 0) . 1))

(deftest reduce-bitstring.13
  (reduce #'+ #(1 1 1) :start 0 :end 0)
  0)

(deftest reduce-bitstring.14
  (let ((s (make-array '(8) :initial-contents '(0 0 1 0 1 1 0 1)
                       :fill-pointer 6
                       :element-type 'bit)))
    (reduce #'+ s))
  3)

(deftest reduce-bitstring.15
  (let ((s (make-array '(8) :initial-contents '(0 0 1 0 1 1 0 1)
                       :fill-pointer 6
                       :element-type 'bit)))
    (reduce #'+ s :start 3))
  2)

(deftest reduce-bitstring.16
  (let ((s (make-array '(8) :initial-contents '(0 0 1 0 1 1 0 1)
                       :fill-pointer 6
                       :element-type 'bit)))
    (reduce #'+ s :start 3 :initial-value 10))
  12)

(deftest reduce-bitstring.17
  (let ((s (make-array '(8) :initial-contents '(0 0 1 0 1 1 0 1)
                       :fill-pointer 6
                       :element-type 'bit)))
    (reduce #'+ s :end nil))
  3)

(deftest reduce-bitstring.18
  (let ((s (make-array '(8) :initial-contents '(1 1 1 1 1 1 1 1)
                       :fill-pointer 6
                       :element-type 'bit)))
    (reduce #'+ s :start 2 :end 4))
  2)

;;; Order of evaluation tests

(deftest reduce.order.1
  (let ((i 0) x y)
    (values
     (reduce (progn (setf x (incf i)) #'cons)
             (progn (setf y (incf i)) '(a b c)))
     i x y))
   ((a . b) . c) 2 1 2)

(deftest reduce.order.2
  (let ((i 0) a b c d e f g)
    (values
     (reduce (progn (setf a (incf i)) #'cons)
             (progn (setf b (incf i)) '(a b c d e f))
             :from-end (progn (setf c (incf i)) t)
             :initial-value (progn (setf d (incf i)) 'nil)
             :start (progn (setf e (incf i)) 1)
             :end (progn (setf f (incf i)) 4)
             :key (progn (setf g (incf i)) #'identity)
             )
     i a b c d e f g))
  (b c d) 7 1 2 3 4 5 6 7)

(deftest reduce.order.3
  (let ((i 0) a b c d e f g)
    (values
     (reduce (progn (setf a (incf i)) #'cons)
             (progn (setf b (incf i)) '(a b c d e f))
             :key (progn (setf c (incf i)) #'identity)
             :end (progn (setf d (incf i)) 4)
             :start (progn (setf e (incf i)) 1)
             :initial-value (progn (setf f (incf i)) 'nil)
             :from-end (progn (setf g (incf i)) t)
             )
     i a b c d e f g))
  (b c d) 7 1 2 3 4 5 6 7)


;;; Keyword tests

(deftest reduce.allow-other-keys.1
  (reduce #'+ '(1 2 3) :allow-other-keys t)
  6)

(deftest reduce.allow-other-keys.2
  (reduce #'+ '(1 2 3) :allow-other-keys nil)
  6)

(deftest reduce.allow-other-keys.3
  (reduce #'+ '(1 2 3) :bad t :allow-other-keys t)
  6)

(deftest reduce.allow-other-keys.4
  (reduce #'+ '(1 2 3) :allow-other-keys t :bad t)
  6)

(deftest reduce.allow-other-keys.5
  (reduce #'+ '(1 2 3) :allow-other-keys t :allow-other-keys nil :bad t)
  6)

(deftest reduce.allow-other-keys.6
  (reduce #'+ '(1 2 3) :allow-other-keys t :bad t :allow-other-keys nil)
  6)

(deftest reduce.allow-other-keys.7
  (reduce #'+ '(1 2 3) :bad t :allow-other-keys t :allow-other-keys nil)
  6)

(deftest reduce.allow-other-keys.8
  (reduce #'cons '(1 2 3) :allow-other-keys t :from-end t :bad t
          :initial-value nil)
  (1 2 3))

(deftest reduce.keywords.9
  (reduce #'cons '(1 2 3) :from-end t :from-end nil
          :initial-value nil :initial-value 'a)
  (1 2 3))

