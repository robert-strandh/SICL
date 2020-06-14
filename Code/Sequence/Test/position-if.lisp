;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 22:08:57 2002
;;;; Contains: Tests for POSITION-IF

(in-package #:sicl-sequence-test)

(deftest position-if-list.1
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-list.2
  (position-if 'evenp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-list.3
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-list.4
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-list.5
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-list.6
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-list.7
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-list.8
  (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-list.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-list.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
                           :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-list.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
                           :key '1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-list.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
                           :key #'1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

;;; Vector tests

(deftest position-if-vector.1
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-vector.2
  (position-if 'evenp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-vector.3
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-vector.4
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-vector.5
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-vector.6
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-vector.7
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-vector.8
  (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-vector.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-vector.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
                           :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-vector.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
                           :key '1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-vector.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
                           :key #'1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-vector.13
  (let ((a (make-array '(10) :initial-contents '(1 3 1 4 3 1 2 1 8 9)
                       :fill-pointer 5)))
    (flet ((%f (x) (eql x 1)))
      (values (position-if #'%f a)
              (position-if #'%f a :from-end t))))
  0 2)

(deftest position-if-vector.14
  (let* ((v1 #(x x x a b 1 d a b 2 d y y y y y))
         (v2 (make-array '(8) :displaced-to v1
                        :displaced-index-offset 3)))
    (values (position-if #'integerp v2)
            (position-if #'integerp v2 :from-end t)))
  2 6)

;;; Bit vector tests

(deftest position-if-bit-vector.1
  (position-if #'evenp #*111010101)
  3)

(deftest position-if-bit-vector.2
  (position-if 'evenp #*111010101)
  3)

(deftest position-if-bit-vector.3
  (position-if #'evenp #*111010101 :start 4)
  5)

(deftest position-if-bit-vector.4
  (position-if #'evenp #*111010101 :from-end t)
  7)

(deftest position-if-bit-vector.5
  (position-if #'evenp #*111010101 :from-end nil)
  3)

(deftest position-if-bit-vector.6
  (position-if #'evenp #*111010101 :start 4
               :from-end t)
  7)

(deftest position-if-bit-vector.7
  (position-if #'evenp #*111010101 :end nil)
  3)

(deftest position-if-bit-vector.8
  (position-if #'evenp #*111010101 :end 3)
  nil)

(deftest position-if-bit-vector.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp #*111010101 :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-bit-vector.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evenp #*111010101 :start i :end j
                           :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-bit-vector.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp #*111010101 :start i :end j
                           :key #'1+)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-bit-vector.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'oddp #*111010101 :start i :end j
                           :key '1+ :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-bit-vector.13
  (let ((a (make-array '(10) :initial-contents '(1 1 1 1 1 0 0 0 0 0)
                       :fill-pointer 5
                       :element-type 'bit)))
    (values (position-if #'evenp a)
            (position-if #'evenp a :from-end 'foo)
            (position-if #'oddp a)
            (position-if #'oddp a :from-end 'foo)))
  nil nil 0 4)

;;; string tests

(deftest position-if-string.1
  (position-if #'evendigitp "131432189")
  3)

(deftest position-if-string.2
  (position-if 'evendigitp "131432189")
  3)

(deftest position-if-string.3
  (position-if #'evendigitp "131432189" :start 4)
  5)

(deftest position-if-string.4
  (position-if #'evendigitp "131432189" :from-end t)
  7)

(deftest position-if-string.5
  (position-if #'evendigitp "131432189" :from-end nil)
  3)

(deftest position-if-string.6
  (position-if #'evendigitp "131432189" :start 4
               :from-end t)
  7)

(deftest position-if-string.7
  (position-if #'evendigitp "131432189" :end nil)
  3)

(deftest position-if-string.8
  (position-if #'evendigitp "131432189" :end 3)
  nil)

(deftest position-if-string.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evendigitp "131432189" :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-string.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'evendigitp "131432189" :start i :end j
                           :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-string.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'odddigitp "131432189" :start i :end j
                           :key #'nextdigit)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-string.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if #'odddigitp "131432189" :start i :end j
                           :key 'nextdigit :from-end t)))
  ((nil nil nil 3 3 5 5 7 7)
   (nil nil 3 3 5 5 7 7)
   (nil 3 3 5 5 7 7)
   (3 3 5 5 7 7)
   (nil 5 5 7 7)
   (5 5 7 7)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-string.13
  (flet ((%f (c) (eql c #\0))
         (%g (c) (eql c #\1)))
    (let ((a (make-array '(10) :initial-contents "1111100000"
                       :fill-pointer 5
                       :element-type 'character)))
    (values (position-if #'%f a)
            (position-if #'%f a :from-end 'foo)
            (position-if #'%g a)
            (position-if #'%g a :from-end 'foo))))
  nil nil 0 4)

(deftest position-if-string.14
  (do-special-strings
   (s "12345a6  78b90" nil)
   (let ((pos (position-if #'alpha-char-p s)))
     (assert (eql pos 5) () "First alpha char in ~A is at position ~A" s pos)))
  nil)

(deftest position-if-string.15
  (do-special-strings
   (s "12345a6  78b90" nil)
   (let ((pos (position-if #'alpha-char-p s :from-end t)))
     (assert (eql pos 11) () "Last alpha char in ~A is at position ~A" s pos)))
  nil)


(deftest position-if.order.1
  (let ((i 0) a b c d e f)
    (values
     (position-if
      (progn (setf a (incf i)) #'zerop)
      (progn (setf b (incf i)) '(3 1 8 2 1 2 3 4))
      :from-end (setf c (incf i))
      :start (progn (setf d (incf i)) 1)
      :end (progn (setf e (incf i)) 6)
      :key (progn (setf f (incf i)) #'1-))
     i a b c d e f))
  4 6 1 2 3 4 5 6)

(deftest position-if.order.2
  (let ((i 0) a b c d e f)
    (values
     (position-if
      (progn (setf a (incf i)) #'zerop)
      (progn (setf b (incf i)) '(3 1 8 2 1 2 3 4))
      :key (progn (setf c (incf i)) #'1-)
      :end (progn (setf d (incf i)) 6)
      :start (progn (setf e (incf i)) 1)
      :from-end (setf f (incf i)))
     i a b c d e f))
  4 6 1 2 3 4 5 6)

;;; Keyword tests

(deftest position-if.allow-other-keys.1
  (position-if #'zerop '(1 2 0 3 2 1) :allow-other-keys t)
  2)

(deftest position-if.allow-other-keys.2
  (position-if #'zerop '(1 2 0 3 2 1) :allow-other-keys nil)
  2)

(deftest position-if.allow-other-keys.3
  (position-if #'zerop '(1 2 0 3 2 1) :allow-other-keys t :bad t)
  2)

(deftest position-if.allow-other-keys.4
  (position-if #'zerop '(1 2 0 3 2 1) :bad t :allow-other-keys t)
  2)

(deftest position-if.allow-other-keys.5
  (position-if #'zerop '(1 2 0 3 2 1) :bad t :allow-other-keys t :key #'1-)
  0)

(deftest position-if.keywords.6
  (position-if #'zerop '(1 2 0 3 2 1) :key #'1- :key #'identity)
  0)

(deftest position-if.allow-other-keys.7
  (position-if #'zerop '(1 2 0 3 2 1) :bad t :allow-other-keys t
               :allow-other-keys nil)
  2)

(deftest position-if.allow-other-keys.8
  (position-if #'zerop '(1 2 0 3 2 1) :allow-other-keys t :bad t
               :allow-other-keys nil)
  2)

(deftest position-if.allow-other-keys.9
  (position-if #'zerop '(1 2 0 3 2 1) :allow-other-keys t
               :allow-other-keys nil :bad t)
  2)


;;; Error tests

(deftest position-if.error.1
  (check-type-error #'(lambda (x) (position-if #'identity x)) #'sequencep)
  nil)

(deftest position-if.error.4
  (signals-error (position-if 'null '(a b c . d)) type-error)
  t)

(deftest position-if.error.5
  (signals-error (position-if) program-error)
  t)

(deftest position-if.error.6
  (signals-error (position-if #'null) program-error)
  t)

(deftest position-if.error.7
  (signals-error (position-if #'null nil :key) program-error)
  t)

(deftest position-if.error.8
  (signals-error (position-if #'null nil 'bad t) program-error)
  t)

(deftest position-if.error.9
  (signals-error (position-if #'null nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest position-if.error.10
  (signals-error (position-if #'null nil 1 2) program-error)
  t)

(deftest position-if.error.11
  (signals-error (locally (position-if #'identity 'b) t) type-error)
  t)

(deftest position-if.error.12
  (signals-error (position-if #'cons '(a b c d)) program-error)
  t)

(deftest position-if.error.13
  (signals-error (position-if #'car '(a b c d)) type-error)
  t)

(deftest position-if.error.14
  (signals-error (position-if #'identity '(a b c d) :key #'cdr) type-error)
  t)

(deftest position-if.error.15
  (signals-error (position-if #'identity '(a b c d) :key #'cons) program-error)
  t)
