;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 24 07:10:05 2002
;;;; Contains: Tests for POSITION-IF-NOT-NOT

(in-package #:sicl-sequence-test)

(deftest position-if-not-list.1
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-list.2
  (position-if-not 'oddp '(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-list.3
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-not-list.4
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-not-list.5
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-not-list.6
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-not-list.7
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-not-list.8
  (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-not-list.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-list.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-not-list.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-not-list.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp '(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-not-vector.1
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-vector.2
  (position-if-not 'oddp #(1 3 1 4 3 2 1 8 9))
  3)

(deftest position-if-not-vector.3
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start 4)
  5)

(deftest position-if-not-vector.4
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :from-end t)
  7)

(deftest position-if-not-vector.5
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :from-end nil)
  3)

(deftest position-if-not-vector.6
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start 4
               :from-end t)
  7)

(deftest position-if-not-vector.7
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :end nil)
  3)

(deftest position-if-not-vector.8
  (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :end 3)
  nil)

(deftest position-if-not-vector.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-vector.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-not-vector.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-not-vector.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp #(1 3 1 4 3 2 1 8 9) :start i :end j
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

(deftest position-if-not-vector.13
  (let ((a (make-array '(10) :initial-contents '(1 2 3 4 5 a b c d e)
                       :fill-pointer 5)))
    (values
     (position-if-not #'numberp a)
     (position-if-not #'symbolp a)
     (position-if-not #'numberp a :from-end t)
     (position-if-not #'symbolp a :from-end t)))
  nil 0 nil 4)

(deftest position-if-not-vector.14
  (let* ((v1 #(x x x a b 1 d a b 2 d y y y y y))
         (v2 (make-array '(8) :displaced-to v1
                        :displaced-index-offset 3)))
    (values (position-if-not #'symbolp v2)
            (position-if-not #'symbolp v2 :from-end t)))
  2 6)

;;; Bit vector tests

(deftest position-if-not-bit-vector.1
  (position-if-not #'oddp #*111010101)
  3)

(deftest position-if-not-bit-vector.2
  (position-if-not 'oddp #*111010101)
  3)

(deftest position-if-not-bit-vector.3
  (position-if-not #'oddp #*111010101 :start 4)
  5)

(deftest position-if-not-bit-vector.4
  (position-if-not #'oddp #*111010101 :from-end t)
  7)

(deftest position-if-not-bit-vector.5
  (position-if-not #'oddp #*111010101 :from-end nil)
  3)

(deftest position-if-not-bit-vector.6
  (position-if-not #'oddp #*111010101 :start 4
               :from-end t)
  7)

(deftest position-if-not-bit-vector.7
  (position-if-not #'oddp #*111010101 :end nil)
  3)

(deftest position-if-not-bit-vector.8
  (position-if-not #'oddp #*111010101 :end 3)
  nil)

(deftest position-if-not-bit-vector.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp #*111010101 :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-bit-vector.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'oddp #*111010101 :start i :end j
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

(deftest position-if-not-bit-vector.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp #*111010101 :start i :end j
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

(deftest position-if-not-bit-vector.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evenp #*111010101 :start i :end j
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

(deftest position-if-not-bit-vector.13
  (let ((a (make-array '(10) :initial-contents '(1 1 1 1 1 0 0 0 0 0)
                       :fill-pointer 5
                       :element-type 'bit)))
    (values
     (position-if-not #'zerop a)
     (position-if-not (complement #'zerop) a)
     (position-if-not #'zerop a :from-end t)
     (position-if-not (complement #'zerop) a :from-end t)))
  0 nil 4 nil)

;;; string tests

(deftest position-if-not-string.1
  (position-if-not #'odddigitp "131432189")
  3)

(deftest position-if-not-string.2
  (position-if-not 'odddigitp "131432189")
  3)

(deftest position-if-not-string.3
  (position-if-not #'odddigitp "131432189" :start 4)
  5)

(deftest position-if-not-string.4
  (position-if-not #'odddigitp "131432189" :from-end t)
  7)

(deftest position-if-not-string.5
  (position-if-not #'odddigitp "131432189" :from-end nil)
  3)

(deftest position-if-not-string.6
  (position-if-not #'odddigitp "131432189" :start 4
               :from-end t)
  7)

(deftest position-if-not-string.7
  (position-if-not #'odddigitp "131432189" :end nil)
  3)

(deftest position-if-not-string.8
  (position-if-not #'odddigitp "131432189" :end 3)
  nil)

(deftest position-if-not-string.9
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'odddigitp "131432189" :start i :end j)))
  ((nil nil nil 3 3 3 3 3 3)
   (nil nil 3 3 3 3 3 3)
   (nil 3 3 3 3 3 3)
   (3 3 3 3 3 3)
   (nil 5 5 5 5)
   (5 5 5 5)
   (nil 7 7)
   (7 7)
   (nil)))

(deftest position-if-not-string.10
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'odddigitp "131432189" :start i :end j
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

(deftest position-if-not-string.11
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evendigitp "131432183" :start i :end j
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

(deftest position-if-not-string.12
  (loop for i from 0 to 8
        collect
        (loop for j from (1+ i) to 9
              collect
              (position-if-not #'evendigitp "131432183" :start i :end j
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

(deftest position-if-not-string.13
  (let ((a (make-array '(10) :initial-contents "55555aaaaa"
                       :fill-pointer 5
                       :element-type 'character)))
    (and (stringp a)
         (values
          (position-if-not #'digit-char-p a)
          (position-if-not (complement #'digit-char-p) a)
          (position-if-not #'digit-char-p a :from-end t)
          (position-if-not (complement #'digit-char-p) a :from-end t))))
  nil 0 nil 4)

(deftest position-if-not-string.14
  (do-special-strings
   (s "12345a6  78b90" nil)
   (let ((pos (position-if-not (complement #'alpha-char-p) s)))
     (assert (eql pos 5) () "First alpha char in ~A is at position ~A" s pos)))
  nil)

(deftest position-if-not-string.15
  (do-special-strings
   (s "12345a6  78b90" nil)
   (let ((pos (position-if-not (complement #'alpha-char-p) s :from-end t)))
     (assert (eql pos 11) () "Last alpha char in ~A is at position ~A" s pos)))
  nil)

(deftest position-if-not.order.1
  (let ((i 0) a b c d e f)
    (values
     (position-if-not
      (progn (setf a (incf i)) (complement #'zerop))
      (progn (setf b (incf i)) '(3 1 8 2 1 2 3 4))
      :from-end (setf c (incf i))
      :start (progn (setf d (incf i)) 1)
      :end (progn (setf e (incf i)) 6)
      :key (progn (setf f (incf i)) #'1-))
     i a b c d e f))
  4 6 1 2 3 4 5 6)

(deftest position-if-not.order.2
  (let ((i 0) a b c d e f)
    (values
     (position-if-not
      (progn (setf a (incf i)) (complement #'zerop))
      (progn (setf b (incf i)) '(3 1 8 2 1 2 3 4))
      :key (progn (setf c (incf i)) #'1-)
      :end (progn (setf d (incf i)) 6)
      :start (progn (setf e (incf i)) 1)
      :from-end (setf f (incf i)))
     i a b c d e f))
  4 6 1 2 3 4 5 6)

;;; Keyword tests

(deftest position-if-not.allow-other-keys.1
  (position-if-not #'zerop '(0 0 3 2 0 1) :allow-other-keys t)
  2)

(deftest position-if-not.allow-other-keys.2
  (position-if-not #'zerop '(0 0 3 2 0 1) :allow-other-keys nil)
  2)

(deftest position-if-not.allow-other-keys.3
  (position-if-not #'zerop '(0 0 1 2 3 0) :allow-other-keys t :bad t)
  2)

(deftest position-if-not.allow-other-keys.4
  (position-if-not #'zerop '(0 0 1 2 3 0) :bad t :allow-other-keys t)
  2)

(deftest position-if-not.allow-other-keys.5
  (position-if-not #'zerop '(0 0 1 2 3 0) :bad t :allow-other-keys t :key #'1-)
  0)

(deftest position-if-not.keywords.6
  (position-if-not #'zerop '(0 0 1 2 3 0) :key #'1- :key #'identity)
  0)

(deftest position-if-not.allow-other-keys.7
  (position-if-not #'zerop '(0 0 1 2 3 0) :bad t :allow-other-keys t
               :allow-other-keys nil)
  2)

(deftest position-if-not.allow-other-keys.8
  (position-if-not #'zerop '(0 0 1 2 3 0) :allow-other-keys t :bad t
               :allow-other-keys nil)
  2)

(deftest position-if-not.allow-other-keys.9
  (position-if-not #'zerop '(0 0 1 2 3 0) :allow-other-keys t
               :allow-other-keys nil :bad t)
  2)


;;; Error tests

(deftest position-if-not.error.1
  (check-type-error #'(lambda (x) (position-if-not #'identity x)) #'sequencep)
  nil)

(deftest position-if-not.error.4
  (signals-error (position-if-not 'identity '(a b c . d)) type-error)
  t)

(deftest position-if-not.error.5
  (signals-error (position-if-not) program-error)
  t)

(deftest position-if-not.error.6
  (signals-error (position-if-not #'null) program-error)
  t)

(deftest position-if-not.error.7
  (signals-error (position-if-not #'null nil :key) program-error)
  t)

(deftest position-if-not.error.8
  (signals-error (position-if-not #'null nil 'bad t) program-error)
  t)

(deftest position-if-not.error.9
  (signals-error (position-if-not #'null nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest position-if-not.error.10
  (signals-error (position-if-not #'null nil 1 2) program-error)
  t)

(deftest position-if-not.error.11
  (signals-error (locally (position-if-not #'identity 'b) t) type-error)
  t)

(deftest position-if-not.error.12
  (signals-error (position-if-not #'cons '(a b c d)) program-error)
  t)

(deftest position-if-not.error.13
  (signals-error (position-if-not #'car '(a b c d)) type-error)
  t)

(deftest position-if-not.error.14
  (signals-error (position-if-not #'identity '(a b c d) :key #'cdr) type-error)
  t)

(deftest position-if-not.error.15
  (signals-error (position-if-not #'identity '(a b c d) :key #'cons) program-error)
  t)
