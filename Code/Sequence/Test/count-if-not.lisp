;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 22:42:35 2002
;;;; Contains: Tests for COUNT-IF-NOT

(in-package #:sicl-sequence-test)

(deftest count-if-not-list.1
  (count-if-not #'identity '(a b nil c d nil e))
  2)

(deftest count-if-not-list.2
  (count-if-not #'not '(a b nil c d nil e))
  5)

(deftest count-if-not-list.3
  (count-if-not #'(lambda (x) (break)) nil)
  0)

(deftest count-if-not-list.4
  (count-if-not #'identity '(a b nil c d nil e) :key #'identity)
  2)

(deftest count-if-not-list.5
  (count-if-not 'identity '(a b nil c d nil e) :key #'identity)
  2)

(deftest count-if-not-list.6
  (count-if-not #'identity '(a b nil c d nil e) :key 'identity)
  2)

(deftest count-if-not-list.8
  (count-if-not #'identity '(a b nil c d nil e) :key 'not)
  5)

(deftest count-if-not-list.9
  (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-not-list.10
  (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-not-list.11
  (let ((c 0))
    (count-if-not #'oddp '(1 2 3 4 4 1 8 10 1)
              :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-list.12
  (let ((c 0))
    (count-if-not #'oddp '(0 1 2 3 4 4 1 7 10 1)
              :from-end t
              :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-list.13
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-not-list.14
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :end 7)
  2)

(deftest count-if-not-list.15
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :end 7
            :start 2)
  1)

(deftest count-if-not-list.16
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            '(a b c d a e f a e f f a a) :end 7
            :start 2 :from-end t)
  1)


;;; tests on vectors

(deftest count-if-not-vector.1
  (count-if-not #'identity #(a b nil c d nil e))
  2)

(deftest count-if-not-vector.2
  (count-if-not #'not #(a b nil c d nil e))
  5)

(deftest count-if-not-vector.3
  (count-if-not #'(lambda (x) (break)) #())
  0)

(deftest count-if-not-vector.4
  (count-if-not #'not #(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-not-vector.5
  (count-if-not 'not #(a b nil c d nil e) :key #'identity)
  5)

(deftest count-if-not-vector.6
  (count-if-not #'not #(a b nil c d nil e) :key 'identity)
  5)

(deftest count-if-not-vector.8
  (count-if-not #'not #(a b nil c d nil e) :key 'not)
  2)

(deftest count-if-not-vector.9
  (count-if-not #'oddp #(1 2 3 4 4 1 8 10 1))
  5)

(deftest count-if-not-vector.10
  (count-if-not #'oddp #(1 2 3 4 4 1 8 10 1) :key #'1+)
  4)

(deftest count-if-not-vector.11
  (let ((c 0))
    (count-if-not #'oddp #(1 2 3 4 4 1 8 10 1)
              :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-vector.12
  (let ((c 0))
    (count-if-not #'oddp #(0 1 2 3 4 4 1 7 10 1)
              :from-end t
              :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-vector.13
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            #(a b c d a e f a e f f a a) :start 2)
  4)

(deftest count-if-not-vector.14
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            #(a b c d a e f a e f f a a) :end 7)
  2)

(deftest count-if-not-vector.15
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            #(a b c d a e f a e f f a a) :end 7
            :start 2)
  1)

(deftest count-if-not-vector.16
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            #(a b c d a e f a e f f a a) :end 7
            :start 2 :from-end t)
  1)

;;; Non-simple vectors

(deftest count-if-not-nonsimple-vector.1
  (count-if-not #'identity (make-array 7 :initial-contents '(a b nil c d nil e)
                                       :fill-pointer t
                                       :adjustable t))
  2)

(deftest count-if-not-nonsimple-vector.2
  (count-if-not #'not (make-array 7 :initial-contents '(a b nil c d nil e)
                                  :fill-pointer t
                                  :adjustable t))
  5)

(deftest count-if-not-nonsimple-vector.3
  (count-if-not #'(lambda (x) (break)) (make-array 0
                                                   :fill-pointer t
                                                   :adjustable t))
  0)

(deftest count-if-not-nonsimple-vector.4
  (count-if-not #'not
            (make-array 7 :initial-contents '(a b nil c d nil e)
                        :fill-pointer t
                        :adjustable t)
            :key #'identity)
  5)

(deftest count-if-not-nonsimple-vector.5
  (count-if-not 'not
            (make-array 7 :initial-contents '(a b nil c d nil e)
                        :fill-pointer t
                        :adjustable t)
            :key #'identity)
  5)

(deftest count-if-not-nonsimple-vector.6
  (count-if-not #'not
            (make-array 7 :initial-contents '(a b nil c d nil e)
                        :fill-pointer t
                        :adjustable t)
            :key 'identity)
  5)

(deftest count-if-not-nonsimple-vector.8
  (count-if-not #'not
            (make-array 7 :initial-contents '(a b nil c d nil e)
                        :fill-pointer t
                        :adjustable t)
            :key 'not)
  2)

(deftest count-if-not-nonsimple-vector.9
  (count-if-not #'oddp (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
                                :fill-pointer t :adjustable t))
  5)

(deftest count-if-not-nonsimple-vector.10
  (count-if-not #'oddp
            (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
                        :fill-pointer t :adjustable t)
            :key #'1+)
  4)

(deftest count-if-not-nonsimple-vector.11
  (let ((c 0))
    (count-if-not #'oddp
              (make-array 9 :initial-contents '(1 2 3 4 4 1 8 10 1)
                          :fill-pointer t :adjustable t)
              :key #'(lambda (x) (+ x (incf c)))))
  6)

(deftest count-if-not-nonsimple-vector.12
  (let ((c 0))
    (count-if-not #'oddp
              (make-array 10 :initial-contents '(0 1 2 3 4 4 1 7 10 1)
                          :fill-pointer t :adjustable t)
              :from-end t
              :key #'(lambda (x) (+ x (incf c)))))
  8)

(deftest count-if-not-nonsimple-vector.13
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
                        :fill-pointer t :adjustable t)
            :start 2)
  4)

(deftest count-if-not-nonsimple-vector.14
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
                        :fill-pointer t :adjustable t)
            :end 7)
  2)

(deftest count-if-not-nonsimple-vector.15
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
                        :fill-pointer t :adjustable t)
            :end 7 :start 2)
  1)

(deftest count-if-not-nonsimple-vector.16
  (count-if-not #'(lambda (x) (not (eqt x 'a)))
            (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
                        :fill-pointer t :adjustable t)
            :end 7 :start 2 :from-end t)
  1)

(deftest count-if-not-nonsimple-vector.17
  (flet ((%a (c) (not (eqt c 'a)))
         (%f (c) (not (eqt c 'f))))
    (let ((a (make-array 13 :initial-contents '(a b c d a e f a e f f a a)
                         :fill-pointer 9)))
      (values (count-if-not #'%a a)
              (count-if-not #'%a a :from-end t)
              (count-if-not #'%f a)
              (count-if-not #'%f a :from-end t)
              )))
  3 3 1 1)

;;; Other special vectors

`(deftest count-if-not.special-vector.1
  (do-special-integer-vectors
   (v #(1 0 1 1 1 0 1 1 1 0 1) nil)
   (assert (eql (count-if-not #'plusp v) 3))
   (assert (eql (count-if-not #'zerop v) 8))
   (assert (eql (count-if-not #'plusp v :start 2) 2))
   (assert (eql (count-if-not #'zerop v :end 9) 7)))
  nil)

(deftest count-if-not.special-vector.2
  (do-special-integer-vectors
   (v #(1 3 2 4 7 5 6 1 0 2 4) nil)
   (assert (eql (count-if-not #'evenp v) 5))
   (assert (eql (count-if-not #'oddp v) 6))
   (assert (eql (count-if-not #'plusp v :start 2) 1))
   (assert (eql (count-if-not #'zerop v :end 8) 8)))
  nil)

(deftest count-if-not.special-vector.3
  (loop for etype in '(short-float single-float double-float long-float)
        for vals = (loop for e in '(0 1 2 1 3 0 4 5 6 0)
                         collect (coerce e etype))
        for vec = (make-array (length vals) :element-type etype :initial-contents vals)
        for result = (count-if-not #'zerop vec)
        unless (= result 7)
        collect (list etype vals vec result))
  nil)

(deftest count-if-not.special-vector.4
  (loop for cetype in '(short-float single-float double-float long-float integer rational)
        for etype = `(complex ,cetype)
        for vals = (loop for e in '(6 1 2 1 3 -4 4 5 6 100)
                         collect (complex 0 (coerce e cetype)))
        for vec = (make-array (length vals) :element-type etype :initial-contents vals)
        for result = (count-if-not #'(lambda (x) (< (abs x) 5/2)) vec)
        unless (= result 7)
        collect (list etype vals vec result))
  nil)


;;; tests on bit-vectors

(deftest count-if-not-bit-vector.1
  (count-if-not #'oddp #*001011101101)
  5)

(deftest count-if-not-bit-vector.2
  (count-if-not #'identity #*001011101101)
  0)

(deftest count-if-not-bit-vector.3
  (count-if-not #'(lambda (x) (break)) #*)
  0)

(deftest count-if-not-bit-vector.4
  (count-if-not #'identity #*001011101101 :key #'zerop)
  7)

(deftest count-if-not-bit-vector.5
  (count-if-not 'not #*001011101101 :key #'zerop)
  5)

(deftest count-if-not-bit-vector.6
  (count-if-not #'not #*001011101101 :key 'zerop)
  5)

(deftest count-if-not-bit-vector.8
  (count-if-not #'identity #*001011101101 :key 'oddp)
  5)

(deftest count-if-not-bit-vector.10
  (count-if-not #'oddp #*001011101101 :key #'1+)
  7)

(deftest count-if-not-bit-vector.11
  (let ((c 0))
    (count-if-not #'oddp #*001011101101
                  :key #'(lambda (x) (+ x (incf c)))))
  7)

(deftest count-if-not-bit-vector.12
  (let ((c 0))
    (count-if-not #'oddp #*001011101101
              :from-end t
              :key #'(lambda (x) (+ x (incf c)))))
  5)

(deftest count-if-not-bit-vector.13
  (count-if-not #'zerop #*0111011011100 :start 2)
  7)

(deftest count-if-not-bit-vector.14
  (count-if-not #'zerop #*0111011011100 :end 7)
  5)

(deftest count-if-not-bit-vector.15
  (count-if-not #'zerop #*0111011011100 :end 7 :start 2)
  4)

(deftest count-if-not-bit-vector.16
  (count-if-not #'zerop #*0111011011100 :end 7 :start 2 :from-end t)
  4)

(deftest count-if-not-bit-vector.17
  (let ((a (make-array '(10) :initial-contents '(0 0 0 1 1 1 0 1 0 0)
                       :fill-pointer 5
                       :element-type 'bit)))
    (and (bit-vector-p a)
         (values (count-if-not #'zerop a)
                 (count-if-not #'oddp a)
                 (count-if-not #'zerop a :from-end t)
                 (count-if-not #'oddp a :from-end t))))
  2 3 2 3)

;;; tests on strings

(deftest count-if-not-string.1
  (count-if-not #'(lambda (x) (eql x #\0)) "001011101101")
  7)

(deftest count-if-not-string.2
  (count-if-not #'identity "001011101101")
  0)

(deftest count-if-not-string.3
  (count-if-not #'(lambda (x) (break)) "")
  0)

(deftest count-if-not-string.4
  (count-if-not #'identity "001011101101" :key #'(lambda (x) (eql x #\0)))
  7)

(deftest count-if-not-string.5
  (count-if-not 'identity "001011101101" :key #'(lambda (x) (eql x #\0)))
  7)

(deftest count-if-not-string.6
  (count-if-not #'(lambda (x) (eql x #\0)) "001011101101" :key 'identity)
  7)

(deftest count-if-not-string.8
  (count-if-not #'identity "001011101101" :key #'(lambda (x) (eql x #\1)))
  5)

(deftest count-if-not-string.11
  (let ((c 0))
    (count-if-not #'oddp "001011101101"
                  :key #'(lambda (x) (+ (if (eql x #\0) 0 1) (incf c)))))
  7)

(deftest count-if-not-string.12
  (let ((c 0))
    (count-if-not #'oddp "001011101101"
                  :from-end t
                  :key #'(lambda (x) (+ (if (eql x #\0) 0 1) (incf c)))))
  5)

(deftest count-if-not-string.13
  (count-if-not #'(lambda (x) (eql x #\0)) "0111011011100" :start 2)
  7)

(deftest count-if-not-string.14
  (count-if-not #'(lambda (x) (eql x #\0)) "0111011011100" :end 7)
  5)

(deftest count-if-not-string.15
  (count-if-not #'(lambda (x) (eql x #\0)) "0111011011100" :end 7 :start 2)
  4)

(deftest count-if-not-string.16
  (count-if-not #'(lambda (x) (eql x #\0))
                "0111011011100" :end 7 :start 2 :from-end t)
  4)

(deftest count-if-not-string.17
  (flet ((%zerop (c) (eql c #\0))
         (%onep (c) (eql c #\1)))
    (let ((a (make-array '(10) :initial-contents "0001110100"
                         :fill-pointer 5
                         :element-type 'character)))
      (and (stringp a)
           (values (count-if-not #'%zerop a)
                   (count-if-not #'%onep a)
                   (count-if-not #'%zerop a :from-end t)
                   (count-if-not #'%onep a :from-end t)))))
  2 3 2 3)

(deftest count-if-not-string.18
  (do-special-strings
   (s "a1ha^%&%#( 873ff83nfa!" nil)
   (assert (= (count-if-not #'alpha-char-p s) 14)))
  nil)

;;; Argument order tests

(deftest count-if-not.order.1
  (let ((i 0) c1 c2 c3 c4 c5 c6)
    (values
     (count-if-not
      (progn (setf c1 (incf i)) #'null)
      (progn (setf c2 (incf i)) '(a nil b c nil d e))
      :start (progn (setf c3 (incf i)) 0)
      :end (progn (setf c4 (incf i)) 3)
      :key (progn (setf c5 (incf i)) #'not)
      :from-end (progn (setf c6 (incf i)) nil)
      )
     i c1 c2 c3 c4 c5 c6))
  1 6 1 2 3 4 5 6)

(deftest count-if-not.order.2
  (let ((i 0) c1 c2 c3 c4 c5 c6)
    (values
     (count-if-not
      (progn (setf c1 (incf i)) #'null)
      (progn (setf c2 (incf i)) '(a nil b c nil d e))
      :from-end (progn (setf c3 (incf i)) nil)
      :key (progn (setf c4 (incf i)) #'not)
      :end (progn (setf c5 (incf i)) 3)
      :start (progn (setf c6 (incf i)) 0)
      )
     i c1 c2 c3 c4 c5 c6))
  1 6 1 2 3 4 5 6)

;;; Keyword tests

(deftest count-if-not.keywords.1
  (count-if-not #'oddp '(1 2 3 4 5) :bad t :allow-other-keys t)
  2)

(deftest count-if-not.keywords.2
  (count-if-not #'oddp '(1 2 3 4 5) :allow-other-keys #p"*" :also-bad t)
  2)

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(deftest count-if-not.keywords.3
  (count-if-not #'oddp '(1 2 3 4 5)
            :allow-other-keys t
            :allow-other-keys nil
            :bad t)
  2)

(deftest count-if-not.keywords.4
  (count-if-not #'oddp '(1 2 3 4 5) :key #'identity :key #'1+)
  2)

(deftest count-if-not.allow-other-keys.5
  (count-if-not #'null '(nil a b c nil) :allow-other-keys nil)
  3)

;;; Error tests

(deftest count-if-not.error.1
  (check-type-error #'(lambda (x) (count-if-not #'identity x)) #'sequencep)
  nil)

(deftest count-if-not.error.4
  (signals-error (count-if-not) program-error)
  t)

(deftest count-if-not.error.5
  (signals-error (count-if-not #'null) program-error)
  t)

(deftest count-if-not.error.6
  (signals-error (count-if-not #'null nil :bad t) program-error)
  t)

(deftest count-if-not.error.7
  (signals-error (count-if-not #'null nil :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest count-if-not.error.8
  (signals-error (count-if-not #'null nil :key) program-error)
  t)

(deftest count-if-not.error.9
  (signals-error (count-if-not #'null nil 3 3) program-error)
  t)

;;; Only leftmost :allow-other-keys argument matters
(deftest count-if-not.error.10
  (signals-error (count-if-not #'null nil :bad t
                                :allow-other-keys nil
                                :allow-other-keys t)
                 program-error)
  t)

(deftest count-if-not.error.11
  (signals-error (locally (count-if-not #'identity 1) t)
                 type-error)
  t)

(deftest count-if-not.error.12
  (signals-error (count-if-not #'cons '(a b c)) program-error)
  t)

(deftest count-if-not.error.13
  (signals-error (count-if-not #'car '(a b c)) type-error)
  t)

(deftest count-if-not.error.14
  (signals-error (count-if-not #'identity '(a b c) :key #'cdr)
                 type-error)
  t)

(deftest count-if-not.error.15
  (signals-error (count-if-not #'identity '(a b c) :key #'cons)
                 program-error)
  t)
