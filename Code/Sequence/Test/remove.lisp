;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Sep 14 11:46:05 2002
;;;; Contains: Tests for REMOVE



(in-package #:sicl-sequence-test)

(deftest remove-list.1
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.2
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :count nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.3
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :key nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.4
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :count 100)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.5
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :count 0)))
    (and (equalp orig x) y))
  (a b c a b d a c b a e))

(deftest remove-list.6
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :count 1)))
    (and (equalp orig x) y))
  (b c a b d a c b a e))

(deftest remove-list.7
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'c x :count 1)))
    (and (equalp orig x) y))
  (a b a b d a c b a e))

(deftest remove-list.8
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :from-end t)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.9
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :from-end t :count 1)))
    (and (equalp orig x) y))
  (a b c a b d a c b e))

(deftest remove-list.10
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :from-end t :count 4)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.11
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
           collect (remove 'a x :start i))
     (equalp orig x)))
  ((b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.12
 (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
           collect (remove 'a x :start i :end nil))
     (equalp orig x)))
  ((b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.13
 (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
           collect (remove 'a x :start i :end 11))
     (equalp orig x)))
  ((b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.14
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove 'a x :end nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-list.15
 (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig)))
    (values
     (loop for i from 0 to 9
           collect (remove 'a x :start i :end 9))
     (equalp orig x)))
  ((b c b d c b a e)
   (a b c b d c b a e)
   (a b c b d c b a e)
   (a b c b d c b a e)
   (a b c a b d c b a e)
   (a b c a b d c b a e)
   (a b c a b d c b a e)
   (a b c a b d a c b a e)
   (a b c a b d a c b a e)
   (a b c a b d a c b a e))
  t)

(deftest remove-list.16
 (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
           collect (remove 'a x :start i :end 11 :count 1))
     (equalp orig x)))
 ((b c a b d a c b a e)
  (a b c b d a c b a e)
  (a b c b d a c b a e)
  (a b c b d a c b a e)
  (a b c a b d c b a e)
  (a b c a b d c b a e)
  (a b c a b d c b a e)
  (a b c a b d a c b e)
  (a b c a b d a c b e)
  (a b c a b d a c b e)
  (a b c a b d a c b a e))
 t)

(deftest remove-list.17
 (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig)))
    (values
     (loop for i from 0 to 10
           collect (remove 'a x :start i :end (1+ i)))
     (equalp orig x)))
 ((  b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c   b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d   c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b a e)
  (a b c a b d a c b   e)
  (a b c a b d a c b a e))
 t)

;;; Show that it tests using EQL, not EQ
;;; NOTE: this test was bogus, since we can't sure non-EQness is preserved
#|
(deftest remove-list.18
   (let* ((i (1+ most-positive-fixnum))
          (orig (list i 0 i 1 i 2 3))
          (x (copy-seq orig))
          (y (remove (1+ most-positive-fixnum) x)))
     (and (equalp orig x) y))
   (0 1 2 3))
|#

(deftest remove-list.19
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 1 x :key #'1-)))
    (and (equalp orig x) y))
  (1 3 6 1 4 1 3 7))

(deftest remove-list.20
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :test #'>)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.21
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :test '> :from-end t)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.22
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 2 x :key nil)))
    (and (equalp orig x) y))
  (1 3 6 1 4 1 3 7))

(deftest remove-list.23
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 1 x :key '1-)))
    (and (equalp orig x) y))
  (1 3 6 1 4 1 3 7))

(deftest remove-list.24
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :test-not #'<=)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.25
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :test-not '<= :from-end t)))
    (and (equalp orig x) y))
  (3 6 4 3 7))

(deftest remove-list.26
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :from-end t :start 1 :end 5)))
    (and (equalp orig x) y))
  (1 2 2 6 1 2 4 1 3 2 7))

(deftest remove-list.27
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :count -1)))
    (and (equalp orig x)
         (equalpt x y)))
  t)

(deftest remove-list.28
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :count -1000000000000)))
    (and (equalp orig x)
         (equalpt x y)))
  t)

(deftest remove-list.29
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove 3 x :count 1000000000000)))
    (and (equalp orig x)
         y))
  (1 2 2 6 1 2 4 1 2 7))

;;; Assorted tests of remove and delete on vectors, strings,
;;; and bit vectors.  These are mostly to exercise bugs previously
;;; detected by the randomized tests

(deftest remove-vector.1
  (remove 'a (vector 'b 'c 'd))
  #(b c d))

(deftest remove-vector.2
  (remove 'a (vector 'b 'c 'd) :count -1)
  #(b c d))

(deftest remove-vector.3
  (remove 'a (vector 'a 'b 'c 'd) :count -1)
  #(a b c d))

(deftest remove-string.1
  (remove #\a (copy-seq "abcad"))
  "bcd")

(deftest remove-string.2
  (remove #\a (copy-seq "abcad") :count -1)
  "abcad")

(deftest remove-string.3
  (remove #\a (copy-seq "bcd") :count -1)
  "bcd")

(deftest remove-string.4
  (do-special-strings
   (s "abcdbad" nil)
   (let ((s2 (remove #\b s)))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= s2 "acdad")))
   (let ((s2 (remove #\b s :count 1)))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= s2 "acdbad")))
   (let ((s2 (remove #\b s :count 1 :from-end t)))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= s2 "abcdad"))))
  nil)

(deftest delete-vector.1
  (delete 'a (vector 'b 'c 'd))
  #(b c d))

(deftest delete-vector.2
  (delete 'a (vector 'b 'c 'd) :count -1)
  #(b c d))

(deftest delete-vector.3
  (delete 'a (vector 'a 'b 'c 'd) :count -1)
  #(a b c d))

(deftest delete-string.1
  (delete #\a (copy-seq "abcad"))
  "bcd")

(deftest delete-string.2
  (delete #\a (copy-seq "abcad") :count -1)
  "abcad")

(deftest delete-string.3
  (delete #\a (copy-seq "bcd") :count -1)
  "bcd")

(deftest delete-string.4
  (do-special-strings
   (s "abcdbad" nil)
   (let ((s2 (delete #\b s)))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= s2 "acdad"))))
  nil)

(deftest delete-string.5
  (do-special-strings
   (s "abcdbad" nil)
   (let ((s2 (delete #\b s :count 1)))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= s2 "acdbad"))))
  nil)

(deftest delete-string.6
  (do-special-strings
   (s "abcdbad" nil)
   (let ((s2 (delete #\b s :count 1 :from-end t)))
     (assert (equal (array-element-type s) (array-element-type s2)))
     (assert (string= s2 "abcdad"))))
  nil)

(deftest remove-bit-vector.1
  (remove 0 (copy-seq #*00011101101))
  #*111111)

(deftest remove-bit-vector.2
  (remove 0 (copy-seq #*00011101101) :count -1)
  #*00011101101)

(deftest remove-bit-vector.3
  (remove 0 (copy-seq #*11111) :count -1)
  #*11111)

(deftest delete-bit-vector.1
  (delete 0 (copy-seq #*00011101101))
  #*111111)

(deftest delete-bit-vector.2
  (delete 0 (copy-seq #*00011101101) :count -1)
  #*00011101101)

(deftest delete-bit-vector.3
  (delete 0 (copy-seq #*11111) :count -1)
  #*11111)

;;; test & test-not together is harmless

(defharmless remove-list.test-and-test-not.1
  (remove 'a '(a b c) :test #'eql :test-not #'eql))

(defharmless remove-list.test-and-test-not.2
  (remove 'a '(a b c) :test-not #'eql :test #'eql))

(defharmless remove-vector.test-and-test-not.1
  (remove 'a #(a b c) :test #'eql :test-not #'eql))

(defharmless remove-vector.test-and-test-not.2
  (remove 'a #(a b c) :test-not #'eql :test #'eql))

(defharmless remove-bit-string.test-and-test-not.1
  (remove 0 #*0001100100 :test #'eql :test-not #'eql))

(defharmless remove-bit-string.test-and-test-not.2
  (remove 0 #*0001100100 :test-not #'eql :test #'eql))

(defharmless remove-string.test-and-test-not.1
  (remove #\0 "0001100100" :test #'eql :test-not #'eql))

(defharmless remove-string.test-and-test-not.2
  (remove #\0 "0001100100" :test-not #'eql :test #'eql))


(defharmless delete-list.test-and-test-not.1
  (delete 'a (list 'a 'b 'c) :test #'eql :test-not #'eql))

(defharmless delete-list.test-and-test-not.2
  (delete 'a (list 'a 'b 'c) :test-not #'eql :test #'eql))

(defharmless delete-vector.test-and-test-not.1
  (delete 'a (vector 'a 'b 'c) :test #'eql :test-not #'eql))

(defharmless delete-vector.test-and-test-not.2
  (delete 'a (vector 'a 'b 'c) :test-not #'eql :test #'eql))

(defharmless delete-bit-string.test-and-test-not.1
  (delete 0 (copy-seq #*0001100100) :test #'eql :test-not #'eql))

(defharmless delete-bit-string.test-and-test-not.2
  (delete 0 (copy-seq #*0001100100) :test-not #'eql :test #'eql))

(defharmless delete-string.test-and-test-not.1
  (delete #\0 (copy-seq "0001100100") :test #'eql :test-not #'eql))

(defharmless delete-string.test-and-test-not.2
  (delete #\0 (copy-seq "0001100100") :test-not #'eql :test #'eql))


;;; Const fold tests

(def-fold-test remove.fold.1 (remove 'c '(a b c d e)))
(def-fold-test remove.fold.2 (remove 'c #(a b c d e)))
(def-fold-test remove.fold.3 (remove 1 #*0011011001))
(def-fold-test remove.fold.4 (remove #\c "abcde"))

(def-fold-test remove-if.fold.1 (remove-if 'null '(a b nil d e)))
(def-fold-test remove-if.fold.2 (remove-if #'null #(a b nil d e)))
(def-fold-test remove-if.fold.3 (remove-if 'plusp #*0011011001))
(def-fold-test remove-if.fold.4 (remove-if 'digit-char-p "ab0de"))

(def-fold-test remove-if-not.fold.1 (remove-if-not #'identity '(a b nil d e)))
(def-fold-test remove-if-not.fold.2 (remove-if-not 'identity #(a b nil d e)))
(def-fold-test remove-if-not.fold.3 (remove-if-not #'zerop #*0011011001))
(def-fold-test remove-if-not.fold.4 (remove-if-not #'alpha-char-p "ab-de"))

;;; Order of evaluation tests

(deftest remove.order.1
  (let ((i 0) a b c d e f g h)
    (values
     (remove
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :from-end (progn (setf c (incf i)) t)
      :count (progn (setf d (incf i)) 1)
      :key (progn (setf e (incf i)) #'identity)
      :test (progn (setf f (incf i)) #'eq)
      :start (progn (setf g (incf i)) 0)
      :end (progn (setf h (incf i)) nil))
     i a b c d e f g h))
  (a b c d f) 8 1 2 3 4 5 6 7 8)

(deftest remove.order.2
  (let ((i 0) a b c d e f g h)
    (values
     (remove
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :end (progn (setf c (incf i)) nil)
      :start (progn (setf d (incf i)) 0)
      :test-not (progn (setf e (incf i)) (complement #'eq))
      :key (progn (setf f (incf i)) #'identity)
      :count (progn (setf g (incf i)) 1)
      :from-end (progn (setf h (incf i)) t)
      )
     i a b c d e f g h))
  (a b c d f) 8 1 2 3 4 5 6 7 8)

(deftest delete.order.1
  (let ((i 0) a b c d e f g h)
    (values
     (delete
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :from-end (progn (setf c (incf i)) t)
      :count (progn (setf d (incf i)) 1)
      :key (progn (setf e (incf i)) #'identity)
      :test (progn (setf f (incf i)) #'eq)
      :start (progn (setf g (incf i)) 0)
      :end (progn (setf h (incf i)) nil))
     i a b c d e f g h))
  (a b c d f) 8 1 2 3 4 5 6 7 8)

(deftest delete.order.2
  (let ((i 0) a b c d e f g h)
    (values
     (delete
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :end (progn (setf c (incf i)) nil)
      :start (progn (setf d (incf i)) 0)
      :test-not (progn (setf e (incf i)) (complement #'eq))
      :key (progn (setf f (incf i)) #'identity)
      :count (progn (setf g (incf i)) 1)
      :from-end (progn (setf h (incf i)) t)
      )
     i a b c d e f g h))
  (a b c d f) 8 1 2 3 4 5 6 7 8)

(deftest remove-if.order.1
  (let ((i 0) a b c d e f g)
    (values
     (remove-if
      (progn (setf a (incf i)) #'(lambda (x) (eq x 'a)))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :from-end (progn (setf c (incf i)) t)
      :count (progn (setf d (incf i)) 1)
      :key (progn (setf e (incf i)) #'identity)
      :start (progn (setf f (incf i)) 0)
      :end (progn (setf g (incf i)) nil))
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest remove-if.order.2
  (let ((i 0) a b c d e f g)
    (values
     (remove-if
      (progn (setf a (incf i)) #'(lambda (x) (eq x 'a)))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :end (progn (setf c (incf i)) nil)
      :start (progn (setf d (incf i)) 0)
      :key (progn (setf e (incf i)) #'identity)
      :count (progn (setf f (incf i)) 1)
      :from-end (progn (setf g (incf i)) t)
      )
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest delete-if.order.1
  (let ((i 0) a b c d e f g)
    (values
     (delete-if
      (progn (setf a (incf i)) #'(lambda (x) (eq x 'a)))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :from-end (progn (setf c (incf i)) t)
      :count (progn (setf d (incf i)) 1)
      :key (progn (setf e (incf i)) #'identity)
      :start (progn (setf f (incf i)) 0)
      :end (progn (setf g (incf i)) nil))
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest delete-if.order.2
  (let ((i 0) a b c d e f g)
    (values
     (delete-if
      (progn (setf a (incf i)) #'(lambda (x) (eq x 'a)))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :end (progn (setf c (incf i)) nil)
      :start (progn (setf d (incf i)) 0)
      :key (progn (setf e (incf i)) #'identity)
      :count (progn (setf f (incf i)) 1)
      :from-end (progn (setf g (incf i)) t)
      )
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest remove-if-not.order.1
  (let ((i 0) a b c d e f g)
    (values
     (remove-if-not
      (progn (setf a (incf i)) #'(lambda (x) (not (eq x 'a))))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :from-end (progn (setf c (incf i)) t)
      :count (progn (setf d (incf i)) 1)
      :key (progn (setf e (incf i)) #'identity)
      :start (progn (setf f (incf i)) 0)
      :end (progn (setf g (incf i)) nil))
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest remove-if-not.order.2
  (let ((i 0) a b c d e f g)
    (values
     (remove-if-not
      (progn (setf a (incf i)) #'(lambda (x) (not (eq x 'a))))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :end (progn (setf c (incf i)) nil)
      :start (progn (setf d (incf i)) 0)
      :key (progn (setf e (incf i)) #'identity)
      :count (progn (setf f (incf i)) 1)
      :from-end (progn (setf g (incf i)) t)
      )
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest delete-if-not.order.1
  (let ((i 0) a b c d e f g)
    (values
     (delete-if-not
      (progn (setf a (incf i)) #'(lambda (x) (not (eq x 'a))))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :from-end (progn (setf c (incf i)) t)
      :count (progn (setf d (incf i)) 1)
      :key (progn (setf e (incf i)) #'identity)
      :start (progn (setf f (incf i)) 0)
      :end (progn (setf g (incf i)) nil))
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

(deftest delete-if-not.order.2
  (let ((i 0) a b c d e f g)
    (values
     (delete-if-not
      (progn (setf a (incf i)) #'(lambda (x) (not (eq x 'a))))
      (progn (setf b (incf i)) (list 'a 'b 'c 'd 'a 'f))
      :end (progn (setf c (incf i)) nil)
      :start (progn (setf d (incf i)) 0)
      :key (progn (setf e (incf i)) #'identity)
      :count (progn (setf f (incf i)) 1)
      :from-end (progn (setf g (incf i)) t)
      )
     i a b c d e f g))
  (a b c d f) 7 1 2 3 4 5 6 7)

;;; Randomized tests

(deftest remove-random
  (loop for i from 1 to 2500
        unless (eq (random-test-remove 20) t)
        do (return *remove-fail-args*))
  nil)

(deftest remove-if-random
  (loop for i from 1 to 2500
        unless (eq (random-test-remove-if 20) t)
        do (return *remove-fail-args*))
  nil)

(deftest remove-if-not-random
  (loop for i from 1 to 2500
        unless (eq (random-test-remove-if 20 t) t)
        do (return *remove-fail-args*))
  nil)

(deftest delete-random
  (loop for i from 1 to 2500
        unless (eq (random-test-delete 20) t)
        do (return *remove-fail-args*))
  nil)

(deftest delete-if-random
  (loop for i from 1 to 2500
        unless (eq (random-test-delete-if 20) t)
        do (return *remove-fail-args*))
  nil)

(deftest delete-if-not-random
  (loop for i from 1 to 2500
        unless (eq (random-test-delete-if 20 t) t)
        do (return *remove-fail-args*))
  nil)

;;; Additional tests with KEY = NIL

(deftest remove-if-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove-if #'evenp x :key nil)))
    (and (equalp orig x) y))
  (1 3 1 1 3 7))

(deftest remove-if-list.2
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove-if #'(lambda (y) (eqt y 'a)) x :key nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest remove-if-not-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (remove-if-not #'oddp x :key nil)))
    (and (equalp orig x) y))
  (1 3 1 1 3 7))

(deftest remove-if-not-list.2
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (remove-if-not #'(lambda (y) (not (eqt y 'a))) x :key nil)))
    (and (equalp orig x) y))
  (b c b d c b e))

(deftest delete-if-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (delete-if #'evenp x :key nil)))
    y)
  (1 3 1 1 3 7))

(deftest delete-if-list.2
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (delete-if #'(lambda (y) (eqt y 'a)) x :key nil)))
    y)
  (b c b d c b e))

(deftest delete-if-not-list.1
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (delete-if-not #'oddp x :key nil)))
    y)
  (1 3 1 1 3 7))

(deftest delete-if-not-list.2
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (delete-if-not #'(lambda (y) (not (eqt y 'a))) x :key nil)))
    y)
  (b c b d c b e))

(deftest delete-list.1
  (let* ((orig '(a b c a b d a c b a e))
         (x (copy-seq orig))
         (y (delete 'a x :key nil)))
    y)
  (b c b d c b e))

(deftest delete-list.2
  (let* ((orig '(1 2 3 2 6 1 2 4 1 3 2 7))
         (x (copy-seq orig))
         (y (delete 2 x :key nil)))
    y)
  (1 3 6 1 4 1 3 7))

;;; Keyword tests

(deftest remove.allow-other-keys.1
  (remove 'a '(a b c a d) :allow-other-keys t)
  (b c d))

(deftest remove.allow-other-keys.2
  (remove 'a '(a b c a d) :allow-other-keys nil)
  (b c d))

(deftest remove.allow-other-keys.3
  (remove 'a '(a b c a d) :bad t :allow-other-keys t)
  (b c d))

(deftest remove.allow-other-keys.4
  (remove 'a '(a b c a d) :allow-other-keys t :bad t :bad nil)
  (b c d))

(deftest remove.allow-other-keys.5
  (remove 'a '(a b c a d) :bad1 t :allow-other-keys t :bad2 t
          :allow-other-keys nil :bad3 t)
  (b c d))

(deftest remove.allow-other-keys.6
  (remove 'a '(a b c a d) :allow-other-keys t :from-end t :count 1)
  (a b c d))

(deftest remove.keywords.7
  (remove 'a '(a b c a d) :from-end t :count 1 :from-end nil :count 10)
  (a b c d))


(deftest delete.allow-other-keys.1
  (delete 'a (copy-seq '(a b c a d)) :allow-other-keys t)
  (b c d))

(deftest delete.allow-other-keys.2
  (delete 'a (copy-seq '(a b c a d)) :allow-other-keys nil)
  (b c d))

(deftest delete.allow-other-keys.3
  (delete 'a (copy-seq '(a b c a d)) :bad t :allow-other-keys t)
  (b c d))

(deftest delete.allow-other-keys.4
  (delete 'a (copy-seq '(a b c a d)) :allow-other-keys t :bad t :bad nil)
  (b c d))

(deftest delete.allow-other-keys.5
  (delete 'a (copy-seq '(a b c a d)) :bad1 t :allow-other-keys t :bad2 t
          :allow-other-keys nil :bad3 t)
  (b c d))

(deftest delete.allow-other-keys.6
  (delete 'a (copy-seq '(a b c a d)) :allow-other-keys t :from-end t :count 1)
  (a b c d))

(deftest delete.keywords.7
  (delete 'a (copy-seq '(a b c a d))
          :from-end t :count 1 :from-end nil :count 10)
  (a b c d))



;;; Error cases

(deftest remove.error.1
  (signals-error (remove) program-error)
  t)

(deftest remove.error.2
  (signals-error (remove 'a) program-error)
  t)

(deftest remove.error.3
  (signals-error (remove 'a nil :key) program-error)
  t)

(deftest remove.error.4
  (signals-error (remove 'a nil 'bad t) program-error)
  t)

(deftest remove.error.4a
  (signals-error (remove 'a nil nil t) program-error)
  t)

(deftest remove.error.5
  (signals-error (remove 'a nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest remove.error.6
  (signals-error (remove 'a nil 1 2) program-error)
  t)

(deftest remove.error.7
  (signals-error (remove 'a (list 'a 'b 'c) :test #'identity) program-error)
  t)

(deftest remove.error.8
  (signals-error (remove 'a (list 'a 'b 'c) :test-not #'identity) program-error)
  t)

(deftest remove.error.9
  (signals-error (remove 'a (list 'a 'b 'c) :key #'cons) program-error)
  t)

(deftest remove.error.10
  (signals-error (remove 'a (list 'a 'b 'c) :key #'car) type-error)
  t)

(deftest remove.error.11
  (check-type-error #'(lambda (x) (remove 'a x)) #'sequencep)
  nil)


;;;

(deftest delete.error.1
  (signals-error (delete) program-error)
  t)

(deftest delete.error.2
  (signals-error (delete 'a) program-error)
  t)

(deftest delete.error.3
  (signals-error (delete 'a nil :key) program-error)
  t)

(deftest delete.error.4
  (signals-error (delete 'a nil 'bad t) program-error)
  t)

(deftest delete.error.5
  (signals-error (delete 'a nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest delete.error.6
  (signals-error (delete 'a nil 1 2) program-error)
  t)

(deftest delete.error.7
  (signals-error (delete 'a (list 'a 'b 'c) :test #'identity) program-error)
  t)

(deftest delete.error.8
  (signals-error (delete 'a (list 'a 'b 'c) :test-not #'identity) program-error)
  t)

(deftest delete.error.9
  (signals-error (delete 'a (list 'a 'b 'c) :key #'cons) program-error)
  t)

(deftest delete.error.10
  (signals-error (delete 'a (list 'a 'b 'c) :key #'car) type-error)
  t)

(deftest delete.error.11
  (check-type-error #'(lambda (x) (delete 'a x)) #'sequencep)
  nil)

;;; More specialized string tests

(deftest remove-if-string.1
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (remove-if #'alpha-char-p s)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "1234"))
     (assert (string= s "ab1c23def4"))))
  nil)

(deftest remove-if-string.2
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (remove-if #'alpha-char-p s :count 3)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "123def4"))
     (assert (string= s "ab1c23def4"))))
  nil)

(deftest remove-if-string.3
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (remove-if #'alpha-char-p s :count 3 :from-end t)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "ab1c234"))
     (assert (string= s "ab1c23def4"))))
  nil)

(deftest remove-if-not-string.1
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (remove-if-not #'digit-char-p s)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "1234"))
     (assert (string= s "ab1c23def4"))))
  nil)

(deftest remove-if-not-string.2
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (remove-if-not #'digit-char-p s :count 3)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "123def4"))
     (assert (string= s "ab1c23def4"))))
  nil)

(deftest remove-if-not-string.3
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (remove-if-not #'digit-char-p s :count 3 :from-end t)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "ab1c234"))
     (assert (string= s "ab1c23def4"))))
  nil)


(deftest delete-if-string.1
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (delete-if #'alpha-char-p s)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "1234"))))
  nil)

(deftest delete-if-string.2
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (delete-if #'alpha-char-p s :count 3)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "123def4"))))
  nil)

(deftest delete-if-string.3
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (delete-if #'alpha-char-p s :count 3 :from-end t)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "ab1c234"))))
  nil)

(deftest delete-if-not-string.1
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (delete-if-not #'digit-char-p s)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "1234"))))
  nil)

(deftest delete-if-not-string.2
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (delete-if-not #'digit-char-p s :count 3)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "123def4"))))
  nil)

(deftest delete-if-not-string.3
  (do-special-strings
   (s "ab1c23def4" nil)
   (let ((s2 (delete-if-not #'digit-char-p s :count 3 :from-end t)))
     (assert (equal (array-element-type s)
                    (array-element-type s2)))
     (assert (string= s2 "ab1c234"))))
  nil)
