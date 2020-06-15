;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 27 16:11:38 2002
;;;; Contains: Tests for REPLACE

(in-package #:sicl-sequence-test)

(deftest replace-list.1
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z))))
    (values (eqt x result) result))
  t
  (x y z d e f g))

(deftest replace-list.2
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 1)))
    (values (eqt x result) result))
  t
  (a x y z e f g))

(deftest replace-list.3
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 4)))
    (values (eqt x result) result))
  t
  (a b c d x y z))

(deftest replace-list.4
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 5)))
    (values (eqt x result) result))
  t
  (a b c d e x y))

(deftest replace-list.5
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 6)))
    (values (eqt x result) result))
  t
  (a b c d e f x))

(deftest replace-list.6
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x #(x y z) :start1 2)))
    (values (eqt x result) result))
  t
  (a b x y z f g))

(deftest replace-list.7
  (replace nil #(x y z))
  nil)

(deftest replace-list.8
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :end1 1)))
    (values (eqt x result) result))
  t
  (x b c d e f g))

(deftest replace-list.9
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 3 :end1 4)))
    (values (eqt x result) result))
  t
  (a b c x e f g))

(deftest replace-list.10
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 0 :end1 5)))
    (values (eqt x result) result))
  t
  (x y z d e f g))


(deftest replace-list.11
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start2 1)))
    (values (eqt x result) result))
  t
  (y z c d e f g))

(deftest replace-list.12
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start2 1 :end1 nil)))
    (values (eqt x result) result))
  t
  (y z c d e f g))

(deftest replace-list.13
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start2 1 :end2 nil)))
    (values (eqt x result) result))
  t
  (y z c d e f g))

(deftest replace-list.14
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  (y b c d e f g))

(deftest replace-list.15
  (let* ((x (copy-seq '(a b c d e f g)))
         (result (replace x '(x y z) :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  (a b c d y f g))

(deftest replace-list.16
  (let* ((x (copy-seq '(a b c d e f)))
         (y #(1 2 3))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  (a 1 2 3 e f))

(deftest replace-list.17
  (let* ((x (copy-seq '(a b c d e f)))
         (y (make-array '(3) :initial-contents '(1 2 3)
                        :fill-pointer t))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  (a 1 2 3 e f))

(deftest replace-list.18
  (let* ((x (copy-seq '(a b c d e f)))
         (y (make-array '(6) :initial-contents '(1 2 3 4 5 6)
                        :fill-pointer 3))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  (a 1 2 3 e f))

(deftest replace-list.19
  (let* ((x (copy-seq '(a b c d e f)))
         (result (replace x x :start1 0 :end1 3 :start2 1 :end2 4)))
    (values (eqt x result) result))
  t
  (b c d d e f))

(deftest replace-list.20
  (let* ((x (copy-seq '(a b c d e f)))
         (result (replace x x :start1 1 :end1 4 :start2 0 :end2 3)))
    (values (eqt x result) result))
  t
  (a a b c e f))


;;; Tests of vectors

(deftest replace-vector.1
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z))))
    (values (eqt x result) result))
  t
  #(x y z d e f g))

(deftest replace-vector.2
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 1)))
    (values (eqt x result) result))
  t
  #(a x y z e f g))

(deftest replace-vector.3
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 4)))
    (values (eqt x result) result))
  t
  #(a b c d x y z))

(deftest replace-vector.4
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 5)))
    (values (eqt x result) result))
  t
  #(a b c d e x y))

(deftest replace-vector.5
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 6)))
    (values (eqt x result) result))
  t
  #(a b c d e f x))

(deftest replace-vector.6
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x '(x y z) :start1 2)))
    (values (eqt x result) result))
  t
  #(a b x y z f g))

(deftest replace-vector.7
  (replace #() #(x y z))
  #())

(deftest replace-vector.8
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :end1 1)))
    (values (eqt x result) result))
  t
  #(x b c d e f g))

(deftest replace-vector.9
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 3 :end1 4)))
    (values (eqt x result) result))
  t
  #(a b c x e f g))

(deftest replace-vector.10
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 0 :end1 5)))
    (values (eqt x result) result))
  t
  #(x y z d e f g))


(deftest replace-vector.11
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start2 1)))
    (values (eqt x result) result))
  t
  #(y z c d e f g))

(deftest replace-vector.12
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start2 1 :end1 nil)))
    (values (eqt x result) result))
  t
  #(y z c d e f g))

(deftest replace-vector.13
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start2 1 :end2 nil)))
    (values (eqt x result) result))
  t
  #(y z c d e f g))

(deftest replace-vector.14
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  #(y b c d e f g))

(deftest replace-vector.15
  (let* ((x (copy-seq #(a b c d e f g)))
         (result (replace x #(x y z) :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  #(a b c d y f g))

(deftest replace-vector.16
  (let* ((x (copy-seq #(a b c d e f)))
         (y '(1 2 3))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  #(a 1 2 3 e f))

(deftest replace-vector.17
  (let* ((x (copy-seq #(a b c d e f)))
         (y (make-array '(3) :initial-contents '(1 2 3)
                        :fill-pointer t))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  #(a 1 2 3 e f))

(deftest replace-vector.18
  (let* ((x (copy-seq #(a b c d e f)))
         (y (make-array '(6) :initial-contents '(1 2 3 4 5 6)
                        :fill-pointer 3))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  #(a 1 2 3 e f))

(deftest replace-vector.19
  (let* ((x (copy-seq #(a b c d e f)))
         (result (replace x x :start1 0 :end1 3 :start2 1 :end2 4)))
    (values (eqt x result) result))
  t
  #(b c d d e f))

(deftest replace-vector.21
  (let* ((x (copy-seq #(a b c d e f)))
         (result (replace x x :start1 1 :end1 4 :start2 0 :end2 3)))
    (values (eqt x result) result))
  t
  #(a a b c e f))

;;; tests on bit vectors

(deftest replace-bit-vector.1
  (let* ((x (copy-seq #*1101001))
         (result (replace x #*011)))
    (values (eqt x result) result))
  t
  #*0111001)

(deftest replace-bit-vector.2
  (let* ((x (copy-seq #*1101001))
         (result (replace x #*011 :start1 1)))
    (values (eqt x result) result))
  t
  #*1011001)

(deftest replace-bit-vector.3
  (let* ((x (copy-seq #*1101001))
         (result (replace x #*011 :start1 4)))
    (values (eqt x result) result))
  t
  #*1101011)

(deftest replace-bit-vector.4
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*111 :start1 5)))
    (values (eqt x result) result))
  t
  #*0000011)

(deftest replace-bit-vector.5
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*100 :start1 6)))
    (values (eqt x result) result))
  t
  #*0000001)

(deftest replace-bit-vector.6
  (let* ((x (copy-seq #*0000000))
         (result (replace x '(1 1 1) :start1 2)))
    (values (eqt x result) result))
  t
  #*0011100)

(deftest replace-bit-vector.7
  (replace #* #*111)
  #*)

(deftest replace-bit-vector.8
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*111 :end1 1)))
    (values (eqt x result) result))
  t
  #*1000000)

(deftest replace-bit-vector.9
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*110 :start1 3 :end1 4)))
    (values (eqt x result) result))
  t
  #*0001000)

(deftest replace-bit-vector.10
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*111 :start1 0 :end1 5)))
    (values (eqt x result) result))
  t
  #*1110000)


(deftest replace-bit-vector.11
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*011 :start2 1)))
    (values (eqt x result) result))
  t
  #*1100000)

(deftest replace-bit-vector.12
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*011 :start2 1 :end1 nil)))
    (values (eqt x result) result))
  t
  #*1100000)

(deftest replace-bit-vector.13
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*011 :start2 1 :end2 nil)))
    (values (eqt x result) result))
  t
  #*1100000)

(deftest replace-bit-vector.14
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*011 :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  #*1000000)

(deftest replace-bit-vector.15
  (let* ((x (copy-seq #*0000000))
         (result (replace x #*011 :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  #*0000100)

(deftest replace-bit-vector.16
  (let* ((x (copy-seq #*001011))
         (y '(1 0 1))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  #*010111)

(deftest replace-bit-vector.17
  (let* ((x (copy-seq #*001011))
         (y (make-array '(3) :initial-contents '(1 0 1)
                        :fill-pointer t :element-type 'bit))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  #*010111)

(deftest replace-bit-vector.18
  (let* ((x (copy-seq #*001011))
         (y (make-array '(6) :initial-contents '(1 0 1 0 0 1)
                        :fill-pointer 3
                        :element-type 'bit))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  #*010111)

(deftest replace-bit-vector.19
  (let* ((x (copy-seq #*001011))
         (result (replace x x :start1 0 :end1 3 :start2 1 :end2 4)))
    (values (eqt x result) result))
  t
  #*010011)

(deftest replace-bit-vector.21
  (let* ((x (copy-seq #*001011))
         (result (replace x x :start1 1 :end1 4 :start2 0 :end2 3)))
    (values (eqt x result) result))
  t
  #*000111)

;;; Tests on strings

(deftest replace-string.1
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz")))
    (values (eqt x result) result))
  t
  "xyzdefg")

(deftest replace-string.2
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 1)))
    (values (eqt x result) result))
  t
  "axyzefg")

(deftest replace-string.3
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 4)))
    (values (eqt x result) result))
  t
  "abcdxyz")

(deftest replace-string.4
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 5)))
    (values (eqt x result) result))
  t
  "abcdexy")

(deftest replace-string.5
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 6)))
    (values (eqt x result) result))
  t
  "abcdefx")

(deftest replace-string.6
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x '(#\x #\y #\z) :start1 2)))
    (values (eqt x result) result))
  t
  "abxyzfg")

(deftest replace-string.7
  (replace "" "xyz")
  "")

(deftest replace-string.8
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :end1 1)))
    (values (eqt x result) result))
  t
  "xbcdefg")

(deftest replace-string.9
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 3 :end1 4)))
    (values (eqt x result) result))
  t
  "abcxefg")

(deftest replace-string.10
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 0 :end1 5)))
    (values (eqt x result) result))
  t
  "xyzdefg")


(deftest replace-string.11
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start2 1)))
    (values (eqt x result) result))
  t
  "yzcdefg")

(deftest replace-string.12
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start2 1 :end1 nil)))
    (values (eqt x result) result))
  t
  "yzcdefg")

(deftest replace-string.13
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start2 1 :end2 nil)))
    (values (eqt x result) result))
  t
  "yzcdefg")

(deftest replace-string.14
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  "ybcdefg")

(deftest replace-string.15
  (let* ((x (copy-seq "abcdefg"))
         (result (replace x "xyz" :start1 4 :end1 5 :start2 1 :end2 2)))
    (values (eqt x result) result))
  t
  "abcdyfg")

(deftest replace-string.16
  (let* ((x (copy-seq "abcdef"))
         (y (coerce "123" 'list))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  "a123ef")

(deftest replace-string.17
  (let* ((x (copy-seq "abcdef"))
         (y (make-array '(3) :initial-contents '(#\1 #\2 #\3)
                        :fill-pointer t :element-type 'character))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  "a123ef")

(deftest replace-string.18
  (let* ((x (copy-seq "abcdef"))
         (y (make-array '(6) :initial-contents "123456"
                        :fill-pointer 3
                        :element-type 'character))
         (result (replace x y :start1 1)))
    (values (eqt x result) result))
  t
  "a123ef")

(deftest replace-string.19
  (let* ((x (copy-seq "abcdef"))
         (result (replace x x :start1 0 :end1 3 :start2 1 :end2 4)))
    (values (eqt x result) result))
  t
  "bcddef")

(deftest replace-string.21
  (let* ((x (copy-seq "abcdef"))
         (result (replace x x :start1 1 :end1 4 :start2 0 :end2 3)))
    (values (eqt x result) result))
  t
  "aabcef")

(deftest replace-string.22
  (do-special-strings
   (s "abcdefg" nil)
   (assert (eq s (replace s "XYZ")))
   (assert (string= s "XYZdefg")))
  nil)

(deftest replace-string.23
  (do-special-strings
   (s "abcdefg" nil)
   (assert (eq s (replace s "XYZ" :start1 1)))
   (assert (string= s "aXYZefg")))
  nil)

(deftest replace-string.24
  (do-special-strings
   (s "abcdefg" nil)
   (assert (eq s (replace s "XYZ" :start1 1 :end2 2)))
   (assert (string= s "aXYdefg")))
  nil)

(deftest replace-string.25
  (do-special-strings
   (s "abcdefg" nil)
   (assert (eq s (replace s "XYZ" :end1 2)))
   (assert (string= s "XYcdefg")))
  nil)

(deftest replace-string.26
  (do-special-strings
   (s "abcdefg" nil)
   (assert (eq s (replace s "XYZ" :start2 1)))
   (assert (string= s "YZcdefg")))
  nil)





;;; Order of evaluation tests

(deftest replace.order.1
  (let ((i 0) a b)
    (values
     (replace (progn (setf a (incf i)) (list 'a 'b 'c))
              (progn (setf b (incf i)) (list 'e 'f)))
     i a b))
  (e f c) 2 1 2)

(deftest replace.order.2
  (let ((i 0) a b c d e f)
    (values
     (replace (progn (setf a (incf i)) (list 'a 'b 'c))
              (progn (setf b (incf i)) (list 'e 'f))
              :start1 (progn (setf c (incf i)) 1)
              :end1 (progn (setf d (incf i)) 3)
              :start2 (progn (setf e (incf i)) 0)
              :end2 (progn (setf f (incf i)) 2)
              )
     i a b c d e f))
  (a e f) 6 1 2 3 4 5 6)

(deftest replace.order.3
  (let ((i 0) a b c d e f)
    (values
     (replace (progn (setf a (incf i)) (list 'a 'b 'c))
              (progn (setf b (incf i)) (list 'e 'f))
              :end2 (progn (setf c (incf i)) 2)
              :start2 (progn (setf d (incf i)) 0)
              :end1 (progn (setf e (incf i)) 3)
              :start1 (progn (setf f (incf i)) 1)
              )
     i a b c d e f))
  (a e f) 6 1 2 3 4 5 6)

;;; Keyword tests

(deftest replace.allow-other-keys.1
  (replace (copy-seq "abcdefg") "xyz" :allow-other-keys t)
  "xyzdefg")

(deftest replace.allow-other-keys.2
  (replace (copy-seq "abcdefg") "xyz" :allow-other-keys nil)
  "xyzdefg")

(deftest replace.allow-other-keys.3
  (replace (copy-seq "abcdefg") "xyz" :allow-other-keys t :bad t)
  "xyzdefg")

(deftest replace.allow-other-keys.4
  (replace (copy-seq "abcdefg") "xyz" :bad t :allow-other-keys t)
  "xyzdefg")

(deftest replace.allow-other-keys.5
  (replace (copy-seq "abcdefg") "xyz" :bad1 t :allow-other-keys t
           :bad2 t :allow-other-keys nil :bad3 nil)
  "xyzdefg")

(deftest replace.allow-other-keys.6
  (replace (copy-seq "abcdefg") "xyz" :allow-other-keys t :start1 1)
  "axyzefg")

(deftest replace.keywords.7
  (replace (copy-seq "abcdefg") "xyz" :start1 0 :start2 0 :end1 3 :end2 3
           :start1 1 :start2 1 :end1 2 :end1 2)
  "xyzdefg")




;;; Error cases

(deftest replace.error.1
  (signals-error (replace) program-error)
  t)

(deftest replace.error.2
  (signals-error (replace nil) program-error)
  t)

(deftest replace.error.3
  (signals-error (replace nil nil :start) program-error)
  t)

(deftest replace.error.4
  (signals-error (replace nil nil 'bad t) program-error)
  t)

(deftest replace.error.5
  (signals-error (replace nil nil :allow-other-keys nil 'bad t) program-error)
  t)

(deftest replace.error.6
  (signals-error (replace nil nil 1 2) program-error)
  t)

