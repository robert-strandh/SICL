;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 19:00:55 2002
;;;; Contains: Tests for NSUBSTITUTE-IF-NOT

(in-package #:sicl-sequence-test)

(deftest nsubstitute-if-not-list.1
  (nsubstitute-if-not 'b 'identity nil)
  nil)

(deftest nsubstitute-if-not-list.2
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x) x)
  (b b b c))

(deftest nsubstitute-if-not-list.3
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count nil))
  (b b b c))

(deftest nsubstitute-if-not-list.4
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 2))
  (b b b c))

(deftest nsubstitute-if-not-list.5
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 1))
  (b b a c))

(deftest nsubstitute-if-not-list.6
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 0))
  (a b a c))

(deftest nsubstitute-if-not-list.7
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count -1))
  (a b a c))

(deftest nsubstitute-if-not-list.8
  (nsubstitute-if-not 'b (is-not-eql-p 'a) nil :from-end t)
  nil)

(deftest nsubstitute-if-not-list.9
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :from-end t))
  (b b b c))

(deftest nsubstitute-if-not-list.10
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :from-end t :count nil))
  (b b b c))

(deftest nsubstitute-if-not-list.11
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 2 :from-end t))
  (b b b c))

(deftest nsubstitute-if-not-list.12
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 1 :from-end t))
  (a b b c))

(deftest nsubstitute-if-not-list.13
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 0 :from-end t))
  (a b a c))

(deftest nsubstitute-if-not-list.14
  (let ((x (copy-seq '(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count -1 :from-end t))
  (a b a c))

(deftest nsubstitute-if-not-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-if-not-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j :from-end t)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-if-not-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j :count c)))
                      (equal y (nconc (make-list i :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 (+ i c)) :initial-element 'a)))))))
  t)

(deftest nsubstitute-if-not-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j :count c :from-end t)))
                      (equal y (nconc (make-list (- j c) :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 j) :initial-element 'a)))))))
  t)

;;; Tests on vectors

(deftest nsubstitute-if-not-vector.1
  (let ((x #())) (nsubstitute-if-not 'b (is-not-eql-p 'a) x))
  #())

(deftest nsubstitute-if-not-vector.2
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x))
  #(b b b c))

(deftest nsubstitute-if-not-vector.3
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count nil) x)
  #(b b b c))

(deftest nsubstitute-if-not-vector.4
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 2))
  #(b b b c))

(deftest nsubstitute-if-not-vector.5
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 1))
  #(b b a c))

(deftest nsubstitute-if-not-vector.6
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 0))
  #(a b a c))

(deftest nsubstitute-if-not-vector.7
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count -1))
  #(a b a c))

(deftest nsubstitute-if-not-vector.8
  (let ((x #())) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :from-end t))
  #())

(deftest nsubstitute-if-not-vector.9
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :from-end t))
  #(b b b c))

(deftest nsubstitute-if-not-vector.10
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :from-end t :count nil))
  #(b b b c))

(deftest nsubstitute-if-not-vector.11
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 2 :from-end t))
  #(b b b c))

(deftest nsubstitute-if-not-vector.12
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 1 :from-end t))
  #(a b b c))

(deftest nsubstitute-if-not-vector.13
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count 0 :from-end t))
  #(a b a c))

(deftest nsubstitute-if-not-vector.14
  (let ((x (copy-seq #(a b a c)))) (nsubstitute-if-not 'b (is-not-eql-p 'a) x :count -1 :from-end t))
  #(a b a c))

(deftest nsubstitute-if-not-vector.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j)))
                (equalp y (concatenate 'simple-vector
                                       (make-array i :initial-element 'a)
                                       (make-array (- j i) :initial-element 'x)
                                       (make-array (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-if-not-vector.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j :from-end t)))
                (equalp y (concatenate 'simple-vector
                                       (make-array i :initial-element 'a)
                                       (make-array (- j i) :initial-element 'x)
                                       (make-array (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-if-not-vector.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j :count c)))
                      (equalp y (concatenate 'simple-vector
                                             (make-array i :initial-element 'a)
                                             (make-array c :initial-element 'x)
                                             (make-array (- 10 (+ i c)) :initial-element 'a)))))))
  t)

(deftest nsubstitute-if-not-vector.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not 'x (is-not-eql-p 'a) x :start i :end j :count c :from-end t)))
                      (equalp y (concatenate 'simple-vector
                                             (make-array (- j c) :initial-element 'a)
                                             (make-array c :initial-element 'x)
                                             (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest nsubstitute-if-not-vector.28
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute-if-not 'z (is-not-eql-p 'a) x)))
    result)
  #(z b z c b))

(deftest nsubstitute-if-not-vector.29
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute-if-not 'z (is-not-eql-p 'a) x :from-end t)))
    result)
  #(z b z c b))

(deftest nsubstitute-if-not-vector.30
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute-if-not 'z (is-not-eql-p 'a) x :count 1)))
    result)
  #(z b a c b))

(deftest nsubstitute-if-not-vector.31
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute-if-not 'z (is-not-eql-p 'a) x
                                    :from-end t :count 1)))
    result)
  #(a b z c b))

(deftest nsubstitute-if-not-vector.32
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (nsubstitute-if-not 'x (is-not-eql-p 'c) v2 :count 1))
  #(d a b x d a b c))

(deftest nsubstitute-if-not-vector.33
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (nsubstitute-if-not 'x (is-not-eql-p 'c) v2 :count 1 :from-end t))
  #(d a b c d a b x))

;;; Tests on strings

(deftest nsubstitute-if-not-string.1
  (let ((x "")) (nsubstitute-if-not #\b (is-not-eql-p #\a) x))
  "")

(deftest nsubstitute-if-not-string.2
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x))
  "bbbc")

(deftest nsubstitute-if-not-string.3
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count nil))
  "bbbc")

(deftest nsubstitute-if-not-string.4
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count 2))
  "bbbc")

(deftest nsubstitute-if-not-string.5
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count 1))
  "bbac")

(deftest nsubstitute-if-not-string.6
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count 0))
  "abac")

(deftest nsubstitute-if-not-string.7
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count -1))
  "abac")

(deftest nsubstitute-if-not-string.8
  (let ((x "")) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :from-end t))
  "")

(deftest nsubstitute-if-not-string.9
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :from-end t))
  "bbbc")

(deftest nsubstitute-if-not-string.10
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :from-end t :count nil))
  "bbbc")

(deftest nsubstitute-if-not-string.11
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count 2 :from-end t))
  "bbbc")

(deftest nsubstitute-if-not-string.12
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count 1 :from-end t))
  "abbc")

(deftest nsubstitute-if-not-string.13
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count 0 :from-end t))
  "abac")

(deftest nsubstitute-if-not-string.14
  (let ((x (copy-seq "abac"))) (nsubstitute-if-not #\b (is-not-eql-p #\a) x :count -1 :from-end t))
  "abac")

(deftest nsubstitute-if-not-string.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (nsubstitute-if-not #\x (is-not-eql-p #\a) x :start i :end j)))
                (equalp y (concatenate 'simple-string
                                       (make-array i :initial-element #\a)
                                       (make-array (- j i) :initial-element #\x)
                                       (make-array (- 10 j) :initial-element #\a))))))
  t)

(deftest nsubstitute-if-not-string.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (nsubstitute-if-not #\x (is-not-eql-p #\a) x :start i :end j :from-end t)))
                (equalp y (concatenate 'simple-string
                                       (make-array i :initial-element #\a)
                                       (make-array (- j i) :initial-element #\x)
                                       (make-array (- 10 j) :initial-element #\a))))))
  t)

(deftest nsubstitute-if-not-string.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not #\x (is-not-eql-p #\a) x :start i :end j :count c)))
                      (equalp y (concatenate 'simple-string
                                             (make-array i :initial-element #\a)
                                             (make-array c :initial-element #\x)
                                             (make-array (- 10 (+ i c)) :initial-element #\a)))))))
  t)

(deftest nsubstitute-if-not-string.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not #\x (is-not-eql-p #\a) x :start i :end j :count c :from-end t)))
                      (equalp y (concatenate 'simple-string
                                             (make-array (- j c) :initial-element #\a)
                                             (make-array c :initial-element #\x)
                                             (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest nsubstitute-if-not-string.28
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute-if-not #\z (is-not-eql-p #\a) x)))
    result)
  "zbzcb")

(deftest nsubstitute-if-not-string.29
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute-if-not #\z (is-not-eql-p #\a) x :from-end t)))
    result)
  "zbzcb")

(deftest nsubstitute-if-not-string.30
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute-if-not #\z (is-not-eql-p #\a) x :count 1)))
    result)
  "zbacb")

(deftest nsubstitute-if-not-string.31
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute-if-not #\z (is-not-eql-p #\a) x
                                    :from-end t :count 1)))
    result)
  "abzcb")

(deftest nsubstitute-if-not-string.32
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (nsubstitute-if-not #\! (is-not-eql-p #\a) s) "xyz!bcxyz!bc")))
  nil)

(deftest nsubstitute-if-not-string.33
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (nsubstitute-if-not #\! (is-not-eql-p #\a) s :count 1) "xyz!bcxyzabc")))
  nil)

(deftest nsubstitute-if-not-string.34
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (nsubstitute-if-not #\! (is-not-eql-p #\a) s :count 1 :from-end t) "xyzabcxyz!bc")))
  nil)


;;; Tests on bit-vectors

(deftest nsubstitute-if-not-bit-vector.1
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 0 (is-not-eql-p 1) x)))
    result)
  #*)

(deftest nsubstitute-if-not-bit-vector.2
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x)))
    result)
  #*)

(deftest nsubstitute-if-not-bit-vector.3
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 0 (is-not-eql-p 1) x)))
    result)
  #*000000)

(deftest nsubstitute-if-not-bit-vector.4
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x)))
    result)
  #*111111)

(deftest nsubstitute-if-not-bit-vector.5
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :start 1)))
    result)
  #*011111)

(deftest nsubstitute-if-not-bit-vector.6
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 0 (is-not-eql-p 1) x :start 2 :end nil)))
    result)
  #*010000)

(deftest nsubstitute-if-not-bit-vector.7
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :end 4)))
    result)
  #*111101)

(deftest nsubstitute-if-not-bit-vector.8
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 0 (is-not-eql-p 1) x :end nil)))
    result)
  #*000000)

(deftest nsubstitute-if-not-bit-vector.9
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 0 (is-not-eql-p 1) x :end 3)))
    result)
  #*000101)

(deftest nsubstitute-if-not-bit-vector.10
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 0 (is-not-eql-p 1) x :start 2 :end 4)))
    result)
  #*010001)

(deftest nsubstitute-if-not-bit-vector.11
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :start 2 :end 4)))
    result)
  #*011101)

(deftest nsubstitute-if-not-bit-vector.12
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count 1)))
    result)
  #*110101)

(deftest nsubstitute-if-not-bit-vector.13
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count 0)))
    result)
  #*010101)

(deftest nsubstitute-if-not-bit-vector.14
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count -1)))
    result)
  #*010101)

(deftest nsubstitute-if-not-bit-vector.15
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count 1 :from-end t)))
    result)
  #*010111)

(deftest nsubstitute-if-not-bit-vector.16
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count 0 :from-end t)))
    result)
  #*010101)

(deftest nsubstitute-if-not-bit-vector.17
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count -1 :from-end t)))
    result)
  #*010101)

(deftest nsubstitute-if-not-bit-vector.18
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count nil)))
    result)
  #*111111)

(deftest nsubstitute-if-not-bit-vector.19
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 0) x :count nil :from-end t)))
    result)
  #*111111)

(deftest nsubstitute-if-not-bit-vector.20
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*0000000000)
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not 1 (is-not-eql-p 0) x :start i :end j :count c)))
                      (equalp y (concatenate
                                 'simple-bit-vector
                                 (make-list i :initial-element 0)
                                 (make-list c :initial-element 1)
                                 (make-list (- 10 (+ i c)) :initial-element 0)))))))
  t)

(deftest nsubstitute-if-not-bit-vector.21
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*1111111111)
                           (x (copy-seq orig))
                           (y (nsubstitute-if-not 0 (is-not-eql-p 1) x :start i :end j :count c :from-end t)))
                      (equalp y (concatenate
                                 'simple-bit-vector
                                 (make-list (- j c) :initial-element 1)
                                 (make-list c :initial-element 0)
                                 (make-list (- 10 j) :initial-element 1)))))))
  t)


;;; More tests

(deftest nsubstitute-if-not-list.24
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute-if-not '(a 10) (is-not-eql-p 'a) x :key #'car)))
    result)
  ((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest nsubstitute-if-not-list.25
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute-if-not '(a 10) (is-not-eql-p 'a) x
                                :key #'car :start 1 :end 5)))
    result)
  ((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest nsubstitute-if-not-vector.24
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute-if-not '(a 10) (is-not-eql-p 'a) x :key #'car)))
    result)
  #((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest nsubstitute-if-not-vector.25
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute-if-not '(a 10) (is-not-eql-p 'a) x :key #'car :start 1 :end 5)))
    result)
  #((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest nsubstitute-if-not-string.24
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (nsubstitute-if-not #\a (is-not-eql-p #\1) x :key #'nextdigit)))
    result)
  "a1a2342a15")

(deftest nsubstitute-if-not-string.25
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (nsubstitute-if-not #\a (is-not-eql-p #\1) x :key #'nextdigit :start 1 :end 6)))
    result)
  "01a2342015")

(deftest nsubstitute-if-not-bit-vector.26
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 1) x :key #'1+)))
    result)
  #*11111111111111111)

(deftest nsubstitute-if-not-bit-vector.27
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (nsubstitute-if-not 1 (is-not-eql-p 1) x :key #'1+ :start 1 :end 10)))
    result)
  #*01111111111010110)

(deftest nsubstitute-if-not-bit-vector.30
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute-if-not 1 #'onep x)))
    result)
  #*11111)

(deftest nsubstitute-if-not-bit-vector.31
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute-if-not 1 #'onep x :from-end t)))
    result)
  #*11111)

(deftest nsubstitute-if-not-bit-vector.32
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute-if-not 1 #'onep x :count 1)))
    result)
  #*11011)

(deftest nsubstitute-if-not-bit-vector.33
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute-if-not 1 #'onep x :from-end t :count 1)))
    result)
  #*01111)

(deftest nsubstitute-if-not.order.1
  (let ((i 0) a b c d e f g h)
    (values
     (nsubstitute-if-not
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) #'identity)
      (progn (setf c (incf i)) (list nil 1 2 nil 3 4 nil 5))
      :count (progn (setf d (incf i)) 2)
      :start (progn (setf e (incf i)) 0)
      :end (progn (setf f (incf i)) 7)
      :key (progn (setf g (incf i)) #'identity)
      :from-end (setf h (incf i))
      )
     i a b c d e f g h))
  (nil 1 2 a 3 4 a 5)
  8 1 2 3 4 5 6 7 8)

(deftest nsubstitute-if-not.order.2
  (let ((i 0) a b c d e f g h)
    (values
     (nsubstitute-if-not
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) #'identity)
      (progn (setf c (incf i)) (list nil 1 2 nil 3 4 nil 5))
      :from-end (setf h (incf i))
      :key (progn (setf g (incf i)) #'identity)
      :end (progn (setf f (incf i)) 7)
      :start (progn (setf e (incf i)) 0)
      :count (progn (setf d (incf i)) 2)
      )
     i a b c d e f g h))
  (nil 1 2 a 3 4 a 5)
  8 1 2 3 8 7 6 5 4)


;;; Keyword tests

(deftest nsubstitute-if-not.allow-other-keys.1
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3)
                      :allow-other-keys t :bad t)
  (a a 0 a a 0 a))

(deftest nsubstitute-if-not.allow-other-keys.2
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3)
                      :bad t :allow-other-keys t)
  (a a 0 a a 0 a))

(deftest nsubstitute-if-not.allow-other-keys.3
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :allow-other-keys nil :bad t)
  (a a 0 a a 0 a))

(deftest nsubstitute-if-not.allow-other-keys.4
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3) :bad t
                  :allow-other-keys t :allow-other-keys nil)
  (a a 0 a a 0 a))

(deftest nsubstitute-if-not.allow-other-keys.5
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3)
                  :allow-other-keys t :key #'1-)
  (1 a a a 1 a a))

(deftest nsubstitute-if-not.keywords.6
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3)
                      :key #'1- :key #'identity)
  (1 a a a 1 a a))

(deftest nsubstitute-if-not.allow-other-keys.7
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :bad t :allow-other-keys nil)
  (a a 0 a a 0 a))

(deftest nsubstitute-if-not.allow-other-keys.8
  (nsubstitute-if-not 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys nil)
  (a a 0 a a 0 a))


;;; Error cases

(deftest nsubstitute-if-not.error.1
  (signals-error (nsubstitute-if-not) program-error)
  t)

(deftest nsubstitute-if-not.error.2
  (signals-error (nsubstitute-if-not 'a) program-error)
  t)

(deftest nsubstitute-if-not.error.3
  (signals-error (nsubstitute-if-not 'a #'null) program-error)
  t)

(deftest nsubstitute-if-not.error.4
  (signals-error (nsubstitute-if-not 'a #'null nil 'bad t) program-error)
  t)

(deftest nsubstitute-if-not.error.5
  (signals-error (nsubstitute-if-not 'a #'null nil
                                      'bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest nsubstitute-if-not.error.6
  (signals-error (nsubstitute-if-not 'a #'null nil :key) program-error)
  t)

(deftest nsubstitute-if-not.error.7
  (signals-error (nsubstitute-if-not 'a #'null nil 1 2) program-error)
  t)

(deftest nsubstitute-if-not.error.8
  (signals-error (nsubstitute-if-not 'a #'cons (list 'a 'b 'c)) program-error)
  t)

(deftest nsubstitute-if-not.error.9
  (signals-error (nsubstitute-if-not 'a #'car (list 'a 'b 'c)) type-error)
  t)

(deftest nsubstitute-if-not.error.10
  (signals-error (nsubstitute-if-not 'a #'identity (list 'a 'b 'c)
                                  :key #'car)
                 type-error)
  t)

(deftest nsubstitute-if-not.error.11
  (signals-error (nsubstitute-if-not 'a #'identity (list 'a 'b 'c)
                                  :key #'cons)
                 program-error)
  t)

(deftest nsubstitute-if-not.error.12
  (check-type-error #'(lambda (x) (nsubstitute-if-not 1 #'null x)) #'sequencep)
  nil)
