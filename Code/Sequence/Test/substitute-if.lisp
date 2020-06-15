;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 17:42:04 2002
;;;; Contains: Tests for SUBSTITUTE-IF

(in-package #:sicl-sequence-test)

(deftest substitute-if-list.1
  (let ((x '())) (values (substitute-if 'b #'identity x) x))
  nil nil)

(deftest substitute-if-list.2
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.3
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.4
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 2) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.5
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 1) x))
  (b b a c)
  (a b a c))

(deftest substitute-if-list.6
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 0) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.7
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count -1) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.8
  (let ((x '())) (values (substitute-if 'b (is-eql-p 'a) x :from-end t) x))
  nil nil)

(deftest substitute-if-list.9
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.10
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :from-end t :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.11
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 2 :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-if-list.12
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 1 :from-end t) x))
  (a b b c)
  (a b a c))

(deftest substitute-if-list.13
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 0 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.14
  (let ((x '(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count -1 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-if-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute-if 'x (is-eql-p 'a) x :start i :end j)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :from-end t)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :count c)))
                      (and (equal orig x)
                           (equal y (nconc (make-list i :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-if-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :count c :from-end t)))
                      (and (equal orig x)
                           (equal y (nconc (make-list (- j c) :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 j) :initial-element 'a))))))))
  t)


;;; Tests on vectors

(deftest substitute-if-vector.1
  (let ((x #())) (values (substitute-if 'b (is-eql-p 'a) x) x))
  #() #())

(deftest substitute-if-vector.2
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-vector.3
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count nil) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-vector.4
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 2) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-vector.5
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 1) x))
  #(b b a c)
  #(a b a c))

(deftest substitute-if-vector.6
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 0) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-vector.7
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count -1) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-vector.8
  (let ((x #())) (values (substitute-if 'b (is-eql-p 'a) x :from-end t) x))
  #() #())

(deftest substitute-if-vector.9
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :from-end t) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-vector.10
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :from-end t :count nil) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-vector.11
  (let ((x #(a b a c))) (values (substitute-if 'b (is-eql-p 'a) x :count 2 :from-end t) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-if-vector.12
  (let ((x #(a b a c)))
    (values (substitute-if 'b (is-eql-p 'a) x :count 1 :from-end t) x))
  #(a b b c)
  #(a b a c))

(deftest substitute-if-vector.13
  (let ((x #(a b a c)))
    (values (substitute-if 'b (is-eql-p 'a) x :count 0 :from-end t) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-vector.14
  (let ((x #(a b a c)))
    (values (substitute-if 'b (is-eql-p 'a) x :count -1 :from-end t) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-if-vector.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute-if 'x (is-eql-p 'a) x :start i :end j)))
                (and (equalp orig x)
                     (equalp y
                             (concatenate
                              'simple-vector
                              (make-array i :initial-element 'a)
                              (make-array (- j i) :initial-element 'x)
                              (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-vector.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute-if 'x (is-eql-p 'a) x :start i :end j :from-end t)))
                (and (equalp orig x)
                     (equalp y
                             (concatenate
                              'simple-vector
                              (make-array i :initial-element 'a)
                              (make-array (- j i) :initial-element 'x)
                              (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-if-vector.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute-if 'x (is-eql-p 'a) x
                                             :start i :end j :count c)))
                      (and (equalp orig x)
                           (equalp
                            y (concatenate
                               'simple-vector
                               (make-array i :initial-element 'a)
                               (make-array c :initial-element 'x)
                               (make-array (- 10 (+ i c))
                                           :initial-element 'a))))))))
  t)

(deftest substitute-if-vector.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute-if 'x (is-eql-p 'a) x
                                             :start i :end j :count c
                                             :from-end t)))
                      (and (equalp orig x)
                           (equalp
                            y
                            (concatenate
                             'simple-vector
                             (make-array (- j c) :initial-element 'a)
                             (make-array c :initial-element 'x)
                             (make-array (- 10 j) :initial-element 'a))))))))
  t)

(deftest substitute-if-vector.28
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute-if 'z (is-eql-p 'a) x)))
    result)
  #(z b z c b))

(deftest substitute-if-vector.29
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute-if 'z (is-eql-p 'a) x :from-end t)))
    result)
  #(z b z c b))

(deftest substitute-if-vector.30
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute-if 'z (is-eql-p 'a) x :count 1)))
    result)
  #(z b a c b))

(deftest substitute-if-vector.31
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute-if 'z (is-eql-p 'a) x :from-end t :count 1)))
    result)
  #(a b z c b))

(deftest substitute-if-vector.32
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values
     (substitute-if 'x (is-eql-p 'c) v2 :count 1)
     v1))
  #(d a b x d a b c)
  #(a b c d a b c d a b c d a b c d))

(deftest substitute-if-vector.33
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values
     (substitute-if 'x (is-eql-p 'c) v2 :count 1 :from-end t)
     v1))
  #(d a b c d a b x)
  #(a b c d a b c d a b c d a b c d))

;;; Tests on strings

(deftest substitute-if-string.1
  (let ((x "")) (values (substitute-if #\b (is-eql-p #\a) x) x))
  "" "")

(deftest substitute-if-string.2
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x) x))
  "bbbc"
  "abac")

(deftest substitute-if-string.3
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count nil) x))
  "bbbc"
  "abac")

(deftest substitute-if-string.4
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count 2) x))
  "bbbc"
  "abac")

(deftest substitute-if-string.5
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count 1) x))
  "bbac"
  "abac")

(deftest substitute-if-string.6
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count 0) x))
  "abac"
  "abac")

(deftest substitute-if-string.7
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count -1) x))
  "abac"
  "abac")

(deftest substitute-if-string.8
  (let ((x "")) (values (substitute-if #\b (is-eql-p #\a) x :from-end t) x))
  "" "")

(deftest substitute-if-string.9
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :from-end t) x))
  "bbbc"
  "abac")

(deftest substitute-if-string.10
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :from-end t :count nil) x))
  "bbbc"
  "abac")

(deftest substitute-if-string.11
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count 2 :from-end t) x))
  "bbbc"
  "abac")

(deftest substitute-if-string.12
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count 1 :from-end t) x))
  "abbc"
  "abac")

(deftest substitute-if-string.13
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count 0 :from-end t) x))
  "abac"
  "abac")

(deftest substitute-if-string.14
  (let ((x "abac")) (values (substitute-if #\b (is-eql-p #\a) x :count -1 :from-end t) x))
  "abac"
  "abac")

(deftest substitute-if-string.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (substitute-if #\x (is-eql-p #\a) x :start i :end j)))
                (and (equalp orig x)
                     (equalp y (concatenate 'simple-string
                                           (make-array i :initial-element #\a)
                                           (make-array (- j i) :initial-element #\x)
                                           (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest substitute-if-string.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (substitute-if #\x (is-eql-p #\a) x
                                       :start i :end j :from-end t)))
                (and (equalp orig x)
                     (equalp y
                             (concatenate
                              'simple-string
                              (make-array i :initial-element #\a)
                              (make-array (- j i) :initial-element #\x)
                              (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest substitute-if-string.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (substitute-if #\x (is-eql-p #\a) x
                                             :start i :end j :count c)))
                      (and (equalp orig x)
                           (equalp y
                                   (concatenate
                                    'simple-string
                                    (make-array i :initial-element #\a)
                                    (make-array c :initial-element #\x)
                                    (make-array (- 10 (+ i c))
                                                :initial-element #\a))))))))
  t)

(deftest substitute-if-string.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (substitute-if #\x (is-eql-p #\a) x
                                             :start i :end j :count c
                                             :from-end t)))
                      (and (equalp orig x)
                           (equalp y (concatenate
                                      'simple-string
                                      (make-array (- j c) :initial-element #\a)
                                      (make-array c :initial-element #\x)
                                      (make-array (- 10 j)
                                                  :initial-element #\a))))))))
  t)


(deftest substitute-if-string.28
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute-if #\z (is-eql-p #\a) x)))
    result)
  "zbzcb")

(deftest substitute-if-string.29
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute-if #\z (is-eql-p #\a) x :from-end t)))
    result)
  "zbzcb")

(deftest substitute-if-string.30
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute-if #\z (is-eql-p #\a) x :count 1)))
    result)
  "zbacb")

(deftest substitute-if-string.31
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute-if #\z (is-eql-p #\a) x :from-end t :count 1)))
    result)
  "abzcb")

;;; Tests on bit-vectors

(deftest substitute-if-bit-vector.1
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (substitute-if 0 (is-eql-p 1) x)))
    (and (equalp orig x)
         result))
  #*)

(deftest substitute-if-bit-vector.2
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (substitute-if 1 'zerop x)))
    (and (equalp orig x)
         result))
  #*)

(deftest substitute-if-bit-vector.3
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 0 (is-eql-p 1) x)))
    (and (equalp orig x)
         result))
  #*000000)

(deftest substitute-if-bit-vector.4
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x)))
    (and (equalp orig x)
         result))
  #*111111)

(deftest substitute-if-bit-vector.5
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :start 1)))
    (and (equalp orig x)
         result))
  #*011111)

(deftest substitute-if-bit-vector.6
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 0 (is-eql-p 1) x :start 2 :end nil)))
    (and (equalp orig x)
         result))
  #*010000)

(deftest substitute-if-bit-vector.7
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :end 4)))
    (and (equalp orig x)
         result))
  #*111101)

(deftest substitute-if-bit-vector.8
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 0 (is-eql-p 1) x :end nil)))
    (and (equalp orig x)
         result))
  #*000000)

(deftest substitute-if-bit-vector.9
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 0 (is-eql-p 1) x :end 3)))
    (and (equalp orig x)
         result))
  #*000101)

(deftest substitute-if-bit-vector.10
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 0 (is-eql-p 1) x :start 2 :end 4)))
    (and (equalp orig x)
         result))
  #*010001)

(deftest substitute-if-bit-vector.11
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :start 2 :end 4)))
    (and (equalp orig x)
         result))
  #*011101)

(deftest substitute-if-bit-vector.12
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count 1)))
    (and (equalp orig x)
         result))
  #*110101)

(deftest substitute-if-bit-vector.13
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count 0)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-if-bit-vector.14
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count -1)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-if-bit-vector.15
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count 1 :from-end t)))
    (and (equalp orig x)
         result))
  #*010111)

(deftest substitute-if-bit-vector.16
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count 0 :from-end t)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-if-bit-vector.17
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count -1 :from-end t)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-if-bit-vector.18
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count nil)))
    (and (equalp orig x)
         result))
  #*111111)

(deftest substitute-if-bit-vector.19
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute-if 1 #'zerop x :count nil :from-end t)))
    (and (equalp orig x)
         result))
  #*111111)

(deftest substitute-if-bit-vector.20
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*0000000000)
                           (x (copy-seq orig))
                           (y (substitute-if 1 #'zerop x :start i :end j :count c)))
                      (and (equalp orig x)
                           (equalp y (concatenate
                                      'simple-bit-vector
                                      (make-list i :initial-element 0)
                                      (make-list c :initial-element 1)
                                      (make-list (- 10 (+ i c)) :initial-element 0))))))))
  t)

(deftest substitute-if-bit-vector.21
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*1111111111)
                           (x (copy-seq orig))
                           (y (substitute-if 0 (is-eql-p 1) x :start i :end j :count c :from-end t)))
                      (and (equalp orig x)
                           (equalp y (concatenate
                                      'simple-bit-vector
                                      (make-list (- j c) :initial-element 1)
                                      (make-list c :initial-element 0)
                                      (make-list (- 10 j) :initial-element 1))))))))
  t)

;;; More tests

(deftest substitute-if-list.24
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute-if '(a 10) (is-eql-p 'a) x :key #'car)))
    (and (equal orig x)
         result))
  ((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest substitute-if-list.25
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute-if '(a 10) (is-eql-p 'a) x
                                :key #'car :start 1 :end 5)))
    (and (equal orig x)
         result))
  ((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-if-vector.24
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute-if '(a 10) (is-eql-p 'a) x :key #'car)))
    (and (equalp orig x)
         result))
  #((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest substitute-if-vector.25
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute-if '(a 10) (is-eql-p 'a) x :key #'car :start 1 :end 5)))
    (and (equalp orig x)
         result))
  #((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-if-string.24
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (substitute-if #\a (is-eql-p #\1) x :key #'nextdigit)))
    (and (equalp orig x)
         result))
  "a1a2342a15")

(deftest substitute-if-string.25
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (substitute-if #\a (is-eql-p #\1) x :key #'nextdigit :start 1 :end 6)))
    (and (equalp orig x)
         result))
  "01a2342015")

(deftest substitute-if-string.26
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (substitute-if #\! (is-eql-p #\a) s) "xyz!bcxyz!bc"))
   (assert (string= (substitute-if #\! (is-eql-p #\a) s :count 1) "xyz!bcxyzabc"))
   (assert (string= (substitute-if #\! (is-eql-p #\a) s :count 1 :from-end t) "xyzabcxyz!bc"))
   (assert (string= s "xyzabcxyzabc")))
  nil)

;;; More bit vector tests

(deftest substitute-if-bit-vector.22
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (substitute-if 1 (is-eql-p 1) x :key #'1+)))
    (and (equalp orig x)
         result))
  #*11111111111111111)

(deftest substitute-if-bit-vector.23
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (substitute-if 1 (is-eql-p 1) x :key #'1+ :start 1 :end 10)))
    (and (equalp orig x)
         result))
  #*01111111111010110)

(deftest substitute-if-bit-vector.24
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute-if 1 #'zerop x)))
    result)
  #*11111)

(deftest substitute-if-bit-vector.25
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute-if 1 #'zerop x :from-end t)))
    result)
  #*11111)

(deftest substitute-if-bit-vector.26
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute-if 1 #'zerop x :count 1)))
    result)
  #*11011)

(deftest substitute-if-bit-vector.27
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute-if 1 #'zerop x :from-end t :count 1)))
    result)
  #*01111)

;;; Order of evaluation tests

(deftest substitute-if.order.1
  (let ((i 0) a b c d e f g h)
    (values
     (substitute-if
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) #'null)
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

(deftest substitute-if.order.2
  (let ((i 0) a b c d e f g h)
    (values
     (substitute-if
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) #'null)
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

(deftest substitute-if.allow-other-keys.1
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys t :bad t)
  (1 2 a 3 1 a 3))

(deftest substitute-if.allow-other-keys.2
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :bad t :allow-other-keys t)
  (1 2 a 3 1 a 3))

(deftest substitute-if.allow-other-keys.3
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :allow-other-keys nil :bad t)
  (1 2 a 3 1 a 3))

(deftest substitute-if.allow-other-keys.4
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :bad t
                  :allow-other-keys t :allow-other-keys nil)
  (1 2 a 3 1 a 3))

(deftest substitute-if.allow-other-keys.5
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3)
                  :allow-other-keys t :key #'1-)
  (a 2 0 3 a 0 3))

(deftest substitute-if.keywords.6
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :key #'1- :key #'identity)
  (a 2 0 3 a 0 3))

(deftest substitute-if.allow-other-keys.7
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :bad t :allow-other-keys nil)
  (1 2 a 3 1 a 3))

(deftest substitute-if.allow-other-keys.8
  (substitute-if 'a #'zerop (list 1 2 0 3 1 0 3) :allow-other-keys nil)
  (1 2 a 3 1 a 3))

;;; Constant folding tests

(def-fold-test substitute-if.fold.1 (substitute-if 'z 'null '(a nil b)))
(def-fold-test substitute-if.fold.2 (substitute-if 'z 'null #(a nil b)))
(def-fold-test substitute-if.fold.3 (substitute-if 0 'plusp #*100110))
(def-fold-test substitute-if.fold.4 (substitute-if #\x 'digit-char-p
                                                   "asdf8234n123f"))

;;; Error cases

(deftest substitute-if.error.1
  (signals-error (substitute-if) program-error)
  t)

(deftest substitute-if.error.2
  (signals-error (substitute-if 'a) program-error)
  t)

(deftest substitute-if.error.3
  (signals-error (substitute-if 'a #'null) program-error)
  t)

(deftest substitute-if.error.4
  (signals-error (substitute-if 'a #'null nil 'bad t) program-error)
  t)

(deftest substitute-if.error.5
  (signals-error (substitute-if 'a #'null nil 'bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest substitute-if.error.6
  (signals-error (substitute-if 'a #'null nil :key) program-error)
  t)

(deftest substitute-if.error.7
  (signals-error (substitute-if 'a #'null nil 1 2) program-error)
  t)

(deftest substitute-if.error.8
  (signals-error (substitute-if 'a #'cons (list 'a 'b 'c)) program-error)
  t)

(deftest substitute-if.error.9
  (signals-error (substitute-if 'a #'car (list 'a 'b 'c)) type-error)
  t)

(deftest substitute-if.error.10
  (signals-error (substitute-if 'a #'identity (list 'a 'b 'c)
                                  :key #'car)
                 type-error)
  t)

(deftest substitute-if.error.11
  (signals-error (substitute-if 'a #'identity (list 'a 'b 'c)
                                  :key #'cons)
                 program-error)
  t)

(deftest substitute-if.error.12
  (check-type-error #'(lambda (x) (substitute-if 'a #'not x)) #'sequencep)
  nil)
