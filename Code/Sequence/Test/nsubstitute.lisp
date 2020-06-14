;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Aug 31 16:56:48 2002
;;;; Contains: Tests for NSUBSTITUTE

(in-package #:sicl-sequence-test)

(deftest nsubstitute-list.1
  (nsubstitute 'b 'a nil)
  nil)

(deftest nsubstitute-list.2
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x) x)
  (b b b c))

(deftest nsubstitute-list.3
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count nil))
  (b b b c))

(deftest nsubstitute-list.4
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 2))
  (b b b c))

(deftest nsubstitute-list.5
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 1))
  (b b a c))

(deftest nsubstitute-list.6
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 0))
  (a b a c))

(deftest nsubstitute-list.7
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count -1))
  (a b a c))

(deftest nsubstitute-list.8
  (nsubstitute 'b 'a nil :from-end t)
  nil)

(deftest nsubstitute-list.9
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :from-end t))
  (b b b c))

(deftest nsubstitute-list.10
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :from-end t :count nil))
  (b b b c))

(deftest nsubstitute-list.11
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 2 :from-end t))
  (b b b c))

(deftest nsubstitute-list.12
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 1 :from-end t))
  (a b b c))

(deftest nsubstitute-list.13
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count 0 :from-end t))
  (a b a c))

(deftest nsubstitute-list.14
  (let ((x (copy-seq '(a b a c)))) (nsubstitute 'b 'a x :count -1 :from-end t))
  (a b a c))

(deftest nsubstitute-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute 'x 'a x :start i :end j)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute 'x 'a x :start i :end j :from-end t)))
                (equal y (nconc (make-list i :initial-element 'a)
                                (make-list (- j i) :initial-element 'x)
                                (make-list (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute 'x 'a x :start i :end j :count c)))
                      (equal y (nconc (make-list i :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 (+ i c)) :initial-element 'a)))))))
  t)

(deftest nsubstitute-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute 'x 'a x :start i :end j :count c :from-end t)))
                      (equal y (nconc (make-list (- j c) :initial-element 'a)
                                      (make-list c :initial-element 'x)
                                      (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest nsubstitute-list.19
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    result)
  (1 2 x x x x x 8 9))

(deftest nsubstitute-list.20
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    result)
  (1 2 x 4 5 6 7 8 9))


(deftest nsubstitute-list.21
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    result)
  (1 2 3 4 5 6 7 x 9))

(deftest nsubstitute-list.22
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    result)
  (1 2 x 4 5 6 7 8 9))


(deftest nsubstitute-list.23
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    result)
  (1 2 3 4 5 6 7 x 9))

;;; Tests on vectors

(deftest nsubstitute-vector.1
  (let ((x #())) (values (nsubstitute 'b 'a x) x))
  #() #())

(deftest nsubstitute-vector.2
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x))
  #(b b b c))

(deftest nsubstitute-vector.3
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count nil) x)
  #(b b b c))

(deftest nsubstitute-vector.4
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 2))
  #(b b b c))

(deftest nsubstitute-vector.5
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 1))
  #(b b a c))

(deftest nsubstitute-vector.6
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 0))
  #(a b a c))

(deftest nsubstitute-vector.7
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count -1))
  #(a b a c))

(deftest nsubstitute-vector.8
  (let ((x #())) (nsubstitute 'b 'a x :from-end t))
  #())

(deftest nsubstitute-vector.9
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :from-end t))
  #(b b b c))

(deftest nsubstitute-vector.10
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :from-end t :count nil))
  #(b b b c))

(deftest nsubstitute-vector.11
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 2 :from-end t))
  #(b b b c))

(deftest nsubstitute-vector.12
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 1 :from-end t))
  #(a b b c))

(deftest nsubstitute-vector.13
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count 0 :from-end t))
  #(a b a c))

(deftest nsubstitute-vector.14
  (let ((x (copy-seq #(a b a c)))) (nsubstitute 'b 'a x :count -1 :from-end t))
  #(a b a c))

(deftest nsubstitute-vector.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute 'x 'a x :start i :end j)))
                (equalp y (concatenate 'simple-vector
                                       (make-array i :initial-element 'a)
                                       (make-array (- j i) :initial-element 'x)
                                       (make-array (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-vector.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (nsubstitute 'x 'a x :start i :end j :from-end t)))
                (equalp y (concatenate 'simple-vector
                                       (make-array i :initial-element 'a)
                                       (make-array (- j i) :initial-element 'x)
                                       (make-array (- 10 j) :initial-element 'a))))))
  t)

(deftest nsubstitute-vector.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute 'x 'a x :start i :end j :count c)))
                      (equalp y (concatenate 'simple-vector
                                             (make-array i :initial-element 'a)
                                             (make-array c :initial-element 'x)
                                             (make-array (- 10 (+ i c)) :initial-element 'a)))))))
  t)

(deftest nsubstitute-vector.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (nsubstitute 'x 'a x :start i :end j :count c :from-end t)))
                      (equalp y (concatenate 'simple-vector
                                             (make-array (- j c) :initial-element 'a)
                                             (make-array c :initial-element 'x)
                                             (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest nsubstitute-vector.19
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    result)
  #(1 2 x x x x x 8 9))

(deftest nsubstitute-vector.20
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    result)
  #(1 2 x 4 5 6 7 8 9))


(deftest nsubstitute-vector.21
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    result)
  #(1 2 3 4 5 6 7 x 9))

(deftest nsubstitute-vector.22
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    result)
  #(1 2 x 4 5 6 7 8 9))

(deftest nsubstitute-vector.23
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    result)
  #(1 2 3 4 5 6 7 x 9))

(deftest nsubstitute-vector.28
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute 'z 'a x)))
    result)
  #(z b z c b))

(deftest nsubstitute-vector.29
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute 'z 'a x :from-end t)))
    result)
  #(z b z c b))

(deftest nsubstitute-vector.30
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute 'z 'a x :count 1)))
    result)
  #(z b a c b))

(deftest nsubstitute-vector.31
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (nsubstitute 'z 'a x :from-end t :count 1)))
    result)
  #(a b z c b))

;;; Tests on strings

(deftest nsubstitute-string.1
  (let ((x "")) (nsubstitute #\b #\a x))
  "")

(deftest nsubstitute-string.2
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x))
  "bbbc")

(deftest nsubstitute-string.3
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count nil))
  "bbbc")

(deftest nsubstitute-string.4
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count 2))
  "bbbc")

(deftest nsubstitute-string.5
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count 1))
  "bbac")

(deftest nsubstitute-string.6
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count 0))
  "abac")

(deftest nsubstitute-string.7
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count -1))
  "abac")

(deftest nsubstitute-string.8
  (let ((x "")) (nsubstitute #\b #\a x :from-end t))
  "")

(deftest nsubstitute-string.9
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :from-end t))
  "bbbc")

(deftest nsubstitute-string.10
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :from-end t :count nil))
  "bbbc")

(deftest nsubstitute-string.11
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count 2 :from-end t))
  "bbbc")

(deftest nsubstitute-string.12
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count 1 :from-end t))
  "abbc")

(deftest nsubstitute-string.13
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count 0 :from-end t))
  "abac")

(deftest nsubstitute-string.14
  (let ((x (copy-seq "abac"))) (nsubstitute #\b #\a x :count -1 :from-end t))
  "abac")

(deftest nsubstitute-string.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (nsubstitute #\x #\a x :start i :end j)))
                (equalp y (concatenate 'simple-string
                                       (make-array i :initial-element #\a)
                                       (make-array (- j i) :initial-element #\x)
                                       (make-array (- 10 j) :initial-element #\a))))))
  t)

(deftest nsubstitute-string.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (nsubstitute #\x #\a x :start i :end j :from-end t)))
                (equalp y (concatenate 'simple-string
                                       (make-array i :initial-element #\a)
                                       (make-array (- j i) :initial-element #\x)
                                       (make-array (- 10 j) :initial-element #\a))))))
  t)

(deftest nsubstitute-string.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (nsubstitute #\x #\a x :start i :end j :count c)))
                      (equalp y (concatenate 'simple-string
                                             (make-array i :initial-element #\a)
                                             (make-array c :initial-element #\x)
                                             (make-array (- 10 (+ i c)) :initial-element #\a)))))))
  t)

(deftest nsubstitute-string.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (nsubstitute #\x #\a x :start i :end j :count c :from-end t)))
                      (equalp y (concatenate 'simple-string
                                             (make-array (- j c) :initial-element #\a)
                                             (make-array c :initial-element #\x)
                                             (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest nsubstitute-string.19
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (result (nsubstitute #\x #\5 x :test #'(lambda (a b)
                                                 (setq a (read-from-string (string a)))
                                                 (setq b (read-from-string (string b)))
                                                 (<= (abs (- a b)) 2)))))
    result)
  "12xxxxx89")

(deftest nsubstitute-string.20
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute #\x #\5 x :test #'(lambda (a b)
                                                 (setq a (read-from-string (string a)))
                                                 (setq b (read-from-string (string b)))
                                                 (incf c 2) (= (+ b c) a)))))
    result)
  "12x456789")


(deftest nsubstitute-string.21
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute #\x #\9 x :test #'(lambda (a b)
                                                 (setq a (read-from-string (string a)))
                                                 (setq b (read-from-string (string b)))
                                                 (incf c -2) (= (+ b c) a))
                             :from-end t)))
    result)
  "1234567x9")

(deftest nsubstitute-string.22
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c -4)
         (result (nsubstitute #\x #\5 x :test-not #'(lambda (a b)
                                                     (setq a (read-from-string (string a)))
                                                     (setq b (read-from-string (string b)))
                                                     (incf c 2) (/= (+ b c) a)))))
    result)
  "12x456789")


(deftest nsubstitute-string.23
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c 5)
         (result (nsubstitute #\x #\9 x :test-not #'(lambda (a b)
                                                     (setq a (read-from-string (string a)))
                                                     (setq b (read-from-string (string b)))
                                                     (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    result)
  "1234567x9")

(deftest nsubstitute-string.28
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute #\z #\a x)))
    result)
  "zbzcb")

(deftest nsubstitute-string.29
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute #\z #\a x :from-end t)))
    result)
  "zbzcb")

(deftest nsubstitute-string.30
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute #\z #\a x :count 1)))
    result)
  "zbacb")

(deftest nsubstitute-string.31
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (nsubstitute #\z #\a x :from-end t :count 1)))
    result)
  "abzcb")


;;; Tests on bit-vectors

(deftest nsubstitute-bit-vector.1
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (nsubstitute 0 1 x)))
    result)
  #*)

(deftest nsubstitute-bit-vector.2
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x)))
    result)
  #*)

(deftest nsubstitute-bit-vector.3
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 0 1 x)))
    result)
  #*000000)

(deftest nsubstitute-bit-vector.4
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x)))
    result)
  #*111111)

(deftest nsubstitute-bit-vector.5
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :start 1)))
    result)
  #*011111)

(deftest nsubstitute-bit-vector.6
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 0 1 x :start 2 :end nil)))
    result)
  #*010000)

(deftest nsubstitute-bit-vector.7
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :end 4)))
    result)
  #*111101)

(deftest nsubstitute-bit-vector.8
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 0 1 x :end nil)))
    result)
  #*000000)

(deftest nsubstitute-bit-vector.9
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 0 1 x :end 3)))
    result)
  #*000101)

(deftest nsubstitute-bit-vector.10
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 0 1 x :start 2 :end 4)))
    result)
  #*010001)

(deftest nsubstitute-bit-vector.11
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :start 2 :end 4)))
    result)
  #*011101)

(deftest nsubstitute-bit-vector.12
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count 1)))
    result)
  #*110101)

(deftest nsubstitute-bit-vector.13
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count 0)))
    result)
  #*010101)

(deftest nsubstitute-bit-vector.14
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count -1)))
    result)
  #*010101)

(deftest nsubstitute-bit-vector.15
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count 1 :from-end t)))
    result)
  #*010111)

(deftest nsubstitute-bit-vector.16
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count 0 :from-end t)))
    result)
  #*010101)

(deftest nsubstitute-bit-vector.17
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count -1 :from-end t)))
    result)
  #*010101)

(deftest nsubstitute-bit-vector.18
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count nil)))
    result)
  #*111111)

(deftest nsubstitute-bit-vector.19
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (nsubstitute 1 0 x :count nil :from-end t)))
    result)
  #*111111)

(deftest nsubstitute-bit-vector.20
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*0000000000)
                           (x (copy-seq orig))
                           (y (nsubstitute 1 0 x :start i :end j :count c)))
                      (equalp y (concatenate
                                 'simple-bit-vector
                                 (make-list i :initial-element 0)
                                 (make-list c :initial-element 1)
                                 (make-list (- 10 (+ i c)) :initial-element 0)))))))
  t)

(deftest nsubstitute-bit-vector.21
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*1111111111)
                           (x (copy-seq orig))
                           (y (nsubstitute 0 1 x :start i :end j :count c :from-end t)))
                      (equalp y (concatenate
                                 'simple-bit-vector
                                 (make-list (- j c) :initial-element 1)
                                 (make-list c :initial-element 0)
                                 (make-list (- 10 j) :initial-element 1)))))))
  t)

(deftest nsubstitute-bit-vector.22
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (nsubstitute 1 0 x :test #'(lambda (a b) (incf c) (and (<= 2 c 5) (= a b))))))
    result)
  #*0111110101)

(deftest nsubstitute-bit-vector.23
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (nsubstitute 1 0 x :test-not #'(lambda (a b) (incf c)
                                                 (not (and (<= 2 c 5) (= a b)))))))
    result)
  #*0111110101)

(deftest nsubstitute-bit-vector.24
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (nsubstitute 1 0 x :test #'(lambda (a b) (incf c) (and (<= 2 c 5) (= a b)))
                             :from-end t)))
    result)
  #*0101011111)

(deftest nsubstitute-bit-vector.25
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (nsubstitute 1 0 x :test-not #'(lambda (a b) (incf c)
                                                 (not (and (<= 2 c 5) (= a b))))
                             :from-end t)))
    result)
  #*0101011111)

(defharmless nsubstitute.test-and-test-not.1
  (nsubstitute 'b 'a (list 'a 'b 'c 'd 'a 'b) :test #'eql :test-not #'eql))

(defharmless nsubstitute.test-and-test-not.2
  (nsubstitute 'b 'a (list 'a 'b 'c 'd 'a 'b) :test-not #'eql :test #'eql))

(defharmless nsubstitute.test-and-test-not.3
  (nsubstitute 'b 'a (vector 'a 'b 'c 'd 'a 'b) :test #'eql :test-not #'eql))

(defharmless nsubstitute.test-and-test-not.4
  (nsubstitute 'b 'a (vector 'a 'b 'c 'd 'a 'b) :test-not #'eql :test #'eql))

(defharmless nsubstitute.test-and-test-not.5
  (nsubstitute #\b #\a (copy-seq "abcdab") :test #'eql :test-not #'eql))

(defharmless nsubstitute.test-and-test-not.6
  (nsubstitute #\b #\a (copy-seq "abcdab") :test-not #'eql :test #'eql))

(defharmless nsubstitute.test-and-test-not.7
  (nsubstitute 1 0 (copy-seq #*001101001) :test #'eql :test-not #'eql))

(defharmless nsubstitute.test-and-test-not.8
  (nsubstitute 0 1 (copy-seq #*1100110101) :test-not #'eql :test #'eql))


;;;; additional tests

(deftest nsubstitute-list.24
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car)))
    result)
  ((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest nsubstitute-list.25
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car :start 1 :end 5)))
    result)
  ((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest nsubstitute-list.26
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car :test (complement #'eql))))
    result)
  ((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest nsubstitute-list.27
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car :test-not #'eql)))
    result)
  ((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest nsubstitute-vector.24
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car)))
    result)
  #((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest nsubstitute-vector.25
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car :start 1 :end 5)))
    result)
  #((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest nsubstitute-vector.26
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car :test (complement #'eql))))
    result)
  #((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest nsubstitute-vector.27
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (nsubstitute '(a 10) 'a x :key #'car :test-not #'eql)))
    result)
  #((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest nsubstitute-vector.32
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values
     (nsubstitute 'x 'c v2 :count 1)
     v1))
  #(d a b x d a b c)
  #(a b c d a b x d a b c d a b c d))

(deftest nsubstitute-vector.33
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values
     (nsubstitute 'x 'c v2 :count 1 :from-end t)
     v1))
  #(d a b c d a b x)
  #(a b c d a b c d a b x d a b c d))


(deftest nsubstitute-string.24
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (nsubstitute #\a #\1 x :key #'nextdigit)))
    result)
  "a1a2342a15")

(deftest nsubstitute-string.25
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (nsubstitute #\a #\1 x :key #'nextdigit :start 1 :end 6)))
    result)
  "01a2342015")

(deftest nsubstitute-string.26
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (nsubstitute #\a #\1 x :key #'nextdigit :test (complement #'eql))))
    result)
  "0a0aaaa0aa")

(deftest nsubstitute-string.27
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (nsubstitute #\a #\1 x :key #'nextdigit :test-not #'eql)))
    result)
  "0a0aaaa0aa")

(deftest nsubstitute-string.32
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (nsubstitute #\! #\a s) "xyz!bcxyz!bc"))
   (assert (string= s "xyz!bcxyz!bc")))
  nil)

(deftest nsubstitute-string.33
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (nsubstitute #\! #\a s :count 1) "xyz!bcxyzabc"))
   (assert (string= s "xyz!bcxyzabc")))
  nil)

(deftest nsubstitute-string.34
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (nsubstitute #\! #\a s :count 1 :from-end t) "xyzabcxyz!bc"))
   (assert (string= s "xyzabcxyz!bc")))
  nil)

;;; More bit vector tests

(deftest nsubstitute-bit-vector.30
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute 1 0 x)))
    result)
  #*11111)

(deftest nsubstitute-bit-vector.31
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute 1 0 x :from-end t)))
    result)
  #*11111)

(deftest nsubstitute-bit-vector.32
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute 1 0 x :count 1)))
    result)
  #*11011)

(deftest nsubstitute-bit-vector.33
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (nsubstitute 1 0 x :from-end t :count 1)))
    result)
  #*01111)

(deftest nsubstitute.order.1
  (let ((i 0) a b c d e f g h)
    (values
     (nsubstitute
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) nil)
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

(deftest nsubstitute.order.2
  (let ((i 0) a b c d e f g h)
    (values
     (nsubstitute
      (progn (setf a (incf i)) 'a)
      (progn (setf b (incf i)) nil)
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

(deftest nsubstitute.allow-other-keys.1
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys t :bad t)
  (1 2 a 3 1 a 3))

(deftest nsubstitute.allow-other-keys.2
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :bad t :allow-other-keys t)
  (1 2 a 3 1 a 3))

(deftest nsubstitute.allow-other-keys.3
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :allow-other-keys nil :bad t)
  (1 2 a 3 1 a 3))

(deftest nsubstitute.allow-other-keys.4
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :bad t
                  :allow-other-keys t :allow-other-keys nil)
  (1 2 a 3 1 a 3))

(deftest nsubstitute.allow-other-keys.5
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3)
                  :allow-other-keys t :key #'1-)
  (a 2 0 3 a 0 3))

(deftest nsubstitute.keywords.6
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :key #'1- :key #'identity)
  (a 2 0 3 a 0 3))

(deftest nsubstitute.allow-other-keys.7
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :bad t :allow-other-keys nil)
  (1 2 a 3 1 a 3))

(deftest nsubstitute.allow-other-keys.8
  (nsubstitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys nil)
  (1 2 a 3 1 a 3))


;;; Error cases

(deftest nsubstitute.error.1
  (signals-error (nsubstitute) program-error)
  t)

(deftest nsubstitute.error.2
  (signals-error (nsubstitute 'a) program-error)
  t)

(deftest nsubstitute.error.3
  (signals-error (nsubstitute 'a 'b) program-error)
  t)

(deftest nsubstitute.error.4
  (signals-error (nsubstitute 'a 'b nil 'bad t) program-error)
  t)

(deftest nsubstitute.error.5
  (signals-error (nsubstitute 'a 'b nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest nsubstitute.error.6
  (signals-error (nsubstitute 'a 'b nil :key) program-error)
  t)

(deftest nsubstitute.error.7
  (signals-error (nsubstitute 'a 'b nil 1 2) program-error)
  t)

(deftest nsubstitute.error.8
  (signals-error (nsubstitute 'a 'b (list 'a 'b 'c) :test #'identity) program-error)
  t)

(deftest nsubstitute.error.9
  (signals-error (nsubstitute 'a 'b (list 'a 'b 'c) :test-not #'identity) program-error)
  t)

(deftest nsubstitute.error.10
  (signals-error (nsubstitute 'a 'b (list 'a 'b 'c) :key #'cons) program-error)
  t)

(deftest nsubstitute.error.11
  (signals-error (nsubstitute 'a 'b (list 'a 'b 'c) :key #'car) type-error)
  t)

(deftest nsubstitute.error.12
  (check-type-error #'(lambda (x) (nsubstitute 1 0 x)) #'sequencep)
  nil)
