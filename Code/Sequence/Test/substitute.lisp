;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 21:15:33 2002
;;;; Contains: Tests for SUBSTITUTE

(in-package #:sicl-sequence-test)

(deftest substitute-list.1
  (let ((x '())) (values (substitute 'b 'a x) x))
  nil nil)

(deftest substitute-list.2
  (let ((x '(a b a c))) (values (substitute 'b 'a x) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.3
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.4
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 2) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.5
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 1) x))
  (b b a c)
  (a b a c))

(deftest substitute-list.6
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 0) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.7
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count -1) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.8
  (let ((x '())) (values (substitute 'b 'a x :from-end t) x))
  nil nil)

(deftest substitute-list.9
  (let ((x '(a b a c))) (values (substitute 'b 'a x :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.10
  (let ((x '(a b a c))) (values (substitute 'b 'a x :from-end t :count nil) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.11
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 2 :from-end t) x))
  (b b b c)
  (a b a c))

(deftest substitute-list.12
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 1 :from-end t) x))
  (a b b c)
  (a b a c))

(deftest substitute-list.13
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count 0 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.14
  (let ((x '(a b a c))) (values (substitute 'b 'a x :count -1 :from-end t) x))
  (a b a c)
  (a b a c))

(deftest substitute-list.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute 'x 'a x :start i :end j)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-list.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig '(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute 'x 'a x :start i :end j :from-end t)))
                (and (equal orig x)
                     (equal y (nconc (make-list i :initial-element 'a)
                                     (make-list (- j i) :initial-element 'x)
                                     (make-list (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-list.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute 'x 'a x :start i :end j :count c)))
                      (and (equal orig x)
                           (equal y (nconc (make-list i :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-list.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig '(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute 'x 'a x :start i :end j :count c :from-end t)))
                      (and (equal orig x)
                           (equal y (nconc (make-list (- j c) :initial-element 'a)
                                           (make-list c :initial-element 'x)
                                           (make-list (- 10 j) :initial-element 'a))))))))
  t)

(deftest substitute-list.19
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (substitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    (and (equal orig x)
         result))
  (1 2 x x x x x 8 9))

(deftest substitute-list.20
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (substitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    (and (equal orig x)
         result))
  (1 2 x 4 5 6 7 8 9))


(deftest substitute-list.21
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (substitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    (and (equal orig x)
         result))
  (1 2 3 4 5 6 7 x 9))

(deftest substitute-list.22
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (substitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    (and (equal orig x)
         result))
  (1 2 x 4 5 6 7 8 9))


(deftest substitute-list.23
  (let* ((orig '(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (substitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    (and (equal orig x)
         result))
  (1 2 3 4 5 6 7 x 9))

(deftest substitute-list.24
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car)))
    (and (equal orig x)
         result))
  ((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest substitute-list.25
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :start 1 :end 5)))
    (and (equal orig x)
         result))
  ((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-list.26
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :test (complement #'eql))))
    (and (equal orig x)
         result))
  ((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest substitute-list.27
  (let* ((orig '((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :test-not #'eql)))
    (and (equal orig x)
         result))
  ((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

;;; Tests on vectors

(deftest substitute-vector.1
  (let ((x #())) (values (substitute 'b 'a x) x))
  #() #())

(deftest substitute-vector.2
  (let ((x #(a b a c))) (values (substitute 'b 'a x) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-vector.3
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count nil) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-vector.4
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count 2) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-vector.5
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count 1) x))
  #(b b a c)
  #(a b a c))

(deftest substitute-vector.6
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count 0) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-vector.7
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count -1) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-vector.8
  (let ((x #())) (values (substitute 'b 'a x :from-end t) x))
  #() #())

(deftest substitute-vector.9
  (let ((x #(a b a c))) (values (substitute 'b 'a x :from-end t) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-vector.10
  (let ((x #(a b a c))) (values (substitute 'b 'a x :from-end t :count nil) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-vector.11
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count 2 :from-end t) x))
  #(b b b c)
  #(a b a c))

(deftest substitute-vector.12
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count 1 :from-end t) x))
  #(a b b c)
  #(a b a c))

(deftest substitute-vector.13
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count 0 :from-end t) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-vector.14
  (let ((x #(a b a c))) (values (substitute 'b 'a x :count -1 :from-end t) x))
  #(a b a c)
  #(a b a c))

(deftest substitute-vector.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute 'x 'a x :start i :end j)))
                (and (equalp orig x)
                     (equalp y (concatenate 'simple-vector
                                           (make-array i :initial-element 'a)
                                           (make-array (- j i) :initial-element 'x)
                                           (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-vector.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig #(a a a a a a a a a a))
                     (x (copy-seq orig))
                     (y (substitute 'x 'a x :start i :end j :from-end t)))
                (and (equalp orig x)
                     (equalp y (concatenate 'simple-vector
                                           (make-array i :initial-element 'a)
                                           (make-array (- j i) :initial-element 'x)
                                           (make-array (- 10 j) :initial-element 'a)))))))
  t)

(deftest substitute-vector.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute 'x 'a x :start i :end j :count c)))
                      (and (equalp orig x)
                           (equalp y (concatenate 'simple-vector
                                                 (make-array i :initial-element 'a)
                                                 (make-array c :initial-element 'x)
                                                 (make-array (- 10 (+ i c)) :initial-element 'a))))))))
  t)

(deftest substitute-vector.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #(a a a a a a a a a a))
                           (x (copy-seq orig))
                           (y (substitute 'x 'a x :start i :end j :count c :from-end t)))
                      (and (equalp orig x)
                           (equalp y (concatenate 'simple-vector
                                                 (make-array (- j c) :initial-element 'a)
                                                 (make-array c :initial-element 'x)
                                                 (make-array (- 10 j) :initial-element 'a))))))))
  t)

(deftest substitute-vector.19
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (result (substitute 'x 5 x :test #'(lambda (a b) (<= (abs (- a b)) 2)))))
    (and (equalp orig x)
         result))
  #(1 2 x x x x x 8 9))

(deftest substitute-vector.20
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (substitute 'x 5 x :test #'(lambda (a b) (incf c 2) (= (+ b c) a)))))
    (and (equalp orig x)
         result))
  #(1 2 x 4 5 6 7 8 9))


(deftest substitute-vector.21
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (substitute 'x 9 x :test #'(lambda (a b) (incf c -2) (= (+ b c) a))
                             :from-end t)))
    (and (equalp orig x)
         result))
  #(1 2 3 4 5 6 7 x 9))

(deftest substitute-vector.22
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c -4)
         (result (substitute 'x 5 x :test-not #'(lambda (a b) (incf c 2) (/= (+ b c) a)))))
    (and (equalp orig x)
         result))
  #(1 2 x 4 5 6 7 8 9))


(deftest substitute-vector.23
  (let* ((orig #(1 2 3 4 5 6 7 8 9))
         (x (copy-seq orig))
         (c 5)
         (result (substitute 'x 9 x :test-not #'(lambda (a b) (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    (and (equalp orig x)
         result))
  #(1 2 3 4 5 6 7 x 9))

(deftest substitute-vector.24
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car)))
    (and (equalp orig x)
         result))
  #((a 10) (b 2) (a 10) (c 4) (d 5) (a 10) (e 7)))

(deftest substitute-vector.25
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :start 1 :end 5)))
    (and (equalp orig x)
         result))
  #((a 1) (b 2) (a 10) (c 4) (d 5) (a 6) (e 7)))

(deftest substitute-vector.26
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :test (complement #'eql))))
    (and (equalp orig x)
         result))
  #((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest substitute-vector.27
  (let* ((orig #((a 1) (b 2) (a 3) (c 4) (d 5) (a 6) (e 7)))
         (x (copy-seq orig))
         (result (substitute '(a 10) 'a x :key #'car :test-not #'eql)))
    (and (equalp orig x)
         result))
  #((a 1) (a 10) (a 3) (a 10) (a 10) (a 6) (a 10)))

(deftest substitute-vector.28
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute 'z 'a x)))
    result)
  #(z b z c b))

(deftest substitute-vector.29
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute 'z 'a x :from-end t)))
    result)
  #(z b z c b))

(deftest substitute-vector.30
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute 'z 'a x :count 1)))
    result)
  #(z b a c b))

(deftest substitute-vector.31
  (let* ((x (make-array '(10) :initial-contents '(a b a c b a d e a f)
                       :fill-pointer 5))
         (result (substitute 'z 'a x :from-end t :count 1)))
    result)
  #(a b z c b))

(deftest substitute-vector.32
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values
     (substitute 'x 'c v2 :count 1)
     v1))
  #(d a b x d a b c)
  #(a b c d a b c d a b c d a b c d))

(deftest substitute-vector.33
  (let* ((v1 (copy-seq #(a b c d a b c d a b c d a b c d)))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values
     (substitute 'x 'c v2 :count 1 :from-end t)
     v1))
  #(d a b c d a b x)
  #(a b c d a b c d a b c d a b c d))

;;; Tests on strings

(deftest substitute-string.1
  (let ((x "")) (values (substitute #\b #\a x) x))
  "" "")

(deftest substitute-string.2
  (let ((x "abac")) (values (substitute #\b #\a x) x))
  "bbbc"
  "abac")

(deftest substitute-string.3
  (let ((x "abac")) (values (substitute #\b #\a x :count nil) x))
  "bbbc"
  "abac")

(deftest substitute-string.4
  (let ((x "abac")) (values (substitute #\b #\a x :count 2) x))
  "bbbc"
  "abac")

(deftest substitute-string.5
  (let ((x "abac")) (values (substitute #\b #\a x :count 1) x))
  "bbac"
  "abac")

(deftest substitute-string.6
  (let ((x "abac")) (values (substitute #\b #\a x :count 0) x))
  "abac"
  "abac")

(deftest substitute-string.7
  (let ((x "abac")) (values (substitute #\b #\a x :count -1) x))
  "abac"
  "abac")

(deftest substitute-string.8
  (let ((x "")) (values (substitute #\b #\a x :from-end t) x))
  "" "")

(deftest substitute-string.9
  (let ((x "abac")) (values (substitute #\b #\a x :from-end t) x))
  "bbbc"
  "abac")

(deftest substitute-string.10
  (let ((x "abac")) (values (substitute #\b #\a x :from-end t :count nil) x))
  "bbbc"
  "abac")

(deftest substitute-string.11
  (let ((x "abac")) (values (substitute #\b #\a x :count 2 :from-end t) x))
  "bbbc"
  "abac")

(deftest substitute-string.12
  (let ((x "abac")) (values (substitute #\b #\a x :count 1 :from-end t) x))
  "abbc"
  "abac")

(deftest substitute-string.13
  (let ((x "abac")) (values (substitute #\b #\a x :count 0 :from-end t) x))
  "abac"
  "abac")

(deftest substitute-string.14
  (let ((x "abac")) (values (substitute #\b #\a x :count -1 :from-end t) x))
  "abac"
  "abac")

(deftest substitute-string.15
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (substitute #\x #\a x :start i :end j)))
                (and (equalp orig x)
                     (equalp y (concatenate 'simple-string
                                           (make-array i :initial-element #\a)
                                           (make-array (- j i) :initial-element #\x)
                                           (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest substitute-string.16
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (let* ((orig "aaaaaaaaaa")
                     (x (copy-seq orig))
                     (y (substitute #\x #\a x :start i :end j :from-end t)))
                (and (equalp orig x)
                     (equalp y (concatenate 'simple-string
                                           (make-array i :initial-element #\a)
                                           (make-array (- j i) :initial-element #\x)
                                           (make-array (- 10 j) :initial-element #\a)))))))
  t)

(deftest substitute-string.17
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (substitute #\x #\a x :start i :end j :count c)))
                      (and (equalp orig x)
                           (equalp y (concatenate 'simple-string
                                                 (make-array i :initial-element #\a)
                                                 (make-array c :initial-element #\x)
                                                 (make-array (- 10 (+ i c)) :initial-element #\a))))))))
  t)

(deftest substitute-string.18
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig "aaaaaaaaaa")
                           (x (copy-seq orig))
                           (y (substitute #\x #\a x :start i :end j :count c :from-end t)))
                      (and (equalp orig x)
                           (equalp y (concatenate 'simple-string
                                                 (make-array (- j c) :initial-element #\a)
                                                 (make-array c :initial-element #\x)
                                                 (make-array (- 10 j) :initial-element #\a))))))))
  t)

(deftest substitute-string.19
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (result (substitute #\x #\5 x :test #'(lambda (a b)
                                                 (setq a (read-from-string (string a)))
                                                 (setq b (read-from-string (string b)))
                                                 (<= (abs (- a b)) 2)))))
    (and (equalp orig x)
         result))
  "12xxxxx89")

(deftest substitute-string.20
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c -4)
         (result (substitute #\x #\5 x :test #'(lambda (a b)
                                                 (setq a (read-from-string (string a)))
                                                 (setq b (read-from-string (string b)))
                                                 (incf c 2) (= (+ b c) a)))))
    (and (equalp orig x)
         result))
  "12x456789")


(deftest substitute-string.21
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c 5)
         (result (substitute #\x #\9 x :test #'(lambda (a b)
                                                 (setq a (read-from-string (string a)))
                                                 (setq b (read-from-string (string b)))
                                                 (incf c -2) (= (+ b c) a))
                             :from-end t)))
    (and (equalp orig x)
         result))
  "1234567x9")

(deftest substitute-string.22
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c -4)
         (result (substitute #\x #\5 x :test-not #'(lambda (a b)
                                                     (setq a (read-from-string (string a)))
                                                     (setq b (read-from-string (string b)))
                                                     (incf c 2) (/= (+ b c) a)))))
    (and (equalp orig x)
         result))
  "12x456789")


(deftest substitute-string.23
  (let* ((orig "123456789")
         (x (copy-seq orig))
         (c 5)
         (result (substitute #\x #\9 x :test-not #'(lambda (a b)
                                                     (setq a (read-from-string (string a)))
                                                     (setq b (read-from-string (string b)))
                                                     (incf c -2) (/= (+ b c) a))
                             :from-end t)))
    (and (equalp orig x)
         result))
  "1234567x9")

(deftest substitute-string.24
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (substitute #\a #\1 x :key #'nextdigit)))
    (and (equalp orig x)
         result))
  "a1a2342a15")

(deftest substitute-string.25
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (substitute #\a #\1 x :key #'nextdigit :start 1 :end 6)))
    (and (equalp orig x)
         result))
  "01a2342015")

(deftest substitute-string.26
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (substitute #\a #\1 x :key #'nextdigit :test (complement #'eql))))
    (and (equalp orig x)
         result))
  "0a0aaaa0aa")

(deftest substitute-string.27
  (let* ((orig "0102342015")
         (x (copy-seq orig))
         (result (substitute #\a #\1 x :key #'nextdigit :test-not #'eql)))
    (and (equalp orig x)
         result))
   "0a0aaaa0aa")

(deftest substitute-string.28
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute #\z #\a x)))
    result)
  "zbzcb")

(deftest substitute-string.29
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute #\z #\a x :from-end t)))
    result)
  "zbzcb")

(deftest substitute-string.30
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute #\z #\a x :count 1)))
    result)
  "zbacb")

(deftest substitute-string.31
  (let* ((x (make-array '(10) :initial-contents "abacbadeaf"
                       :fill-pointer 5 :element-type 'character))
         (result (substitute #\z #\a x :from-end t :count 1)))
    result)
  "abzcb")

(deftest substitute-string.32
  (do-special-strings
   (s "xyzabcxyzabc" nil)
   (assert (string= (substitute #\! #\a s) "xyz!bcxyz!bc"))
   (assert (string= (substitute #\! #\a s :count 1) "xyz!bcxyzabc"))
   (assert (string= (substitute #\! #\a s :count 1 :from-end t) "xyzabcxyz!bc"))
   (assert (string= s "xyzabcxyzabc")))
  nil)

;;; Tests on bit-vectors

(deftest substitute-bit-vector.1
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (substitute 0 1 x)))
    (and (equalp orig x)
         result))
  #*)

(deftest substitute-bit-vector.2
  (let* ((orig #*)
         (x (copy-seq orig))
         (result (substitute 1 0 x)))
    (and (equalp orig x)
         result))
  #*)

(deftest substitute-bit-vector.3
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 0 1 x)))
    (and (equalp orig x)
         result))
  #*000000)

(deftest substitute-bit-vector.4
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x)))
    (and (equalp orig x)
         result))
  #*111111)

(deftest substitute-bit-vector.5
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :start 1)))
    (and (equalp orig x)
         result))
  #*011111)

(deftest substitute-bit-vector.6
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 0 1 x :start 2 :end nil)))
    (and (equalp orig x)
         result))
  #*010000)

(deftest substitute-bit-vector.7
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :end 4)))
    (and (equalp orig x)
         result))
  #*111101)

(deftest substitute-bit-vector.8
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 0 1 x :end nil)))
    (and (equalp orig x)
         result))
  #*000000)

(deftest substitute-bit-vector.9
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 0 1 x :end 3)))
    (and (equalp orig x)
         result))
  #*000101)

(deftest substitute-bit-vector.10
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 0 1 x :start 2 :end 4)))
    (and (equalp orig x)
         result))
  #*010001)

(deftest substitute-bit-vector.11
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :start 2 :end 4)))
    (and (equalp orig x)
         result))
  #*011101)

(deftest substitute-bit-vector.12
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count 1)))
    (and (equalp orig x)
         result))
  #*110101)

(deftest substitute-bit-vector.13
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count 0)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-bit-vector.14
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count -1)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-bit-vector.15
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count 1 :from-end t)))
    (and (equalp orig x)
         result))
  #*010111)

(deftest substitute-bit-vector.16
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count 0 :from-end t)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-bit-vector.17
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count -1 :from-end t)))
    (and (equalp orig x)
         result))
  #*010101)

(deftest substitute-bit-vector.18
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count nil)))
    (and (equalp orig x)
         result))
  #*111111)

(deftest substitute-bit-vector.19
  (let* ((orig #*010101)
         (x (copy-seq orig))
         (result (substitute 1 0 x :count nil :from-end t)))
    (and (equalp orig x)
         result))
  #*111111)

(deftest substitute-bit-vector.20
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*0000000000)
                           (x (copy-seq orig))
                           (y (substitute 1 0 x :start i :end j :count c)))
                      (and (equalp orig x)
                           (equalp y (concatenate
                                      'simple-bit-vector
                                      (make-list i :initial-element 0)
                                      (make-list c :initial-element 1)
                                      (make-list (- 10 (+ i c)) :initial-element 0))))))))
  t)

(deftest substitute-bit-vector.21
  (loop for i from 0 to 9 always
        (loop for j from i to 10 always
              (loop for c from 0 to (- j i) always
                    (let* ((orig #*1111111111)
                           (x (copy-seq orig))
                           (y (substitute 0 1 x :start i :end j :count c :from-end t)))
                      (and (equalp orig x)
                           (equalp y (concatenate
                                      'simple-bit-vector
                                      (make-list (- j c) :initial-element 1)
                                      (make-list c :initial-element 0)
                                      (make-list (- 10 j) :initial-element 1))))))))
  t)

(deftest substitute-bit-vector.22
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (substitute 1 0 x :test #'(lambda (a b) (incf c) (and (<= 2 c 5) (= a b))))))
    (and (equalp orig x)
         result))
  #*0111110101)

(deftest substitute-bit-vector.23
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (substitute 1 0 x :test-not #'(lambda (a b) (incf c)
                                                 (not (and (<= 2 c 5) (= a b)))))))
    (and (equalp orig x)
         result))
  #*0111110101)

(deftest substitute-bit-vector.24
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (substitute 1 0 x :test #'(lambda (a b) (incf c) (and (<= 2 c 5) (= a b)))
                             :from-end t)))
    (and (equalp orig x)
         result))
  #*0101011111)

(deftest substitute-bit-vector.25
  (let* ((orig #*0101010101)
         (x (copy-seq orig))
         (c 0)
         (result (substitute 1 0 x :test-not #'(lambda (a b) (incf c)
                                                 (not (and (<= 2 c 5) (= a b))))
                             :from-end t)))
    (and (equalp orig x)
         result))
  #*0101011111)

(deftest substitute-bit-vector.26
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (substitute 1 1 x :key #'1+)))
    (and (equalp orig x)
         result))
  #*11111111111111111)

(deftest substitute-bit-vector.27
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (substitute 1 1 x :key #'1+ :start 1 :end 10)))
    (and (equalp orig x)
         result))
  #*01111111111010110)

(deftest substitute-bit-vector.28
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (substitute 0 1 x :key #'1+ :test (complement #'eql))))
    (and (equalp orig x)
         result))
  #*00000000000000000)

(deftest substitute-bit-vector.29
  (let* ((orig #*00111001011010110)
         (x (copy-seq orig))
         (result (substitute 0 1 x :key #'1+ :test-not #'eql)))
    (and (equalp orig x)
         result))
  #*00000000000000000)

(deftest substitute-bit-vector.30
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute 1 0 x)))
    result)
  #*11111)

(deftest substitute-bit-vector.31
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute 1 0 x :from-end t)))
    result)
  #*11111)

(deftest substitute-bit-vector.32
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute 1 0 x :count 1)))
    result)
  #*11011)

(deftest substitute-bit-vector.33
  (let* ((x (make-array '(10) :initial-contents '(0 1 0 1 1 0 1 1 0 1)
                       :fill-pointer 5 :element-type 'bit))
         (result (substitute 1 0 x :from-end t :count 1)))
    result)
  #*01111)

(defharmless substitute.test-and-test-not.1
  (substitute 'b 'a (list 'a 'b 'c 'd 'a 'b) :test #'eql :test-not #'eql))

(defharmless substitute.test-and-test-not.2
  (substitute 'b 'a (list 'a 'b 'c 'd 'a 'b) :test-not #'eql :test #'eql))

(defharmless substitute.test-and-test-not.3
  (substitute 'b 'a (vector 'a 'b 'c 'd 'a 'b) :test #'eql :test-not #'eql))

(defharmless substitute.test-and-test-not.4
  (substitute 'b 'a (vector 'a 'b 'c 'd 'a 'b) :test-not #'eql :test #'eql))

(defharmless substitute.test-and-test-not.5
  (substitute #\b #\a (copy-seq "abcdab") :test #'eql :test-not #'eql))

(defharmless substitute.test-and-test-not.6
  (substitute #\b #\a (copy-seq "abcdab") :test-not #'eql :test #'eql))

(defharmless substitute.test-and-test-not.7
  (substitute 1 0 (copy-seq #*001101001) :test #'eql :test-not #'eql))

(defharmless substitute.test-and-test-not.8
  (substitute 0 1 (copy-seq #*1100110101) :test-not #'eql :test #'eql))


(deftest substitute.order.1
  (let ((i 0) a b c d e f g h)
    (values
     (substitute
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

(deftest substitute.order.2
  (let ((i 0) a b c d e f g h)
    (values
     (substitute
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

(deftest substitute.allow-other-keys.1
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys t :bad t)
  (1 2 a 3 1 a 3))

(deftest substitute.allow-other-keys.2
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :bad t :allow-other-keys t)
  (1 2 a 3 1 a 3))

(deftest substitute.allow-other-keys.3
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :allow-other-keys nil :bad t)
  (1 2 a 3 1 a 3))

(deftest substitute.allow-other-keys.4
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :bad t
                  :allow-other-keys t :allow-other-keys nil)
  (1 2 a 3 1 a 3))

(deftest substitute.allow-other-keys.5
  (substitute 'a 0 (list 1 2 0 3 1 0 3)
                  :allow-other-keys t :key #'1-)
  (a 2 0 3 a 0 3))

(deftest substitute.keywords.6
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :key #'1- :key #'identity)
  (a 2 0 3 a 0 3))

(deftest substitute.allow-other-keys.7
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys t
                  :bad t :allow-other-keys nil)
  (1 2 a 3 1 a 3))

(deftest substitute.allow-other-keys.8
  (substitute 'a 0 (list 1 2 0 3 1 0 3) :allow-other-keys nil)
  (1 2 a 3 1 a 3))

;;; Constant folding tests

(def-fold-test substitute.fold.1 (substitute 'z 'b '(a b c)))
(def-fold-test substitute.fold.2 (substitute 'z 'b #(a b c)))
(def-fold-test substitute.fold.3 (substitute 0 1 #*001101))
(def-fold-test substitute.fold.4 (substitute #\a #\b "abcebadfke"))

;;; Error cases

(deftest substitute.error.1
  (signals-error (substitute) program-error)
  t)

(deftest substitute.error.2
  (signals-error (substitute 'a) program-error)
  t)

(deftest substitute.error.3
  (signals-error (substitute 'a 'b) program-error)
  t)

(deftest substitute.error.4
  (signals-error (substitute 'a 'b nil 'bad t) program-error)
  t)

(deftest substitute.error.5
  (signals-error (substitute 'a 'b nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest substitute.error.6
  (signals-error (substitute 'a 'b nil :key) program-error)
  t)

(deftest substitute.error.7
  (signals-error (substitute 'a 'b nil 1 2) program-error)
  t)

(deftest substitute.error.8
  (signals-error (substitute 'a 'b (list 'a 'b 'c) :test #'identity) program-error)
  t)

(deftest substitute.error.9
  (signals-error (substitute 'a 'b (list 'a 'b 'c) :test-not #'identity) program-error)
  t)

(deftest substitute.error.10
  (signals-error (substitute 'a 'b (list 'a 'b 'c) :key #'cons) program-error)
  t)

(deftest substitute.error.11
  (signals-error (substitute 'a 'b (list 'a 'b 'c) :key #'car) type-error)
  t)

(deftest substitute.error.12
  (check-type-error #'(lambda (x) (substitute 'a 'b x)) #'sequencep)
  nil)
