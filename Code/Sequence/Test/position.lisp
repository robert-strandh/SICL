;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Aug 23 07:49:49 2002
;;;; Contains: Tests for POSITION

(in-package #:sicl-sequence-test)

(deftest position-list.1
  (position 'c '(a b c d e c a))
  2)

(deftest position-list.2
  (position 'c '(a b c d e c a) :from-end t)
  5)

(deftest position-list.3
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i))
  (2 2 2 5 5 5 nil nil))

(deftest position-list.4
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i :end nil))
  (2 2 2 5 5 5 nil nil))

(deftest position-list.5
  (loop for i from 7 downto 0 collect
        (position 'c '(a b c d e c a) :end i))
  (2 2 2 2 2 nil nil nil))

(deftest position-list.6
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-list.7
  (loop for i from 0 to 7 collect
        (position 'c '(a b c d e c a) :start i :end nil :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-list.8
  (loop for i from 7 downto 0 collect
        (position 'c '(a b c d e c a) :end i :from-end t))
  (5 5 2 2 2 nil nil nil))

(deftest position-list.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c '(a b c d e c a) :start i :end j)))
  ((nil nil 2 2 2 2 2)
   (nil 2 2 2 2 2)
   (2 2 2 2 2)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-list.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c '(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil 2 2 2 5 5)
   (nil 2 2 2 5 5)
   (2 2 2 5 5)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-list.11
  (position 5 '(1 2 3 4 5 6 4 8) :key #'1+)
  3)

(deftest position-list.12
  (position 5 '(1 2 3 4 5 6 4 8) :key '1+)
  3)

(deftest position-list.13
  (position 5 '(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  6)

(deftest position-list.14
  (position 'a '(a a b a c e d a f a) :test (complement #'eql))
  2)

(deftest position-list.15
  (position 'a '(a a b a c e d a f a) :test (complement #'eql)
            :from-end t)
  8)

(deftest position-list.16
  (position 'a '(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-list.17
  (position 'a '(a a b a c e d a f a) :test-not 'eql
            :from-end t)
  8)

(deftest position-list.18
  (position 'a '(a a b a c e d a f a) :test-not 'eql)
  2)

(deftest position-list.19
  (position 'a '(a a b a c e d a f a) :test-not #'eql
            :from-end t)
  8)

(deftest position-list.20
  (position 'a '(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-list.21
  (position 'a '(a a b a c e d a f a) :test #'eql
            :start 2)
  3)

(deftest position-list.22
  (position 'a '(a a b a c e d a f a) :test #'eql
            :start 2 :end nil)
  3)

(deftest position-list.23
  (position 'a '(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5)
  2)

(deftest position-list.24
  (position 'a '(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5 :from-end t)
  4)

(deftest position-list.25
  (position '(a b) '(a (b a) (a b c) (a b) (d e) f) :test #'equal)
  3)

(deftest position-list.26
  (position 'a '((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  2)

(deftest position-list.27
  (position 'a '((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
            :start 3)
  4)

(deftest position-list.28
  (position 'a '((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
            :start 2 :from-end t)
  4)

(deftest position-list.29
  (position 10 '(1 4 8 10 15 20) :test #'<)
  4)

(deftest position-list.30
  (position 10 '(1 4 8 10 15 20) :test-not #'>=)
  4)

;;; Tests on vectors

(deftest position-vector.1
  (position 'c #(a b c d e c a))
  2)

(deftest position-vector.2
  (position 'c #(a b c d e c a) :from-end t)
  5)

(deftest position-vector.3
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i))
  (2 2 2 5 5 5 nil nil))

(deftest position-vector.4
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i :end nil))
  (2 2 2 5 5 5 nil nil))

(deftest position-vector.5
  (loop for i from 7 downto 0 collect
        (position 'c #(a b c d e c a) :end i))
  (2 2 2 2 2 nil nil nil))

(deftest position-vector.6
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-vector.7
  (loop for i from 0 to 7 collect
        (position 'c #(a b c d e c a) :start i :end nil :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-vector.8
  (loop for i from 7 downto 0 collect
        (position 'c #(a b c d e c a) :end i :from-end t))
  (5 5 2 2 2 nil nil nil))

(deftest position-vector.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c #(a b c d e c a) :start i :end j)))
  ((nil nil 2 2 2 2 2)
   (nil 2 2 2 2 2)
   (2 2 2 2 2)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-vector.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 'c #(a b c d e c a) :start i :end j :from-end t)))
  ((nil nil 2 2 2 5 5)
   (nil 2 2 2 5 5)
   (2 2 2 5 5)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-vector.11
  (position 5 #(1 2 3 4 5 6 4 8) :key #'1+)
  3)

(deftest position-vector.12
  (position 5 #(1 2 3 4 5 6 4 8) :key '1+)
  3)

(deftest position-vector.13
  (position 5 #(1 2 3 4 5 6 4 8) :key #'1+ :from-end t)
  6)

(deftest position-vector.14
  (position 'a #(a a b a c e d a f a) :test (complement #'eql))
  2)

(deftest position-vector.15
  (position 'a #(a a b a c e d a f a) :test (complement #'eql)
            :from-end t)
  8)

(deftest position-vector.16
  (position 'a #(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-vector.17
  (position 'a #(a a b a c e d a f a) :test-not 'eql
            :from-end t)
  8)

(deftest position-vector.18
  (position 'a #(a a b a c e d a f a) :test-not 'eql)
  2)

(deftest position-vector.19
  (position 'a #(a a b a c e d a f a) :test-not #'eql
            :from-end t)
  8)

(deftest position-vector.20
  (position 'a #(a a b a c e d a f a) :test-not #'eql)
  2)

(deftest position-vector.21
  (position 'a #(a a b a c e d a f a) :test #'eql
            :start 2)
  3)

(deftest position-vector.22
  (position 'a #(a a b a c e d a f a) :test #'eql
            :start 2 :end nil)
  3)

(deftest position-vector.23
  (position 'a #(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5)
  2)

(deftest position-vector.24
  (position 'a #(a a b a c e d a f a) :test-not #'eql
            :start 0 :end 5 :from-end t)
  4)

(deftest position-vector.25
  (position '(a b) #(a (b a) (a b c) (a b) (d e) f) :test #'equal)
  3)

(deftest position-vector.26
  (position 'a #((c) (b a) (a b c) (a b) (d e) f) :key #'car)
  2)

(deftest position-vector.27
  (position 'a #((c) (b a) (a b c) (z) (a b) (d e) f) :key #'car
            :start 3)
  4)

(deftest position-vector.28
  (position 'a #((c) (b a) (a b c) (z) (a b) (d e) (f)) :key #'car
            :start 2 :from-end t)
  4)

(deftest position-vector.29
  (position 'a (make-array '(10) :initial-contents '(b b b b b a a a a a)
                           :fill-pointer 5))
  nil)

(deftest position-vector.30
  (position 'a (make-array '(10) :initial-contents '(b b b b a a a a a a)
                           :fill-pointer 5))
  4)

(deftest position-vector.31
  (position 'a (make-array '(10) :initial-contents '(b a b b a a a a a a)
                           :fill-pointer 5)
            :from-end t)
  4)

(deftest position-vector.32
  (position 10 #(1 4 8 10 15 20) :test #'<)
  4)

(deftest position-vector.33
  (position 10 #(1 4 8 10 15 20) :test-not #'>=)
  4)

(deftest position-vector.34
  (let* ((v1 #(x x x a b c d a b c d y y y y y))
         (v2 (make-array '(8) :displaced-to v1
                         :displaced-index-offset 3)))
    (values (position 'c v2)
            (position 'c v2 :from-end t)))
  2 6)

;;; tests on bit vectors

(deftest position-bit-vector.1
  (position 1 #*001001010100)
  2)

(deftest position-bit-vector.2
  (position 1 #*001001010100 :from-end t)
  9)

(deftest position-bit-vector.3
  (loop for i from 0 to 7 collect
        (position 1 #*0010010 :start i))
  (2 2 2 5 5 5 nil nil))

(deftest position-bit-vector.4
  (loop for i from 0 to 7 collect
        (position 1 #*0010010 :start i :end nil))
  (2 2 2 5 5 5 nil nil))

(deftest position-bit-vector.5
  (loop for i from 7 downto 0 collect
        (position 1 #*0010010 :end i))
  (2 2 2 2 2 nil nil nil))

(deftest position-bit-vector.6
  (loop for i from 0 to 7 collect
        (position 1 #*0010010 :start i :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-bit-vector.7
  (loop for i from 0 to 7 collect
        (position 0 #*1101101 :start i :end nil :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-bit-vector.8
  (loop for i from 7 downto 0 collect
        (position 0 #*1101101 :end i :from-end t))
  (5 5 2 2 2 nil nil nil))

(deftest position-bit-vector.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 1 #*0010010 :start i :end j)))
  ((nil nil 2 2 2 2 2)
   (nil 2 2 2 2 2)
   (2 2 2 2 2)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-bit-vector.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position 1 #*0010010 :start i :end j :from-end t)))
  ((nil nil 2 2 2 5 5)
   (nil 2 2 2 5 5)
   (2 2 2 5 5)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-bit-vector.11
  (position 2 #*00010001010 :key #'1+)
  3)

(deftest position-bit-vector.12
  (position 2 #*00010001010 :key '1+)
  3)

(deftest position-bit-vector.13
  (position 2 #*0010001000 :key #'1+ :from-end t)
  6)

(deftest position-bit-vector.14
  (position 0 #*0010111010 :test (complement #'eql))
  2)

(deftest position-bit-vector.15
  (position 0 #*0010111010 :test (complement #'eql)
            :from-end t)
  8)

(deftest position-bit-vector.16
  (position 0 #*0010111010 :test-not #'eql)
  2)

(deftest position-bit-vector.17
  (position 0 #*001011101 :test-not 'eql
            :from-end t)
  8)

(deftest position-bit-vector.18
  (position 0 #*00101110 :test-not 'eql)
  2)

(deftest position-bit-vector.19
  (position 0 #*00101110 :test-not #'eql
            :from-end t)
  6)

(deftest position-bit-vector.20
  (position 0 #*00101110 :test-not #'eql)
  2)

(deftest position-bit-vector.21
  (position 0 #*00101110 :test #'eql
            :start 2)
  3)

(deftest position-bit-vector.22
  (position 0 #*00101110 :test #'eql
            :start 2 :end nil)
  3)

(deftest position-bit-vector.23
  (position 0 #*00101110 :test-not #'eql
            :start 0 :end 5)
  2)

(deftest position-bit-vector.24
  (position 0 #*00101110 :test-not #'eql
            :start 0 :end 5 :from-end t)
  4)

(deftest position-bit-vector.25
  (position 2 #*1100001010 :key #'1+
            :start 3)
  6)

(deftest position-bit-vector.27
  (position 2 #*1100001010 :key #'1+
            :start 2 :from-end t)
  8)

(deftest position-bit-vector.28
  (position 0 (make-array '(10) :initial-contents '(1 1 1 1 1 0 0 0 0 0)
                          :element-type 'bit
                          :fill-pointer 5))
  nil)

(deftest position-bit-vector.29
  (position 0 (make-array '(10) :initial-contents '(1 1 1 1 1 0 0 0 0 0)
                          :element-type 'bit
                          :fill-pointer 5)
            :from-end t)
  nil)

(deftest position-bit-vector.30
  (position 0 (make-array '(10) :initial-contents '(1 1 1 1 0 0 0 0 0 0)
                          :element-type 'bit
                          :fill-pointer 5))
  4)

(deftest position-bit-vector.31
  (position 0 (make-array '(10) :initial-contents '(0 1 0 1 0 0 0 0 0 0)
                          :element-type 'bit
                          :fill-pointer 5)
            :from-end t)
  4)

(deftest position-bit-vector.32
  (position 0 (make-array '(10) :initial-contents '(1 0 1 1 0 0 0 0 0 0)
                          :element-type 'bit
                          :fill-pointer 5))
  1)

(deftest position-bit-vector.33
  (position 0 #*1111000 :test #'>=)
  4)

(deftest position-bit-vector.34
  (position 0 #*1111000 :test-not #'<)
  4)

;;; strings

(deftest position-string.1
  (position #\c "abcdeca")
  2)

(deftest position-string.2
  (position #\c "abcdeca" :from-end t)
  5)

(deftest position-string.3
  (loop for i from 0 to 7 collect
        (position #\c "abcdeca" :start i))
  (2 2 2 5 5 5 nil nil))

(deftest position-string.4
  (loop for i from 0 to 7 collect
        (position #\c "abcdeca" :start i :end nil))
  (2 2 2 5 5 5 nil nil))

(deftest position-string.5
  (loop for i from 7 downto 0 collect
        (position #\c "abcdeca" :end i))
  (2 2 2 2 2 nil nil nil))

(deftest position-string.6
  (loop for i from 0 to 7 collect
        (position #\c "abcdeca" :start i :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-string.7
  (loop for i from 0 to 7 collect
        (position #\c "abcdeca" :start i :end nil :from-end t))
  (5 5 5 5 5 5 nil nil))

(deftest position-string.8
  (loop for i from 7 downto 0 collect
        (position #\c "abcdeca" :end i :from-end t))
  (5 5 2 2 2 nil nil nil))

(deftest position-string.9
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position #\c "abcdeca" :start i :end j)))
  ((nil nil 2 2 2 2 2)
   (nil 2 2 2 2 2)
   (2 2 2 2 2)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-string.10
  (loop for i from 0 to 6 collect
        (loop for j from (1+ i) to 7
              collect
              (position #\c "abcdeca" :start i :end j :from-end t)))
  ((nil nil 2 2 2 5 5)
   (nil 2 2 2 5 5)
   (2 2 2 5 5)
   (nil nil 5 5)
   (nil 5 5)
   (5 5)
   (nil)))

(deftest position-string.11
  (position 5 "12345648" :key #'(lambda (c)
                                  (1+ (read-from-string (string c)))))
  3)

(deftest position-string.13
  (position 5 "12345648" :key #'(lambda (c)
                                  (1+ (read-from-string (string c))))
            :from-end t)
  6)

(deftest position-string.14
  (position #\a "aabacedafa" :test (complement #'eql))
  2)

(deftest position-string.15
  (position #\a "aabacedafa" :test (complement #'eql)
            :from-end t)
  8)

(deftest position-string.16
  (position #\a "aabacedafa" :test-not #'eql)
  2)

(deftest position-string.17
  (position #\a "aabacedafa" :test-not 'eql
            :from-end t)
  8)

(deftest position-string.18
  (position #\a "aabacedafa" :test-not 'eql)
  2)

(deftest position-string.19
  (position #\a "aabacedafa" :test-not #'eql
            :from-end t)
  8)

(deftest position-string.20
  (position #\a "aabacedafa" :test-not #'eql)
  2)

(deftest position-string.21
  (position #\a "aabacedafa" :test #'eql
            :start 2)
  3)

(deftest position-string.22
  (position #\a "aabacedafa" :test #'eql
            :start 2 :end nil)
  3)

(deftest position-string.23
  (position #\a "aabacedafa" :test-not #'eql
            :start 0 :end 5)
  2)

(deftest position-string.24
  (position #\a "aabacedafa" :test-not #'eql
            :start 0 :end 5 :from-end t)
  4)

(deftest position-string.25
  (position #\a (make-array '(10) :initial-contents "bbbbbaaaaa"
                            :element-type 'character
                            :fill-pointer 5))
  nil)

(deftest position-string.26
  (position #\a (make-array '(10) :initial-contents "bbbbbaaaaa"
                            :element-type 'character
                            :fill-pointer 5)
            :from-end t)
  nil)

(deftest position-string.27
  (position #\a (make-array '(10) :initial-contents "bbbbaaaaaa"
                            :element-type 'character
                            :fill-pointer 5))
  4)

(deftest position-string.28
  (position #\a (make-array '(10) :initial-contents "babbaaaaaa"
                            :element-type 'character
                            :fill-pointer 5)
            :from-end t)
  4)

(deftest position-string.29
  (position #\m "adfmpz" :test #'char<)
  4)

(deftest position-string.30
  (position #\m "adfmpz" :test-not #'char>=)
  4)

(deftest position-string.31
  (let* ((s1 (copy-seq "xxxabcdyyyyy"))
         (s2 (make-array '(4) :displaced-to s1
                         :displaced-index-offset 3
                         :element-type (array-element-type s1))))
    (position #\c s2))
  2)

(deftest position-string.32
  (let* ((s1 (copy-seq "xxxabcdabcdyyyyyyyy"))
         (s2 (make-array '(8) :displaced-to s1
                         :displaced-index-offset 3
                         :element-type (array-element-type s1))))
    (position #\c s2 :from-end t))
  6)

(deftest position-string.33
  (do-special-strings
   (s "abcdabcdabcd" nil)
   (let* ((c #\c)
          (pos (position c s)))
     (assert (eql pos 2) () "First position of ~A in ~A is ~A" c s pos)))
  nil)

(deftest position-string.34
  (do-special-strings
   (s "abcdabcdabcd" nil)
   (let* ((c #\c)
          (pos (position c s :from-end t)))
     (assert (eql pos 10) () "Last position of ~A in ~A is ~A" c s pos)))
  nil)

(defharmless position.test-and-test-not.1
  (position 'b '(a b c d) :test #'eql :test-not #'eql))

(defharmless position.test-and-test-not.2
  (position 'b '(a b c d) :test-not #'eql :test #'eql))

(defharmless position.test-and-test-not.3
  (position 'b #(a b c d) :test #'eql :test-not #'eql))

(defharmless position.test-and-test-not.4
  (position 'b #(a b c d) :test-not #'eql :test #'eql))

(defharmless position.test-and-test-not.5
  (position #\b "abcd" :test #'eql :test-not #'eql))

(defharmless position.test-and-test-not.6
  (position #\b "abcd" :test-not #'eql :test #'eql))

(defharmless position.test-and-test-not.7
  (position 1 #*001010010 :test #'eql :test-not #'eql))

(defharmless position.test-and-test-not.8
  (position 0 #*1110010110111 :test-not #'eql :test #'eql))

(deftest position.order.1
  (let ((i 0) a b c d e f g)
    (values
     (position
      (progn (setf a (incf i)) 0)
      (progn (setf b (incf i)) '(3 1 8 2 1 2 3 4))
      :from-end (setf c (incf i))
      :start (progn (setf d (incf i)) 1)
      :end (progn (setf e (incf i)) 6)
      :key (progn (setf f (incf i)) #'1-)
      :test (progn (setf g (incf i)) #'=)
      )
     i a b c d e f g))
  4 7 1 2 3 4 5 6 7)

(deftest position.order.2
  (let ((i 0) a b c d e f g)
    (values
     (position
      (progn (setf a (incf i)) 0)
      (progn (setf b (incf i)) '(3 1 8 2 1 2 3 4))
      :test-not (progn (setf c (incf i)) #'/=)
      :key (progn (setf d (incf i)) #'1-)
      :end (progn (setf e (incf i)) 6)
      :start (progn (setf f (incf i)) 1)
      :from-end (setf g (incf i))
      )
     i a b c d e f g))
  4 7 1 2 3 4 5 6 7)

;;; Keyword tests

(deftest position.allow-other-keys.1
  (position 0 '(1 2 0 3 2 1) :allow-other-keys t)
  2)

(deftest position.allow-other-keys.2
  (position 0 '(1 2 0 3 2 1) :allow-other-keys nil)
  2)

(deftest position.allow-other-keys.3
  (position 0 '(1 2 0 3 2 1) :allow-other-keys t :bad t)
  2)

(deftest position.allow-other-keys.4
  (position 0 '(1 2 0 3 2 1) :bad t :allow-other-keys t)
  2)

(deftest position.allow-other-keys.5
  (position 0 '(1 2 0 3 2 1) :bad t :allow-other-keys t :key #'1-)
  0)

(deftest position.keywords.6
  (position 0 '(1 2 0 3 2 1) :key #'1- :key #'identity)
  0)

(deftest position.allow-other-keys.7
  (position 0 '(1 2 0 3 2 1) :bad t :allow-other-keys t
               :allow-other-keys nil)
  2)

(deftest position.allow-other-keys.8
  (position 0 '(1 2 0 3 2 1) :allow-other-keys t :bad t
               :allow-other-keys nil)
  2)

(deftest position.allow-other-keys.9
  (position 0 '(1 2 0 3 2 1) :allow-other-keys t
               :allow-other-keys nil :bad t)
  2)

;;; Error tests

(deftest position.error.1
  (check-type-error #'(lambda (x) (position 'a x)) #'sequencep)
  nil)

(deftest position.error.4
  (signals-error (position 'e '(a b c . d)) type-error)
  t)

(deftest position.error.5
  (signals-error (position) program-error)
  t)

(deftest position.error.6
  (signals-error (position 'a) program-error)
  t)

(deftest position.error.7
  (signals-error (position 'a nil :key) program-error)
  t)

(deftest position.error.8
  (signals-error (position 'a nil 'bad t) program-error)
  t)

(deftest position.error.9
  (signals-error (position 'a nil 'bad t :allow-other-keys nil) program-error)
  t)

(deftest position.error.10
  (signals-error (position 'a nil 1 2) program-error)
  t)

(deftest position.error.11
  (signals-error (locally (position 'a 'b) t) type-error)
  t)

(deftest position.error.12
  (signals-error (position 'b '(a b c d) :test #'identity) program-error)
  t)

(deftest position.error.13
  (signals-error (position 'b '(a b c d) :test-not #'not) program-error)
  t)

(deftest position.error.14
  (signals-error (position 'b '(a b c d) :key #'cdr) type-error)
  t)

(deftest position.error.15
  (signals-error (position 'b '(a b c d) :key #'cons) program-error)
  t)
