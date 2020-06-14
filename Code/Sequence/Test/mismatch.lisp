;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug 26 23:55:29 2002
;;;; Contains: Tests for MISMATCH

(in-package #:sicl-sequence-test)

(deftest mismatch-list.1
  (mismatch '() '(a b c))
  0)

(deftest mismatch-list.2
  (mismatch '(a b c d) '())
  0)

(deftest mismatch-list.3
  (mismatch '(a b c) '(a b c))
  nil)

(deftest mismatch-list.4
  (mismatch '(a b c) '(a b d))
  2)

(deftest mismatch-list.5
  (mismatch '(a b c) '(b c) :start1 1)
  nil)

(deftest mismatch-list.6
  (mismatch '(a b c d) '(z b c e) :start1 1 :start2 1)
  3)

(deftest mismatch-list.7
  (mismatch '(a b c d) '(z b c e) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-list.8
  (mismatch '(1 2 3 4) '(5 6 7 8) :test #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-list.9
  (mismatch '(1 2 3 4) '(5 6 17 8) :test #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-list.10
  (mismatch '(1 2 3 4) '(10 11 7 123) :test-not #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-list.11
  (mismatch '(1 2 3 4) '(5 6 17 8) :key #'evenp)
  nil)

(deftest mismatch-list.12
  (mismatch '(1 2 3 4) '(5 6 12 8) :key 'oddp)
  2)

(deftest mismatch-list.13
  (mismatch '(1 2 3 4) '(1 2 3 4) :test 'eql)
  nil)

(deftest mismatch-list.14
  (mismatch '(1 2 3 4) '(5 6 7 8) :test-not 'eql)
  nil)

(deftest mismatch-list.15
  (mismatch '(a b c d e f g h i j k) '(a b c c e f g h z j k))
  3)

(deftest mismatch-list.16
  (mismatch '(a b c d e f g h i j k) '(a b c c y f g h z j k) :from-end t)
  9)

(deftest mismatch-list.17
  (mismatch '(a b c) '(a b c a b c d) :from-end t)
  3)

(deftest mismatch-list.18
  (mismatch '(a b c a b c d) '(a b c) :from-end t)
  7)

(deftest mismatch-list.19
  (mismatch '(1 1 1) '(2 2 2 2 2 1 2 2) :from-end t :test-not 'eql)
  1)

(deftest mismatch-list.20
  (mismatch '(1 1 1 1 1 1 1) '(2 3 3) :from-end t :key #'evenp)
  5)

(deftest mismatch-list.21
  (mismatch '(1 1 1) '(2 2 2 2 2 1 2 2) :from-end t :test-not #'equal)
  1)

(deftest mismatch-list.22
  (mismatch '(1 1 1 1 1 1 1) '(2 3 3) :from-end t :key 'evenp)
  5)

;;; tests on vectors

(deftest mismatch-vector.1
  (mismatch #() #(a b c))
  0)

(deftest mismatch-vector.2
  (mismatch #(a b c d) #())
  0)

(deftest mismatch-vector.3
  (mismatch #(a b c) #(a b c))
  nil)

(deftest mismatch-vector.4
  (mismatch #(a b c) #(a b d))
  2)

(deftest mismatch-vector.5
  (mismatch #(a b c) #(b c) :start1 1)
  nil)

(deftest mismatch-vector.6
  (mismatch #(a b c d) #(z b c e) :start1 1 :start2 1)
  3)

(deftest mismatch-vector.7
  (mismatch #(a b c d) #(z b c e) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-vector.8
  (mismatch #(1 2 3 4) #(5 6 7 8) :test #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-vector.9
  (mismatch #(1 2 3 4) #(5 6 17 8) :test #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-vector.10
  (mismatch #(1 2 3 4) #(10 11 7 123) :test-not #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-vector.11
  (mismatch #(1 2 3 4) #(5 6 17 8) :key #'evenp)
  nil)

(deftest mismatch-vector.12
  (mismatch #(1 2 3 4) #(5 6 12 8) :key 'oddp)
  2)

(deftest mismatch-vector.13
  (mismatch #(1 2 3 4) #(1 2 3 4) :test 'eql)
  nil)

(deftest mismatch-vector.14
  (mismatch #(1 2 3 4) #(5 6 7 8) :test-not 'eql)
  nil)

(deftest mismatch-vector.15
  (mismatch #(a b c d e f g h i j k) #(a b c c e f g h z j k))
  3)

(deftest mismatch-vector.16
  (mismatch #(a b c d e f g h i j k) #(a b c c y f g h z j k) :from-end t)
  9)

(deftest mismatch-vector.17
  (mismatch #(a b c) #(a b c a b c d) :from-end t)
  3)

(deftest mismatch-vector.18
  (mismatch #(a b c a b c d) #(a b c) :from-end t)
  7)

(deftest mismatch-vector.19
  (mismatch #(1 1 1) #(2 2 2 2 2 1 2 2) :from-end t :test-not 'eql)
  1)

(deftest mismatch-vector.20
  (mismatch #(1 1 1 1 1 1 1) #(2 3 3) :from-end t :key #'evenp)
  5)

(deftest mismatch-vector.21
  (mismatch #(1 1 1) #(2 2 2 2 2 1 2 2) :from-end t :test-not #'equal)
  1)

(deftest mismatch-vector.22
  (mismatch #(1 1 1 1 1 1 1) #(2 3 3) :from-end t :key 'evenp)
  5)

(deftest mismatch-vector.23
  (let ((a (make-array '(9) :initial-contents '(1 2 3 4 5 6 7 8 9)
                       :fill-pointer 5)))
    (values
     (mismatch '(1 2 3 4 5) a)
     (mismatch '(1 2 3 4 5) a :from-end t)
     (mismatch '(1 2 3 4) a)
     (mismatch '(1 2 3 4 5 6) a)
     (mismatch '(6 7 8 9) a :from-end t)
     (mismatch '(2 3 4 5) a :from-end t)))
  nil nil 4 5 4 0)

(deftest mismatch-vector.24
  (let ((m (make-array '(6) :initial-contents '(1 2 3 4 5 6)
                       :fill-pointer 4))
        (a '(1 2 3 4 5)))
    (list
     (mismatch m a)
     (mismatch m a :from-end t)
     (setf (fill-pointer m) 5)
     (mismatch m a)
     (mismatch m a :from-end t)
     (setf (fill-pointer m) 6)
     (mismatch m a)
     (mismatch m a :from-end t)))
  (4 4 5 nil nil 6 5 6))

;;; tests on bit vectors

(deftest mismatch-bit-vector.1
  (mismatch "" #*111)
  0)

(deftest mismatch-bit-vector.1a
  (mismatch '() #*111)
  0)

(deftest mismatch-bit-vector.1b
  (mismatch "" '(1 1 1))
  0)

(deftest mismatch-bit-vector.2
  (mismatch #*1010 #*)
  0)

(deftest mismatch-bit-vector.2a
  (mismatch #*1010 '())
  0)

(deftest mismatch-bit-vector.2b
  (mismatch '(1 0 1 0) #*)
  0)

(deftest mismatch-bit-vector.3
  (mismatch #*101 #*101)
  nil)

(deftest mismatch-bit-vector.4
  (mismatch #*101 #*100)
  2)

(deftest mismatch-bit-vector.5
  (mismatch #*101  #*01 :start1 1)
  nil)

(deftest mismatch-bit-vector.6
  (mismatch #*0110 #*0111 :start1 1 :start2 1)
  3)

(deftest mismatch-bit-vector.7
  (mismatch #*0110 #*0111 :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-bit-vector.7a
  (mismatch '(0 1 1 0) #*0111 :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-bit-vector.7b
  (mismatch #*0110 '(0 1 1 1) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-bit-vector.8
  (mismatch #*1001 #*0110 :test #'(lambda (x y) (= x (- 1 y))))
  nil)

(deftest mismatch-bit-vector.8a
  (mismatch #*1001 '(5 4 4 5) :test #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-bit-vector.9
  (mismatch #*1001 '(5 4 17 5) :test #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-bit-vector.9a
  (mismatch '(5 4 17 5) #*1001 :test #'(lambda (x y) (= y (- x 4))))
  2)

(deftest mismatch-bit-vector.9b
  (mismatch #*0100 #*1001 :test #'(lambda (x y) (= x (- 1 y))))
  2)

(deftest mismatch-bit-vector.10
  (mismatch #*1001 '(10 11 4 123) :test-not #'(lambda (x y) (= x (- y 4))))
  2)

(deftest mismatch-bit-vector.10a
  (mismatch #*1001 '(10 11 100 123) :test-not #'(lambda (x y) (= x (- y 4))))
  nil)

(deftest mismatch-bit-vector.11
  (mismatch #*1010 '(5 6 17 8) :key #'evenp)
  nil)

(deftest mismatch-bit-vector.11a
  (mismatch '(5 6 17 8) #*1010 :key #'evenp)
  nil)

(deftest mismatch-bit-vector.11b
  (mismatch #*0101 #*1010 :key #'evenp :test-not 'eql)
  nil)

(deftest mismatch-bit-vector.11c
  (mismatch '(5 6 17 8) #*10101 :key #'evenp)
  4)

(deftest mismatch-bit-vector.11d
  (mismatch '(5 6 17 8 100) #*1010 :key #'evenp)
  4)

(deftest mismatch-bit-vector.12
  (mismatch #*1010 #*1000 :key 'oddp)
  2)

(deftest mismatch-bit-vector.12a
  (mismatch #*1010 '(5 6 8 8) :key 'oddp)
  2)

(deftest mismatch-bit-vector.12b
  (mismatch '(5 6 8 8) #*1010 :key 'oddp)
  2)

(deftest mismatch-bit-vector.13
  (mismatch #*0001 #*0001 :test 'eql)
  nil)

(deftest mismatch-bit-vector.14
  (mismatch '#*10001 #*01110 :test-not 'eql)
  nil)

(deftest mismatch-bit-vector.15
  (mismatch #*00100010100 #*00110010000)
  3)

(deftest mismatch-bit-vector.16
  (mismatch #*00100010100 #*00110010000 :from-end t)
  9)

(deftest mismatch-bit-vector.17
  (mismatch #*001 #*0010010 :from-end t)
  3)

(deftest mismatch-bit-vector.18
  (mismatch #*0010010 #*001 :from-end t)
  7)

(deftest mismatch-bit-vector.19
  (mismatch #*000 #*11111011 :from-end t :test-not 'eql)
  1)

(deftest mismatch-bit-vector.20
  (mismatch #*1111111 '(2 3 3) :from-end t :key #'evenp)
  5)

(deftest mismatch-bit-vector.21
  (mismatch #*111 #*00000100 :from-end t :test-not #'equal)
  1)

(deftest mismatch-bit-vector.22
  (mismatch #*1111111 '(2 3 3) :from-end t :key 'evenp)
  5)

(deftest mismatch-bit-vector.23
  (let ((a (make-array '(9) :initial-contents #*001011000
                       :fill-pointer 5
                       :element-type 'bit)))
    (values
     (mismatch #*00101 a)
     (mismatch #*00101 a :from-end t)
     (mismatch #*0010 a)
     (mismatch #*001011 a)
     (mismatch #*1000 a :from-end t)
     (mismatch #*0010 a :from-end t)))
  nil nil 4 5 4 4)

(deftest mismatch-bit-vector.24
  (let ((m (make-array '(6) :initial-contents #*001011
                       :fill-pointer 4
                       :element-type 'bit))
        (a #*00101))
    (list
     (mismatch m a)
     (mismatch m a :from-end t)
     (setf (fill-pointer m) 5)
     (mismatch m a)
     (mismatch m a :from-end t)
     (setf (fill-pointer m) 6)
     (mismatch m a)
     (mismatch m a :from-end t)))
  (4 4 5 nil nil 6 5 5))

;;; tests on strings

(deftest mismatch-string.1
  (mismatch "" "111")
  0)

(deftest mismatch-string.1a
  (mismatch '() "111")
  0)

(deftest mismatch-string.1b
  (mismatch "" '(1 1 1))
  0)

(deftest mismatch-string.2
  (mismatch "1010" "")
  0)

(deftest mismatch-string.2a
  (mismatch "1010" '())
  0)

(deftest mismatch-string.2b
  (mismatch '(1 0 1 0) "")
  0)

(deftest mismatch-string.3
  (mismatch "101" "101")
  nil)

(deftest mismatch-string.4
  (mismatch "101" "100")
  2)

(deftest mismatch-string.5
  (mismatch "101" "01" :start1 1)
  nil)

(deftest mismatch-string.6
  (mismatch "0110" "0111" :start1 1 :start2 1)
  3)

(deftest mismatch-string.7
  (mismatch "0110" "0111" :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-string.7a
  (mismatch '(#\0 #\1 #\1 #\0) "0111" :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-string.7b
  (mismatch "0110" '(#\0 #\1 #\1 #\1) :start1 1 :start2 1 :end1 3 :end2 3)
  nil)

(deftest mismatch-string.8
  (mismatch "1001" "0110" :test #'(lambda (x y) (eql x (if (eql y #\0)
                                                           #\1 #\0))))
  nil)

(deftest mismatch-string.8a
  (mismatch "1001" '(5 4 4 5) :test #'(lambda (x y)
                                        (setq x (read-from-string (string x)))
                                        (= x (- y 4))))
  nil)

(deftest mismatch-string.9
  (mismatch "1001" '(5 4 17 5) :test #'(lambda (x y)
                                         (setq x (read-from-string (string x)))
                                         (= x (- y 4))))
  2)

(deftest mismatch-string.9a
  (mismatch '(5 4 17 5) "1001" :test #'(lambda (x y)
                                         (setq y (read-from-string (string y)))
                                         (= y (- x 4))))
  2)

(deftest mismatch-string.9b
  (mismatch "0100" "1001" :test #'(lambda (x y) (eql x (if (eql y #\0) #\1 #\0))))
  2)

(deftest mismatch-string.10
  (mismatch "1001" "0049" :test-not #'(lambda (x y)
                                         (setq x (read-from-string (string x)))
                                         (setq y (read-from-string (string y)))
                                         (eql x (- y 4))))
  2)

(deftest mismatch-string.10a
  (mismatch "1001" "3333" :test-not #'(lambda (x y)
                                        (setq x (read-from-string (string x)))
                                        (setq y (read-from-string (string y)))
                                        (eql x (- y 4))))
  nil)

(deftest mismatch-string.11
  (mismatch "1010" "5678"  :key #'evendigitp)
  nil)

(deftest mismatch-string.11a
  (mismatch "5678" "1010" :key #'odddigitp)
  nil)

(deftest mismatch-string.11b
  (mismatch "0101" "1010" :key #'evendigitp :test-not 'eql)
  nil)

(deftest mismatch-string.11c
  (mismatch "5678" "10101" :key #'evendigitp)
  4)

(deftest mismatch-string.11d
  (mismatch "56122" "1010" :key #'evendigitp)
  4)

(deftest mismatch-string.11e
  (mismatch "0101" '(#\1 #\0 #\1 #\0) :key #'evendigitp :test-not 'eql)
  nil)

(deftest mismatch-string.12
  (mismatch "1010" "1000" :key 'odddigitp)
  2)

(deftest mismatch-string.12a
  (mismatch "1010" "5688" :key 'odddigitp)
  2)

(deftest mismatch-string.12b
  (mismatch '(#\5 #\6 #\8 #\8) "1010" :key 'odddigitp)
  2)

(deftest mismatch-string.13
  (mismatch "0001" "0001" :test 'eql)
  nil)

(deftest mismatch-string.14
  (mismatch "10001" "01110" :test-not 'eql)
  nil)

(deftest mismatch-string.15
  (mismatch "00100010100" "00110010000")
  3)

(deftest mismatch-string.16
  (mismatch "00100010100" "00110010000" :from-end t)
  9)

(deftest mismatch-string.17
  (mismatch "001" "0010010" :from-end t)
  3)

(deftest mismatch-string.18
  (mismatch "0010010" "001" :from-end t)
  7)

(deftest mismatch-string.19
  (mismatch "000" "11111011" :from-end t :test-not 'eql)
  1)

(deftest mismatch-string.20
  (mismatch "1111111" "233" :from-end t :key #'evendigitp)
  5)

(deftest mismatch-string.20a
  (mismatch "1111111" '(#\2 #\3 #\3) :from-end t :key #'evendigitp)
  5)

(deftest mismatch-string.21
  (mismatch "111" "00000100" :from-end t :test-not #'equal)
  1)

(deftest mismatch-string.22
  (mismatch "1111111" "233" :from-end t :key 'evendigitp)
  5)

(deftest mismatch-string.23
  (let ((a (make-array '(9) :initial-contents "123456789"
                       :fill-pointer 5
                       :element-type 'character)))
    (values
     (mismatch "12345" a)
     (mismatch "12345" a :from-end t)
     (mismatch "1234" a)
     (mismatch "123456" a)
     (mismatch "6789" a :from-end t)
     (mismatch "2345" a :from-end t)))
  nil nil 4 5 4 0)

(deftest mismatch-string.24
  (let ((m (make-array '(6) :initial-contents "123456"
                       :fill-pointer 4
                       :element-type 'character))
        (a "12345"))
    (list
     (mismatch m a)
     (mismatch m a :from-end t)
     (setf (fill-pointer m) 5)
     (mismatch m a)
     (mismatch m a :from-end t)
     (setf (fill-pointer m) 6)
     (mismatch m a)
     (mismatch m a :from-end t)))
  (4 4 5 nil nil 6 5 6))

(deftest mistmatch-string.25
  (let ((s0 "12345")
        (s1 "123A")
        (s2 "245"))
    (do-special-strings
     (s s0 nil)
     (assert (null (mismatch s s0)))
     (assert (null (mismatch s0 s)))
     (assert (null (mismatch s s0 :from-end t)))
     (assert (null (mismatch s0 s :from-end t)))
     (assert (eql (mismatch s s1) 3))
     (assert (eql (mismatch s1 s) 3))
     ))
  nil)

;;; test and test-not tests

(defharmless mismatch.test-and-test-not.1
  (mismatch '(1 2 3) '(1 2 4) :test #'eql :test-not #'eql))

(defharmless mismatch.test-and-test-not.2
  (mismatch '(1 2 3) '(1 2 4) :test-not #'eql :test #'eql))

(defharmless mismatch.test-and-test-not.3
  (mismatch #(1 2 3) #(1 2 4) :test #'eql :test-not #'eql))

(defharmless mismatch.test-and-test-not.4
  (mismatch #(1 2 3) #(1 2 4) :test-not #'eql :test #'eql))

(defharmless mismatch.test-and-test-not.5
  (mismatch "abc" "abd" :test #'eql :test-not #'eql))

(defharmless mismatch.test-and-test-not.6
  (mismatch "abc" "abd" :test-not #'eql :test #'eql))

(defharmless mismatch.test-and-test-not.7
  (mismatch #*011 #*010 :test #'eql :test-not #'eql))

(defharmless mismatch.test-and-test-not.8
  (mismatch #*011 #*010 :test-not #'eql :test #'eql))

;;; Keyword tests

(deftest mismatch.allow-other-keys.1
  (mismatch "1234" "1244" :allow-other-keys t :bad t)
  2)

(deftest mismatch.allow-other-keys.2
  (mismatch "1234" "1244" :bad t :allow-other-keys t)
  2)

(deftest mismatch.allow-other-keys.3
  (mismatch "1234" "1244" :bad t :allow-other-keys t :allow-other-keys nil)
  2)

(deftest mismatch.allow-other-keys.4
  (mismatch "1234" "1244" :allow-other-keys t :bad t
            :allow-other-keys nil)
  2)

(deftest mismatch.allow-other-keys.5
  (mismatch "1234" "1244" :allow-other-keys t
            :allow-other-keys nil
            :bad t)
  2)

(deftest mismatch.keywords.6
  (mismatch "1234" "1244" :test #'equal :test (complement #'equal))
  2)

(deftest mismatch.allow-other-keys.7
  (mismatch "1234" "1244" :bad t :allow-other-keys t
            :test (complement #'equal))
  0)

;;; Order of evaluation

(deftest mismatch.order.1
  (let ((i 0) a b)
    (values
     (mismatch (progn (setf a (incf i)) "abcd")
               (progn (setf b (incf i)) "abzd"))
     i a b))
  2 2 1 2)

(deftest mismatch.order.2
  (let ((i 0) a b c d e f g h j)
    (values
     (mismatch (progn (setf a (incf i)) "abcdef")
               (progn (setf b (incf i)) "abcdef")
               :key (progn (setf c (incf i)) #'identity)
               :test (progn (setf d (incf i)) #'equal)
               :start1 (progn (setf e (incf i)) 1)
               :start2 (progn (setf f (incf i)) 1)
               :end1 (progn (setf g (incf i)) 4)
               :end2 (progn (setf h (incf i)) 4)
               :from-end (setf j (incf i)))
     i a b c d e f g h j))
  nil 9 1 2 3 4 5 6 7 8 9)

(deftest mismatch.order.3
  (let ((i 0) a b c d e f g h j)
    (values
     (mismatch (progn (setf a (incf i)) "abcdef")
               (progn (setf b (incf i)) "abcdef")
               :from-end (setf c (incf i))
               :end2 (progn (setf d (incf i)) 4)
               :end1 (progn (setf e (incf i)) 4)
               :start2 (progn (setf f (incf i)) 1)
               :start1 (progn (setf g (incf i)) 1)
               :test (progn (setf h (incf i)) #'equal)
               :key (progn (setf j (incf i)) #'identity))
     i a b c d e f g h j))
  nil 9 1 2 3 4 5 6 7 8 9)


;;; Error cases

(deftest mismatch.error.1
  (signals-error (mismatch) program-error)
  t)

(deftest mismatch.error.2
  (signals-error (mismatch nil) program-error)
  t)

(deftest mismatch.error.3
  (signals-error (mismatch nil nil :bad t) program-error)
  t)

(deftest mismatch.error.4
  (signals-error (mismatch nil nil :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest mismatch.error.5
  (signals-error (mismatch nil nil :key) program-error)
  t)

(deftest mismatch.error.6
  (signals-error (mismatch nil nil 1 2) program-error)
  t)

(deftest mismatch.error.7
  (signals-error (mismatch '(a b) '(a b) :test #'identity) program-error)
  t)

(deftest mismatch.error.8
  (signals-error (mismatch '(a b) '(a b) :test-not #'identity) program-error)
  t)

(deftest mismatch.error.9
  (signals-error (mismatch '(a b) '(a b) :key #'car) type-error)
  t)

(deftest mismatch.error.10
  (signals-error (mismatch '(a b) '(a b) :key #'cons) program-error)
  t)

