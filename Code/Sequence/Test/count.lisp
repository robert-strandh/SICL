;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Aug 19 07:31:55 2002
;;;; Contains: Tests for COUNT

(in-package #:sicl-sequence-test)

(deftest count-list.1
  (count 'a '(a b c d e a e f))
  2)

(deftest count-list.2
  (count 'a '(a b c d e a e f) :test #'eql)
  2)

(deftest count-list.3
  (count 'a '(a b c d e a e f) :test 'eql)
  2)

(deftest count-list.4
  (count 1 '(1 2 2 3 2 1 2 2 5 4) :key #'1-)
  5)

(deftest count-list.5
  (count 1 '(1 2 2 3 2 1 2 2 5 4) :key '1-)
  5)

(deftest count-list.6
  (count 1 '(1 2 2 3 2 1 2 2 5 4) :key #'1- :test #'equal)
  5)

(deftest count-list.7
  (count 1 '(2 1 1 2 3 1 4 1 7 6 1 8) :from-end t)
  5)

(deftest count-list.8
  (let ((c 0))
    (count 1 '(1 2 3 1 4 1 7 6 1 8)
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  4)

(deftest count-list.9
  (let ((c 0))
    (count 1 '(1 2 3 7 4 5 7 6 2 8)
           :from-end t
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  3)

(deftest count-list.10
  (count 1 '(1 1 1 1 1 2 1 1) :start 3)
  4)

(deftest count-list.11
  (count 1 '(1 1 1 1 1 2 1 1) :end 6)
  5)

(deftest count-list.12
  (count 1 '(1 1 1 1 1 2 1 1) :start 2 :end 7)
  4)

(deftest count-list.13
  (count 1 '(1 1 1 1 1 2 1 1) :start 3 :end nil)
  4)

(deftest count-list.14
  (count 1 '(1 1 1 1 1 2 1 1)  :end nil)
  7)

(deftest count-list.15
  (count 1 '(1 1 1 1 1 2 1 1)  :test-not #'eql)
  1)

(deftest count-list.16
  (count 1 '(1 1 1 3 1 2 1 1) :start 2 :end 7
         :test #'(lambda (x y) (declare (ignore x y))  t))
  5)

(deftest count-list.17
  (count 10 '(1 11 2 4 14 5 18 6 7) :test #'<)
  3)

(deftest count-list.18
  (count 10 '(1 11 2 4 14 5 18 6 7) :test-not #'>=)
  3)

(defharmless count-list.test-and-test-not.1
  (count 0 '(0 1 2 0 1 2 3 0 1) :test #'eql :test-not #'eql))

(defharmless count-list.test-and-test-not.2
  (count 0 '(0 1 2 0 1 2 3 0 1) :test-not #'eql :test #'eql))

;;; On vectors

(deftest count-vector.1
  (count 'a #(a b c d e a e f))
  2)

(deftest count-vector.2
  (count 'a #(a b c d e a e f) :test #'eql)
  2)

(deftest count-vector.3
  (count 'a #(a b c d e a e f) :test 'eql)
  2)

(deftest count-vector.4
  (count 1 #(1 2 2 3 2 1 2 2 5 4) :key #'1-)
  5)

(deftest count-vector.5
  (count 1 #(1 2 2 3 2 1 2 2 5 4) :key '1-)
  5)

(deftest count-vector.6
  (count 1 #(1 2 2 3 2 1 2 2 5 4) :key #'1- :test #'equal)
  5)

(deftest count-vector.7
  (count 1 #(2 1 1 2 3 1 4 1 7 6 1 8) :from-end t)
  5)

(deftest count-vector.8
  (let ((c 0))
    (count 1 #(1 2 3 1 4 1 7 6 1 8)
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  4)

(deftest count-vector.9
  (let ((c 0))
    (count 1 #(1 2 3 7 4 5 7 6 2 8)
           :from-end t
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  3)

(deftest count-vector.10
  (count 1 #(1 1 1 1 1 2 1 1) :start 3)
  4)

(deftest count-vector.11
  (count 1 #(1 1 1 1 1 2 1 1) :end 6)
  5)

(deftest count-vector.12
  (count 1 #(1 1 1 1 1 2 1 1) :start 2 :end 7)
  4)

(deftest count-vector.13
  (count 1 #(1 1 1 1 1 2 1 1) :start 3 :end nil)
  4)

(deftest count-vector.14
  (count 1 #(1 1 1 1 1 2 1 1)  :end nil)
  7)

(deftest count-vector.15
  (count 1 #(1 1 1 1 1 2 1 1)  :test-not #'eql)
  1)

(deftest count-vector.16
  (count 1 #(1 1 1 3 1 2 1 1) :start 2 :end 7
         :test #'(lambda (x y) (declare (ignore x y)) t))
  5)

(deftest count-vector.17
  (count 10 #(1 11 2 4 14 5 18 6 7) :test #'<)
  3)

(deftest count-vector.18
  (count 10 #(1 11 2 4 14 5 18 6 7) :test-not #'>=)
  3)

(defharmless count-vector.test-and-test-not.1
  (count 0 #(0 1 2 0 1 2 3 0 1) :test #'eql :test-not #'eql))

(defharmless count-vector.test-and-test-not.2
  (count 0 #(0 1 2 0 1 2 3 0 1) :test-not #'eql :test #'eql))

;;; Non-simple vectors

(deftest count-filled-vector.1
  (count 'a (make-array 8 :initial-contents '(a b c d e a e f)
                        :fill-pointer t))
  2)

(deftest count-filled-vector.2
  (count 'a (make-array 8 :initial-contents '(a b c d e a e f)
                        :fill-pointer t)
         :test #'eql)
  2)

(deftest count-filled-vector.3
  (count 'a (make-array 8 :initial-contents '(a b c d e a e f)
                        :fill-pointer t)
         :test 'eql)
  2)

(deftest count-filled-vector.4
  (count 1 (make-array 10 :initial-contents '(1 2 2 3 2 1 2 2 5 4)
                       :fill-pointer t)
         :key #'1-)
  5)

(deftest count-filled-vector.5
  (count 1 (make-array 10 :initial-contents '(1 2 2 3 2 1 2 2 5 4)
                       :fill-pointer t)
         :key '1-)
  5)

(deftest count-filled-vector.6
  (count 1 (make-array 10 :initial-contents '(1 2 2 3 2 1 2 2 5 4)
                       :fill-pointer t)
         :key #'1- :test #'equal)
  5)

(deftest count-filled-vector.7
  (count 1 (make-array 12 :initial-contents '(2 1 1 2 3 1 4 1 7 6 1 8)
                       :fill-pointer t)
         :from-end t)
  5)

(deftest count-filled-vector.8
  (let ((c 0))
    (count 1 (make-array 10 :initial-contents '(1 2 3 1 4 1 7 6 1 8)
                         :fill-pointer t)
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  4)

(deftest count-filled-vector.9
  (let ((c 0))
    (count 1 (make-array 10 :initial-contents '(1 2 3 7 4 5 7 6 2 8)
                         :fill-pointer t)
           :from-end t
           :key #'(lambda (x)
                    ;; (format t "~%~A ~A" x c)
                    (prog1 (- x c) (incf c)))))
  3)

(deftest count-filled-vector.10
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 2 1 1)
                       :fill-pointer t)
         :start 3)
  4)

(deftest count-filled-vector.11
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 2 1 1)
                       :fill-pointer t)
         :end 6)
  5)

(deftest count-filled-vector.12
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 2 1 1)
                       :fill-pointer t)
         :start 2 :end 7)
  4)

(deftest count-filled-vector.13
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 2 1 1)
                       :fill-pointer t)
         :start 3 :end nil)
  4)

(deftest count-filled-vector.14
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 2 1 1)
                       :fill-pointer t)
         :end nil)
  7)

(deftest count-filled-vector.15
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 2 1 1)
                       :fill-pointer t)
         :test-not #'eql)
  1)

(deftest count-filled-vector.16
  (count 1 (make-array 8 :initial-contents '(1 1 1 3 1 2 1 1)
                       :fill-pointer t)
         :start 2 :end 7
         :test #'(lambda (x y) (declare (ignore x y)) t))
  5)

(deftest count-filled-vector.17
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 1 1 1)
                       :fill-pointer 6))
  6)

(deftest count-filled-vector.18
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 1 1 1)
                       :fill-pointer 6)
         :start 2)
  4)
(deftest count-filled-vector.19
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 1 1 1)
                       :fill-pointer 6)
         :from-end 'foo)
  6)

(deftest count-filled-vector.20
  (count 1 (make-array 8 :initial-contents '(1 1 1 1 1 1 1 1)
                       :fill-pointer 6)
         :start 2 :from-end 'yes)
  4)

;;; Other specialized vectors

(deftest count.special-vector.1
  (do-special-integer-vectors
   (v #(0 1 1 0 1 1 1 0 1 1 1 1 0) nil)
   (assert (eql (count 0 v) 4))
   (assert (eql (count 1 v) 9))
   (assert (eql (count 2 v) 0))
   (assert (eql (count 0 v :start 2) 3))
   (assert (eql (count 1 v :end 11) 8)))
  nil)

(deftest count.special-vector.2
  (do-special-integer-vectors
   (v #(1 2 3 4 5 6 7) nil)
   (assert (eql (count 0 v) 0))
   (assert (eql (count 1 v) 1))
   (assert (eql (count 2 v) 1))
   (assert (eql (count 3 v) 1))
   (assert (eql (count 4 v) 1))
   (assert (eql (count 5 v) 1))
   (assert (eql (count 6 v) 1))
   (assert (eql (count 7 v) 1)))
  nil)

(deftest count.special-vector.3
  (loop for etype in '(short-float single-float double-float long-float)
        for vals = (loop for e in '(0 1 2 1 3 1 4 5 6 0)
                         collect (coerce e etype))
        for vec = (make-array (length vals) :element-type etype :initial-contents vals)
        for result = (count (coerce 1 etype) vec)
        unless (= result 3)
        collect (list etype vals vec result))
  nil)

(deftest count.special-vector.4
  (loop for cetype in '(short-float single-float double-float long-float rational integer)
        for etype = `(complex ,cetype)
        for vals = (loop for e in '(4 1 2 1 3 1 4 5 6 6)
                         collect (complex 0 (coerce e cetype)))
        for vec = (make-array (length vals) :element-type etype :initial-contents vals)
        for result = (count (complex 0 (coerce 1 cetype)) vec)
        unless (= result 3)
        collect (list etype vals vec result))
  nil)



;;; Tests on bit vectors

(deftest count-bit-vector.1
  (count 1 #*00101100011011000)
  7)

(deftest count-bit-vector.2
  (count 1 #*00101100011011000 :test #'eql)
  7)

(deftest count-bit-vector.3
  (count 1 #*00101100011011000 :test 'eql)
  7)

(deftest count-bit-vector.4
  (count 1 #*00101100011011000 :key #'1+)
  10)

(deftest count-bit-vector.5
  (count 0 #*00101100011011000 :key '1-)
  7)

(deftest count-bit-vector.6
  (count 0 #*00101100011011000 :key #'1- :test #'equal)
  7)

(deftest count-bit-vector.7
  (count 1 #*00101100011011000 :from-end t)
  7)

(deftest count-bit-vector.8
  (let ((c 1))
    (count 0 #*0000110101001
           :key #'(lambda (x) (setf c (- c)) (+ c x))))
  2)

(deftest count-bit-vector.9
  (let ((c 1))
    (count 0 #*0000011010101
           :from-end t
           :key #'(lambda (x) (setf c (- c)) (+ c x))))
  4)

(deftest count-bit-vector.10
  (count 1 #*11000110110 :start 3)
  4)

(deftest count-bit-vector.11
  (count 1 '#*110111110111 :end 6)
  5)

(deftest count-bit-vector.12
  (count 1 #*11111011 :start 2 :end 7)
  4)

(deftest count-bit-vector.13
  (count 1 #*11111011 :start 3 :end nil)
  4)

(deftest count-bit-vector.14
  (count 1 #*11111011 :end nil)
  7)

(deftest count-bit-vector.15
  (count 1 #*11111011  :test-not #'eql)
  1)

(deftest count-bit-vector.16
  (count 1 #*11101101 :start 2 :end 7
         :test #'(lambda (x y) (declare (ignore x y)) t))
  5)

(deftest count-bit-vector.17
  (count 1 (make-array 8 :initial-contents '(1 0 1 1 1 1 1 1)
                       :element-type 'bit
                       :fill-pointer 5))
  4)

(deftest count-bit-vector.18
  (count 1 (make-array 8 :initial-contents '(1 0 1 1 1 1 1 1)
                       :element-type 'bit
                       :fill-pointer 5)
         :start 1)
  3)

(deftest count-bit-vector.19
  (count 1 (make-array 8 :initial-contents '(1 0 1 1 1 1 1 1)
                       :element-type 'bit
                       :fill-pointer 5)
         :end nil)
  4)


(deftest count-bit-vector.20
  (count 1 (make-array 8 :initial-contents '(1 0 1 1 1 1 1 1)
                       :element-type 'bit
                       :fill-pointer 6)
         :end 4)
  3)

(deftest count-bit-vector.21
  (count 1 #*00001100100 :test #'<=)
  3)

(deftest count-bit-vector.22
  (count 1 #*00001100100 :test-not #'>)
  3)

(defharmless count-bit-vector.test-and-test-not.1
  (count 0 #*0011010101100010000 :test #'eql :test-not #'eql))

(defharmless count-bit-vector.test-and-test-not.2
  (count 0 #*0011010101100010000 :test-not #'eql :test #'eql))

;;; Tests on strings

(deftest count-string.1
  (count #\1 "00101100011011000")
  7)

(deftest count-string.2
  (count #\1 "00101100011011000" :test #'eql)
  7)

(deftest count-string.3
  (count #\1 "00101100011011000" :test 'eql)
  7)

(deftest count-string.4
  (count #\1 "00101100011011000" :key #'(lambda (x) (if (eql x #\0) #\1 #\2)))
  10)

(deftest count-string.5
  (count #\1 "00101100011011000" :key 'identity)
  7)

(deftest count-string.6
  (count #\1 "00101100011011000" :key #'identity :test #'equal)
  7)

(deftest count-string.7
  (count #\1 "00101100011011000" :from-end t)
  7)

(deftest count-string.8
  (let ((c nil))
    (count #\0 "0000110101001"
           :key #'(lambda (x) (setf c (not c))
                    (and c x))))
  5)

(deftest count-string.9
  (let ((c nil))
    (count #\0 "0000011010101"
           :from-end t
           :key #'(lambda (x) (setf c (not c))
                    (and c x))))
  3)

(deftest count-string.10
  (count #\1 "11000110110" :start 3)
  4)

(deftest count-string.11
  (count #\1 '"110111110111" :end 6)
  5)

(deftest count-string.12
  (count #\1 "11111011" :start 2 :end 7)
  4)

(deftest count-string.13
  (count #\1 "11111011" :start 3 :end nil)
  4)

(deftest count-string.14
  (count #\1 "11111011" :end nil)
  7)

(deftest count-string.15
  (count #\1 "11111011"  :test-not #'eql)
  1)

(deftest count-string.16
  (count #\1 "11101101" :start 2 :end 7
         :test #'(lambda (x y) (declare (ignore x y)) t))
  5)

(deftest count-string.17
  (count #\a (make-array 10 :initial-contents "abaaacaaaa"
                         :fill-pointer 7
                         :element-type 'character))
  5)

(deftest count-string.18
  (count #\a (make-array 10 :initial-contents "abaaacaaaa"
                         :fill-pointer 7
                         :element-type 'character)
         :start 1)
  4)

(deftest count-string.19
  (count #\a (make-array 10 :initial-contents "abaaacaaaa"
                         :fill-pointer 7
                         :element-type 'character)
         :end nil)
  5)

(deftest count-string.20
  (count #\a (make-array 10 :initial-contents "abaaacaaaa"
                         :fill-pointer 7
                         :element-type 'character)
         :start 2 :end 5)
  3)

(deftest count-string.21
  (count #\1 "00001100100" :test #'char<=)
  3)

(deftest count-string.22
  (count #\1 "00001100100" :test-not #'char>)
  3)

(deftest count-string.23
  (do-special-strings
   (s "a1a3abcda" nil)
   (assert (= (count #\a s) 4)))
  nil)

(defharmless count-string.test-and-test-not.1
  (count #\0 "0011010101100010000" :test #'eql :test-not #'eql))

(defharmless count-string.test-and-test-not.2
  (count #\0 "0011010101100010000" :test-not #'eql :test #'eql))

;;; Argument order tests

(deftest count.order.1
  (let ((i 0) c1 c2 c3 c4 c5 c6 c7)
    (values
     (count (progn (setf c1 (incf i)) nil)
            (progn (setf c2 (incf i)) '(a nil b c nil d e))
            :start (progn (setf c3 (incf i)) 0)
            :end (progn (setf c4 (incf i)) 3)
            :key (progn (setf c5 (incf i)) #'identity)
            :from-end (progn (setf c6 (incf i)) nil)
            :test (progn (setf c7 (incf i)) #'eql)
            )
     i c1 c2 c3 c4 c5 c6 c7))
  1 7 1 2 3 4 5 6 7)

(deftest count.order.2
  (let ((i 0) c1 c2 c3 c4 c5 c6 c7)
    (values
     (count (progn (setf c1 (incf i)) nil)
            (progn (setf c2 (incf i)) '(a nil b c nil d e))
            :test (progn (setf c3 (incf i)) #'eql)
            :from-end (progn (setf c4 (incf i)) nil)
            :key (progn (setf c5 (incf i)) #'identity)
            :end (progn (setf c6 (incf i)) 3)
            :start (progn (setf c7 (incf i)) 0)
            )
     i c1 c2 c3 c4 c5 c6 c7))
  1 7 1 2 3 4 5 6 7)


;;; Keyword tests

(deftest count.allow-other-keys.1
  (count 'a '(b a d a c) :bad t :allow-other-keys t)
  2)

(deftest count.allow-other-keys.2
  (count 'a '(b a d a c) :allow-other-keys #p"*" :also-bad t)
  2)

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(deftest count.allow-other-keys.3
  (count 'a '(b a d a c)
         :allow-other-keys t
         :allow-other-keys nil
         :bad t)
  2)

(deftest count.keywords.4
  (count 2 '(1 2 3 2 5) :key #'identity :key #'1+)
  2)

(deftest count.allow-other-keys.5
  (count 'a '(a b c a) :allow-other-keys nil)
  2)

;;; Error tests

(deftest count.error.1
  (check-type-error #'(lambda (x) (count 'a x)) #'sequencep)
  nil)

(deftest count.error.4
  (signals-error (count) program-error)
  t)

(deftest count.error.5
  (signals-error (count nil) program-error)
  t)

(deftest count.error.6
  (signals-error (count nil nil :bad t) program-error)
  t)

(deftest count.error.7
  (signals-error (count nil nil :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest count.error.8
  (signals-error (count nil nil :key) program-error)
  t)

(deftest count.error.9
  (signals-error (count nil nil 3 3) program-error)
  t)

;;; Only leftmost :allow-other-keys argument matters
(deftest count.error.10
  (signals-error (count 'a nil :bad t
                         :allow-other-keys nil
                         :allow-other-keys t)
                 program-error)
  t)

(deftest count.error.11
  (signals-error (locally (count 'a 1) t) type-error)
  t)

(deftest count.error.12
  (signals-error (count 'b '(a b c) :test #'identity)
                 program-error)
  t)

(deftest count.error.13
  (signals-error (count 'b '(a b c) :key #'car) type-error)
  t)

(deftest count.error.14
  (signals-error (count 'b '(a b c) :test-not #'identity)
                 program-error)
  t)

(deftest count.error.15
  (signals-error (count 'b '(a b c) :key #'cons)
                 program-error)
  t)
