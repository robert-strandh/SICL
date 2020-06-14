;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 18:37:52 2002
;;;; Contains: Tests for FIND-IF

(in-package #:sicl-sequence-test)

(deftest find-if-list.1
  (find-if #'identity ())
  nil)

(deftest find-if-list.2
  (find-if #'identity '(a))
  a)

(deftest find-if-list.2a
  (find-if 'identity '(a))
  a)

(deftest find-if-list.3
  (find-if #'evenp '(1 2 4 8 3 1 6 7))
  2)

(deftest find-if-list.4
  (find-if #'evenp '(1 2 4 8 3 1 6 7) :from-end t)
  6)

(deftest find-if-list.5
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-list.6
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :end nil))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-list.7
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-list.8
  (loop for i from 0 to 7 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-list.9
  (loop for i from 0 to 8 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :end i))
  (nil nil 2 2 2 2 2 2 2))

(deftest find-if-list.10
  (loop for i from 0 to 8 collect
        (find-if #'evenp '(1 2 4 8 3 1 6 7) :end i :from-end t))
  (nil nil 2 4 8 8 8 6 6))

(deftest find-if-list.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evenp '(1 2 4 8 3 1 6 7) :start j :end i)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-list.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evenp '(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-list.13
  (loop for i from 0 to 6
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key #'1+ :start i))
  (1 11 11 45 45 71 nil))

(deftest find-if-list.14
  (loop for i from 0 to 6
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))
  (71 71 71 71 71 71 nil))

(deftest find-if-list.15
  (loop for i from 0 to 7
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key #'1+ :end i))
  (nil 1 1 1 1 1 1 1))

(deftest find-if-list.16
  (loop for i from 0 to 7
        collect
        (find-if #'evenp '(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))
  (nil 1 1 11 11 45 71 71))

(deftest find-if-list.17
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'oddp '(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-list.18
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'oddp '(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t :key #'1+)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

;;; tests for vectors

(deftest find-if-vector.1
  (find-if #'identity #())
  nil)

(deftest find-if-vector.2
  (find-if #'identity #(a))
  a)

(deftest find-if-vector.2a
  (find-if 'identity #(a))
  a)

(deftest find-if-vector.3
  (find-if #'evenp #(1 2 4 8 3 1 6 7))
  2)

(deftest find-if-vector.4
  (find-if #'evenp #(1 2 4 8 3 1 6 7) :from-end t)
  6)

(deftest find-if-vector.5
  (loop for i from 0 to 7 collect
        (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-vector.6
  (loop for i from 0 to 7 collect
        (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i :end nil))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-vector.7
  (loop for i from 0 to 7 collect
        (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-vector.8
  (loop for i from 0 to 7 collect
        (find-if #'evenp #(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-vector.9
  (loop for i from 0 to 8 collect
        (find-if #'evenp #(1 2 4 8 3 1 6 7) :end i))
  (nil nil 2 2 2 2 2 2 2))

(deftest find-if-vector.10
  (loop for i from 0 to 8 collect
        (find-if #'evenp #(1 2 4 8 3 1 6 7) :end i :from-end t))
  (nil nil 2 4 8 8 8 6 6))

(deftest find-if-vector.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evenp #(1 2 4 8 3 1 6 7) :start j :end i)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-vector.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evenp #(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-vector.13
  (loop for i from 0 to 6
        collect
        (find-if #'evenp #(1 6 11 32 45 71 100) :key #'1+ :start i))
  (1 11 11 45 45 71 nil))

(deftest find-if-vector.14
  (loop for i from 0 to 6
        collect
        (find-if #'evenp #(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))
  (71 71 71 71 71 71 nil))

(deftest find-if-vector.15
  (loop for i from 0 to 7
        collect
        (find-if #'evenp #(1 6 11 32 45 71 100) :key #'1+ :end i))
  (nil 1 1 1 1 1 1 1))

(deftest find-if-vector.16
  (loop for i from 0 to 7
        collect
        (find-if #'evenp #(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))
  (nil 1 1 11 11 45 71 71))

(deftest find-if-vector.17
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'oddp #(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-vector.18
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'oddp #(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t :key #'1+)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-vector.19
  (let ((a (make-array '(10) :initial-contents '(1 2 3 4 5 6 7 8 9 10)
                       :fill-pointer 5)))
    (values
     (find-if #'evenp a)
     (find-if #'evenp a :from-end t)
     (find-if #'oddp a)
     (find-if #'oddp a :from-end t)
     ))
  2 4 1 5)

;;; Tests for bit vectors

(deftest find-if-bit-vector.1
  (find-if #'identity #*)
  nil)

(deftest find-if-bit-vector.2
  (find-if #'identity #*1)
  1)

(deftest find-if-bit-vector.3
  (find-if #'identity #*0)
  0)

(deftest find-if-bit-vector.4
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if #'evenp #*0110110 :start i :end j)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

(deftest find-if-bit-vector.5
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if #'evenp #*0110110 :start i :end j
                                       :from-end t)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

(deftest find-if-bit-vector.6
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if #'oddp #*0110110 :start i :end j
                                       :from-end t :key #'1+)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

(deftest find-if-bit-vector.7
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if #'oddp #*0110110 :start i :end j
                                       :key '1-)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

;;; Tests for strings

(deftest find-if-string.1
  (find-if #'identity "")
  nil)

(deftest find-if-string.2
  (find-if #'identity "a")
  #\a)

(deftest find-if-string.2a
  (find-if 'identity "a")
  #\a)

(deftest find-if-string.3
  (find-if #'evendigitp "12483167")
  #\2)

(deftest find-if-string.3a
  (find-if #'evenp "12483167" :key #'(lambda (c) (read-from-string (string c))))
  #\2)

(deftest find-if-string.4
  (find-if #'evendigitp "12483167" :from-end t)
  #\6)

(deftest find-if-string.5
  (loop for i from 0 to 7 collect
        (find-if #'evendigitp "12483167" :start i))
  (#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil))

(deftest find-if-string.6
  (loop for i from 0 to 7 collect
        (find-if #'evendigitp "12483167" :start i :end nil))
  (#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil))

(deftest find-if-string.7
  (loop for i from 0 to 7 collect
        (find-if #'evendigitp "12483167" :start i :from-end t))
  (#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil))

(deftest find-if-string.8
  (loop for i from 0 to 7 collect
        (find-if #'evendigitp "12483167" :start i :end nil :from-end t))
  (#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil))

(deftest find-if-string.9
  (loop for i from 0 to 8 collect
        (find-if #'evendigitp "12483167" :end i))
  (nil nil #\2 #\2 #\2 #\2 #\2 #\2 #\2))

(deftest find-if-string.10
  (loop for i from 0 to 8 collect
        (find-if #'evendigitp "12483167" :end i :from-end t))
  (nil nil #\2 #\4 #\8 #\8 #\8 #\6 #\6))

(deftest find-if-string.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evendigitp "12483167" :start j :end i)))
  ((nil #\2 #\2 #\2 #\2 #\2 #\2 #\2)
   (#\2 #\2 #\2 #\2 #\2 #\2 #\2)
   (#\4 #\4 #\4 #\4 #\4 #\4)
   (#\8 #\8 #\8 #\8 #\8)
   (nil nil #\6 #\6)
   (nil #\6 #\6)
   (#\6 #\6)
   (nil)))

(deftest find-if-string.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if #'evendigitp "12483167" :start j :end i
                       :from-end t)))
  ((nil #\2 #\4 #\8 #\8 #\8 #\6 #\6)
   (#\2 #\4 #\8 #\8 #\8 #\6 #\6)
   (#\4 #\8 #\8 #\8 #\6 #\6)
   (#\8 #\8 #\8 #\6 #\6)
   (nil nil #\6 #\6)
   (nil #\6 #\6)
   (#\6 #\6)
   (nil)))

(deftest find-if-string.13
  (loop for i from 0 to 6
        collect
        (find-if #'evenp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start i))
  (#\4 #\4 #\8 #\8 #\8 #\6 #\6))

(deftest find-if-string.14
  (loop for i from 0 to 6
        collect
        (find-if #'evenp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start i :from-end t))
  (#\6 #\6 #\6 #\6 #\6 #\6 #\6))

(deftest find-if-string.15
  (loop for i from 0 to 7
        collect
        (find-if #'evenp "1473816"
                 :key (compose #'read-from-string #'string)
                 :end i))
  (nil nil #\4 #\4 #\4 #\4 #\4 #\4))

(deftest find-if-string.16
  (loop for i from 0 to 7
        collect
        (find-if #'evenp "1473816"
                 :key (compose #'read-from-string #'string)
                 :end i :from-end t))
  (nil nil #\4 #\4 #\4 #\8 #\8 #\6))

(deftest find-if-string.17
  (loop for j from 0 to 6
        collect
        (loop for i from (1+ j) to 7 collect
              (find-if #'evenp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start j :end i)))
  ((nil #\4 #\4 #\4 #\4 #\4 #\4)
   (#\4 #\4 #\4 #\4 #\4 #\4)
   (nil nil #\8 #\8 #\8)
   (nil #\8 #\8 #\8)
   (#\8 #\8 #\8)
   (nil #\6)
   (#\6)))

(deftest find-if-string.18
  (loop for j from 0 to 6
        collect
        (loop for i from (1+ j) to 7 collect
              (find-if #'evenp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start j :end i
                 :from-end t)))
  ((nil #\4 #\4 #\4 #\8 #\8 #\6)
   (#\4 #\4 #\4 #\8 #\8 #\6)
   (nil nil #\8 #\8 #\6)
   (nil #\8 #\8 #\6)
   (#\8 #\8 #\6)
   (nil #\6)
   (#\6)))

(deftest find-if-string.19
  (let ((a (make-array '(10) :initial-contents "123456789a"
                       :fill-pointer 5
                       :element-type 'character)))
    (values
     (find-if #'evendigitp a)
     (find-if #'evendigitp a :from-end t)
     (find-if #'odddigitp a)
     (find-if #'odddigitp a :from-end t)
     ))
  #\2 #\4 #\1 #\5)

(deftest find-if-string.20
  (do-special-strings
   (s "123a456" nil)
   (assert (eql (find-if #'alpha-char-p s) #\a)))
  nil)

;;; Keyword tests

(deftest find-if.allow-other-keys.1
  (find-if #'evenp '(1 2 3 4 5) :bad t :allow-other-keys t)
  2)

(deftest find-if.allow-other-keys.2
  (find-if #'evenp '(1 2 3 4 5) :allow-other-keys t :also-bad t)
  2)

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(deftest find-if.allow-other-keys.3
  (find-if #'evenp '(1 2 3 4 5)
            :allow-other-keys t
            :allow-other-keys nil
            :bad t)
  2)

(deftest find-if.keywords.4
  (find-if #'evenp '(1 2 3 4 5) :key #'identity :key #'1+)
  2)

(deftest find-if.allow-other-keys.5
  (find-if #'identity '(nil a b c nil) :allow-other-keys nil)
  a)


;;; Error tests

(deftest find-if.error.1
  (check-type-error #'(lambda (x) (find-if #'null x)) #'(lambda (x) (typep x 'sequence)))
  nil)

(deftest find-if.error.4
  (signals-error (find-if 'null '(a b c . d)) type-error)
  t)

(deftest find-if.error.5
  (signals-error (find-if) program-error)
  t)

(deftest find-if.error.6
  (signals-error (find-if #'null) program-error)
  t)

(deftest find-if.error.7
  (signals-error (find-if #'null nil :bad t) program-error)
  t)

(deftest find-if.error.8
  (signals-error (find-if #'null nil :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest find-if.error.9
  (signals-error (find-if #'null nil 1 1) program-error)
  t)

(deftest find-if.error.10
  (signals-error (find-if #'null nil :key) program-error)
  t)

(deftest find-if.error.11
  (signals-error (locally (find-if #'null 'b) t) type-error)
  t)

(deftest find-if.error.12
  (signals-error (find-if #'cons '(a b c)) program-error)
  t)

(deftest find-if.error.13
  (signals-error (find-if #'car '(a b c)) type-error)
  t)

(deftest find-if.error.14
  (signals-error (find-if #'identity '(a b c) :key #'cons) program-error)
  t)

(deftest find-if.error.15
  (signals-error (find-if #'identity '(a b c) :key #'car)
                 type-error)
  t)

;;; Order of evaluation tests

(deftest find-if.order.1
  (let ((i 0) x y)
    (values
     (find-if (progn (setf x (incf i)) #'identity)
              (progn (setf y (incf i)) '(nil nil nil a nil nil)))
     i x y))
  a 2 1 2)

(deftest find-if.order.2
  (let ((i 0) a b c d e f)
    (values
     (find-if (progn (setf a (incf i)) #'null)
              (progn (setf b (incf i)) '(nil nil nil a nil nil))
              :start (progn (setf c (incf i)) 1)
              :end   (progn (setf d (incf i)) 4)
              :from-end (setf e (incf i))
              :key   (progn (setf f (incf i)) #'null)
              )
     i a b c d e f))
  a 6 1 2 3 4 5 6)


(deftest find-if.order.3
  (let ((i 0) a b c d e f)
    (values
     (find-if (progn (setf a (incf i)) #'null)
              (progn (setf b (incf i)) '(nil nil nil a nil nil))
              :key   (progn (setf c (incf i)) #'null)
              :from-end (setf d (incf i))
              :end   (progn (setf e (incf i)) 4)
              :start (progn (setf f (incf i)) 1)
              )
     i a b c d e f))
  a 6 1 2 3 4 5 6)
