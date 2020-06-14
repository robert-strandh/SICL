;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Aug 28 20:53:24 2002
;;;; Contains: Tests for FIND-IF-NOT

(in-package #:sicl-sequence-test)

(deftest find-if-not-list.1
  (find-if-not #'identity ())
  nil)

(deftest find-if-not-list.2
  (find-if-not #'null '(a))
  a)

(deftest find-if-not-list.2a
  (find-if-not 'null '(a))
  a)

(deftest find-if-not-list.3
  (find-if-not #'oddp '(1 2 4 8 3 1 6 7))
  2)

(deftest find-if-not-list.4
  (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :from-end t)
  6)

(deftest find-if-not-list.5
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-not-list.6
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i :end nil))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-not-list.7
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-not-list.8
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-not-list.9
  (loop for i from 0 to 8 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :end i))
  (nil nil 2 2 2 2 2 2 2))

(deftest find-if-not-list.10
  (loop for i from 0 to 8 collect
        (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :end i :from-end t))
  (nil nil 2 4 8 8 8 6 6))

(deftest find-if-not-list.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start j :end i)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-not-list.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'oddp '(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-not-list.13
  (loop for i from 0 to 6
        collect
        (find-if-not #'oddp '(1 6 11 32 45 71 100) :key #'1+ :start i))
  (1 11 11 45 45 71 nil))

(deftest find-if-not-list.14
  (loop for i from 0 to 6
        collect
        (find-if-not #'oddp '(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))
  (71 71 71 71 71 71 nil))

(deftest find-if-not-list.15
  (loop for i from 0 to 7
        collect
        (find-if-not #'oddp '(1 6 11 32 45 71 100) :key #'1+ :end i))
  (nil 1 1 1 1 1 1 1))

(deftest find-if-not-list.16
  (loop for i from 0 to 7
        collect
        (find-if-not #'oddp '(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))
  (nil 1 1 11 11 45 71 71))

(deftest find-if-not-list.17
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'evenp '(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-not-list.18
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'evenp '(1 2 4 8 3 1 6 7) :start j :end i
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

(deftest find-if-not-vector.1
  (find-if-not #'identity #())
  nil)

(deftest find-if-not-vector.2
  (find-if-not #'not #(a))
  a)

(deftest find-if-not-vector.2a
  (find-if-not 'null #(a))
  a)

(deftest find-if-not-vector.3
  (find-if-not #'oddp #(1 2 4 8 3 1 6 7))
  2)

(deftest find-if-not-vector.4
  (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :from-end t)
  6)

(deftest find-if-not-vector.5
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-not-vector.6
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i :end nil))
  (2 2 4 8 6 6 6 nil))

(deftest find-if-not-vector.7
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-not-vector.8
  (loop for i from 0 to 7 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start i :end nil :from-end t))
  (6 6 6 6 6 6 6 nil))

(deftest find-if-not-vector.9
  (loop for i from 0 to 8 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :end i))
  (nil nil 2 2 2 2 2 2 2))

(deftest find-if-not-vector.10
  (loop for i from 0 to 8 collect
        (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :end i :from-end t))
  (nil nil 2 4 8 8 8 6 6))

(deftest find-if-not-vector.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start j :end i)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-not-vector.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'oddp #(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-not-vector.13
  (loop for i from 0 to 6
        collect
        (find-if-not #'oddp #(1 6 11 32 45 71 100) :key #'1+ :start i))
  (1 11 11 45 45 71 nil))

(deftest find-if-not-vector.14
  (loop for i from 0 to 6
        collect
        (find-if-not #'oddp #(1 6 11 32 45 71 100) :key '1+ :start i :from-end t))
  (71 71 71 71 71 71 nil))

(deftest find-if-not-vector.15
  (loop for i from 0 to 7
        collect
        (find-if-not #'oddp #(1 6 11 32 45 71 100) :key #'1+ :end i))
  (nil 1 1 1 1 1 1 1))

(deftest find-if-not-vector.16
  (loop for i from 0 to 7
        collect
        (find-if-not #'oddp #(1 6 11 32 45 71 100) :key '1+ :end i :from-end t))
  (nil 1 1 11 11 45 71 71))

(deftest find-if-not-vector.17
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'evenp #(1 2 4 8 3 1 6 7) :start j :end i :key #'1-)))
  ((nil 2 2 2 2 2 2 2)
   (2 2 2 2 2 2 2)
   (4 4 4 4 4 4)
   (8 8 8 8 8)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

(deftest find-if-not-vector.18
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'evenp #(1 2 4 8 3 1 6 7) :start j :end i
                       :from-end t :key #'1+)))
  ((nil 2 4 8 8 8 6 6)
   (2 4 8 8 8 6 6)
   (4 8 8 8 6 6)
   (8 8 8 6 6)
   (nil nil 6 6)
   (nil 6 6)
   (6 6)
   (nil)))

;;; Tests for bit vectors

(deftest find-if-not-bit-vector.1
  (find-if-not #'identity #*)
  nil)

(deftest find-if-not-bit-vector.2
  (find-if-not #'null #*1)
  1)

(deftest find-if-not-bit-vector.3
  (find-if-not #'not #*0)
  0)

(deftest find-if-not-bit-vector.4
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if-not #'oddp #*0110110 :start i :end j)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

(deftest find-if-not-bit-vector.5
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if-not #'oddp #*0110110 :start i :end j
                                       :from-end t)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

(deftest find-if-not-bit-vector.6
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if-not #'evenp #*0110110 :start i :end j
                                       :from-end t :key #'1+)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

(deftest find-if-not-bit-vector.7
  (loop for i from 0 to 6
        collect (loop for j from i to 7
                      collect (find-if-not #'evenp #*0110110 :start i :end j
                                       :key '1-)))
  ((nil 0 0 0 0 0 0 0)
   (nil nil nil 0 0 0 0)
   (nil nil 0 0 0 0)
   (nil 0 0 0 0)
   (nil nil nil 0)
   (nil nil 0)
   (nil 0)))

;;; Tests for strings

(deftest find-if-not-string.1
  (find-if-not #'identity "")
  nil)

(deftest find-if-not-string.2
  (find-if-not #'null "a")
  #\a)

(deftest find-if-not-string.2a
  (find-if-not 'null "a")
  #\a)

(deftest find-if-not-string.3
  (find-if-not #'odddigitp "12483167")
  #\2)

(deftest find-if-not-string.3a
  (find-if-not #'oddp "12483167" :key #'(lambda (c) (read-from-string (string c))))
  #\2)

(deftest find-if-not-string.4
  (find-if-not #'odddigitp "12483167" :from-end t)
  #\6)

(deftest find-if-not-string.5
  (loop for i from 0 to 7 collect
        (find-if-not #'odddigitp "12483167" :start i))
  (#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil))

(deftest find-if-not-string.6
  (loop for i from 0 to 7 collect
        (find-if-not #'odddigitp "12483167" :start i :end nil))
  (#\2 #\2 #\4 #\8 #\6 #\6 #\6 nil))

(deftest find-if-not-string.7
  (loop for i from 0 to 7 collect
        (find-if-not #'odddigitp "12483167" :start i :from-end t))
  (#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil))

(deftest find-if-not-string.8
  (loop for i from 0 to 7 collect
        (find-if-not #'odddigitp "12483167" :start i :end nil :from-end t))
  (#\6 #\6 #\6 #\6 #\6 #\6 #\6 nil))

(deftest find-if-not-string.9
  (loop for i from 0 to 8 collect
        (find-if-not #'odddigitp "12483167" :end i))
  (nil nil #\2 #\2 #\2 #\2 #\2 #\2 #\2))

(deftest find-if-not-string.10
  (loop for i from 0 to 8 collect
        (find-if-not #'odddigitp "12483167" :end i :from-end t))
  (nil nil #\2 #\4 #\8 #\8 #\8 #\6 #\6))

(deftest find-if-not-string.11
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'odddigitp "12483167" :start j :end i)))
  ((nil #\2 #\2 #\2 #\2 #\2 #\2 #\2)
   (#\2 #\2 #\2 #\2 #\2 #\2 #\2)
   (#\4 #\4 #\4 #\4 #\4 #\4)
   (#\8 #\8 #\8 #\8 #\8)
   (nil nil #\6 #\6)
   (nil #\6 #\6)
   (#\6 #\6)
   (nil)))

(deftest find-if-not-string.12
  (loop for j from 0 to 7
        collect
        (loop for i from (1+ j) to 8 collect
              (find-if-not #'odddigitp "12483167" :start j :end i
                       :from-end t)))
  ((nil #\2 #\4 #\8 #\8 #\8 #\6 #\6)
   (#\2 #\4 #\8 #\8 #\8 #\6 #\6)
   (#\4 #\8 #\8 #\8 #\6 #\6)
   (#\8 #\8 #\8 #\6 #\6)
   (nil nil #\6 #\6)
   (nil #\6 #\6)
   (#\6 #\6)
   (nil)))

(deftest find-if-not-string.13
  (loop for i from 0 to 6
        collect
        (find-if-not #'oddp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start i))
  (#\4 #\4 #\8 #\8 #\8 #\6 #\6))

(deftest find-if-not-string.14
  (loop for i from 0 to 6
        collect
        (find-if-not #'oddp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start i :from-end t))
  (#\6 #\6 #\6 #\6 #\6 #\6 #\6))

(deftest find-if-not-string.15
  (loop for i from 0 to 7
        collect
        (find-if-not #'oddp "1473816"
                 :key (compose #'read-from-string #'string)
                 :end i))
  (nil nil #\4 #\4 #\4 #\4 #\4 #\4))

(deftest find-if-not-string.16
  (loop for i from 0 to 7
        collect
        (find-if-not #'oddp "1473816"
                 :key (compose #'read-from-string #'string)
                 :end i :from-end t))
  (nil nil #\4 #\4 #\4 #\8 #\8 #\6))

(deftest find-if-not-string.17
  (loop for j from 0 to 6
        collect
        (loop for i from (1+ j) to 7 collect
              (find-if-not #'oddp "1473816"
                 :key (compose #'read-from-string #'string)
                 :start j :end i)))
  ((nil #\4 #\4 #\4 #\4 #\4 #\4)
   (#\4 #\4 #\4 #\4 #\4 #\4)
   (nil nil #\8 #\8 #\8)
   (nil #\8 #\8 #\8)
   (#\8 #\8 #\8)
   (nil #\6)
   (#\6)))

(deftest find-if-not-string.18
  (loop for j from 0 to 6
        collect
        (loop for i from (1+ j) to 7 collect
              (find-if-not #'oddp "1473816"
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

(deftest find-if-not-string.19
  (do-special-strings
   (s "abc1def" nil)
   (assert (eql (find-if-not #'alpha-char-p s) #\1)))
  nil)

;;; Keyword tests

(deftest find-if-not.allow-other-keys.1
  (find-if-not #'oddp '(1 2 3 4 5) :bad t :allow-other-keys t)
  2)

(deftest find-if-not.allow-other-keys.2
  (find-if-not #'oddp '(1 2 3 4 5) :allow-other-keys t :also-bad t)
  2)

;;; The leftmost of two :allow-other-keys arguments is the one that  matters.
(deftest find-if-not.allow-other-keys.3
  (find-if-not #'oddp '(1 2 3 4 5)
            :allow-other-keys t
            :allow-other-keys nil
            :bad t)
  2)

(deftest find-if-not.keywords.4
  (find-if-not #'oddp '(1 2 3 4 5) :key #'identity :key #'1+)
  2)

(deftest find-if-not.allow-other-keys.5
  (find-if-not #'null '(nil a b c nil) :allow-other-keys nil)
  a)

;;; Error tests

(deftest find-if-not.error.1
  (check-type-error #'(lambda (x) (find-if-not #'null x)) #'(lambda (x) (typep x 'sequence)))
  nil)

(deftest find-if-not.error.4
  (signals-error (find-if-not 'identity '(a b c . d))
                 type-error)
  t)

(deftest find-if-not.error.5
  (signals-error (find-if-not) program-error)
  t)

(deftest find-if-not.error.6
  (signals-error (find-if-not #'null) program-error)
  t)

(deftest find-if-not.error.7
  (signals-error (find-if-not #'null nil :bad t) program-error)
  t)

(deftest find-if-not.error.8
  (signals-error (find-if-not #'null nil :bad t :allow-other-keys nil)
                 program-error)
  t)

(deftest find-if-not.error.9
  (signals-error (find-if-not #'null nil 1 1) program-error)
  t)

(deftest find-if-not.error.10
  (signals-error (find-if-not #'null nil :key) program-error)
  t)

(deftest find-if-not.error.11
  (signals-error (locally (find-if-not #'null 'b) t) type-error)
  t)

(deftest find-if-not.error.12
  (signals-error (find-if-not #'cons '(a b c)) program-error)
  t)

(deftest find-if-not.error.13
  (signals-error (find-if-not #'car '(a b c)) type-error)
  t)

(deftest find-if-not.error.14
  (signals-error (find-if-not #'identity '(a b c) :key #'cons)
                 program-error)
  t)

(deftest find-if-not.error.15
  (signals-error (find-if-not #'identity '(a b c) :key #'car)
                 type-error)
  t)

;;; Order of evaluation tests

(deftest find-if-not.order.1
  (let ((i 0) x y)
    (values
     (find-if-not (progn (setf x (incf i)) #'null)
                  (progn (setf y (incf i)) '(nil nil nil a nil nil)))
     i x y))
  a 2 1 2)

(deftest find-if-not.order.2
  (let ((i 0) a b c d e f)
    (values
     (find-if-not (progn (setf a (incf i)) #'identity)
                  (progn (setf b (incf i)) '(nil nil nil a nil nil))
                  :start (progn (setf c (incf i)) 1)
                  :end   (progn (setf d (incf i)) 4)
                  :from-end (setf e (incf i))
                  :key   (progn (setf f (incf i)) #'null)
                  )
     i a b c d e f))
  a 6 1 2 3 4 5 6)


(deftest find-if-not.order.3
  (let ((i 0) a b c d e f)
    (values
     (find-if-not (progn (setf a (incf i)) #'identity)
                  (progn (setf b (incf i)) '(nil nil nil a nil nil))
                  :key   (progn (setf c (incf i)) #'null)
                  :from-end (setf d (incf i))
                  :end   (progn (setf e (incf i)) 4)
                  :start (progn (setf f (incf i)) 1)
                  )
     i a b c d e f))
  a 6 1 2 3 4 5 6)
