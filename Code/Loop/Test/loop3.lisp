;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 27 08:36:36 2002
;;;; Contains: Tests of FOR-ON-AS-LIST iteration control in LOOP

(cl:in-package :sicl-loop-test)

(deftest loop.3.1
  (loop for x on '(1 2 3) sum (car x))
  6)

(deftest loop.3.2
  (loop for x on '(1 2 3 4)
        do (when (evenp (car x)) (return x)))
  (2 3 4))


(deftest loop.3.3
  (loop for x on '(a b c . d) collect (car x))
  (a b c))

(deftest loop.3.4
  (let ((x nil))
    (loop for e on '(a b c d) do (push (car e) x))
    x)
  (d c b a))

(deftest loop.3.5
  (loop for e on '(a b c d e f) by #'cddr
        collect (car e))
  (a c e))

(deftest loop.3.6
  (loop for e on '(a b c d e f g) by #'cddr
        collect (car e))
  (a c e g))

(deftest loop.3.7
  (loop for e on '(a b c d e f)
        by #'(lambda (l) (and (cdr l) (cons (car l) (cddr l))))
        collect (car e))
  (a a a a a a))

(deftest loop.3.8
  (loop for ((x . y)) on '((a . b) (c . d) (e . f))
        collect (list x y))
  ((a b) (c d) (e f)))

(deftest loop.3.9
  (loop for ((x nil y)) on '((a b c) (d e f) (g h i))
        collect (list x y))
  ((a c) (d f) (g i)))

(deftest loop.3.10
  (loop for ((x y)) of-type (fixnum) on '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(deftest loop.3.11
  (loop for ((x y)) of-type (fixnum) on '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(deftest loop.3.12
  (loop for ((x y)) of-type ((fixnum fixnum)) on '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(deftest loop.3.13
  (loop for ((x . y)) of-type ((fixnum . fixnum)) on '((1 . 2) (3 . 4) (5 . 6))
        collect (+ x y))
  (3 7 11))

(deftest loop.3.14
  (signals-error
   (loop for x on '(a b c)
         for x on '(d e f) collect x)
   program-error)
  t)

(deftest loop.3.15
  (signals-error (loop for (x . x) on '((a b) (c d)) collect x)
                 program-error)
  t)

(deftest loop.3.16
  (loop for nil on nil do (return t))
  nil)

(deftest loop.3.17
  (let ((x '(a b c)))
    (values
     x
     (loop for x on '(d e f) collect x)
     x))
  (a b c)
  ((d e f) (e f) (f))
  (a b c))

(deftest loop.3.18
  (loop for (x) of-type ((integer 0 10)) on '(2 4 6 7) sum x)
  19)

;;; Tests of the 'AS' form

(deftest loop.3.19
  (loop as x on '(1 2 3) sum (car x))
  6)

(deftest loop.3.20
  (loop as x on '(a b c)
        as y on '(1 2 3)
        collect (list (car x) (car y)))
  ((a 1) (b 2) (c 3)))

(deftest loop.3.21
  (loop as x on '(a b c)
        for y on '(1 2 3)
        collect (list (car x) (car y)))
  ((a 1) (b 2) (c 3)))

(deftest loop.3.22
  (loop for x on '(a b c)
        as y on '(1 2 3)
        collect (list (car x) (car y)))
  ((a 1) (b 2) (c 3)))

(deftest loop.3.23
  (let (a b (i 0))
    (values
     (loop for e on (progn (setf a (incf i))
                           '(a b c d e f g))
           by (progn (setf b (incf i)) #'cddr)
           collect (car e))
     a b i))
  (a c e g)
  1 2 2)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

;; (deftest loop.3.24
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for x on (expand-in-current-env (%m '(1 2 3))) sum (car x)))
;;   6)

;; (deftest loop.3.25
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for e on (expand-in-current-env (%m '(a b c d e f))) by #'cddr
;;          collect (car e)))
;;   (a c e))

;; (deftest loop.3.26
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for e on '(a b c d e f)
;;          by (expand-in-current-env (%m #'cddr))
;;          collect (car e)))
;;   (a c e))

;; (deftest loop.3.27
;;   (macrolet
;;    ((%m (z) z))
;;    (loop as x on (expand-in-current-env (%m '(1 2 3))) sum (car x)))
;;   6)
