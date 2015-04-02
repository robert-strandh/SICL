;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Oct 26 13:45:45 2002
;;;; Contains: Tests of the FOR-AS-IN-LIST loop iteration control form,
;;;;      and of destructuring in loop forms

(cl:in-package :sicl-loop-test)

(deftest loop.2.1
  (loop for x in '(1 2 3) sum x)
  6)

(deftest loop.2.2
  (loop for x in '(1 2 3 4)
        do (when (evenp x) (return x)))
  2)

(deftest loop.2.3
  (signals-error (loop for x in '(a . b) collect x)
                 type-error)
  t)

(deftest loop.2.4
  (let ((x nil))
    (loop for e in '(a b c d) do (push e x))
    x)
  (d c b a))

(deftest loop.2.5
  (loop for e in '(a b c d e f) by #'cddr
        collect e)
  (a c e))

(deftest loop.2.6
  (loop for e in '(a b c d e f g) by #'cddr
        collect e)
  (a c e g))

(deftest loop.2.7
  (loop for e in '(a b c d e f)
        by #'(lambda (l) (and (cdr l) (cons (car l) (cddr l))))
        collect e)
  (a a a a a a))

(deftest loop.2.8
  (loop for (x . y) in '((a . b) (c . d) (e . f))
        collect (list x y))
  ((a b) (c d) (e f)))

(deftest loop.2.9
  (loop for (x nil y) in '((a b c) (d e f) (g h i))
        collect (list x y))
  ((a c) (d f) (g i)))

(deftest loop.2.10
  (loop for (x y) of-type fixnum in '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

;; (deftest loop.2.11
;;   (loop for (x y) of-type fixnum in '((1 2) (3 4) (5 6))
;;         collect (+ x y))
;;   (3 7 11))

(deftest loop.2.12
  (loop for (x y) of-type (fixnum fixnum) in '((1 2) (3 4) (5 6))
        collect (+ x y))
  (3 7 11))

(deftest loop.2.13
  (loop for (x . y) of-type (fixnum . fixnum) in '((1 . 2) (3 . 4) (5 . 6))
        collect (+ x y))
  (3 7 11))

(deftest loop.2.14
  (signals-error
   (loop for x in '(a b c)
         for x in '(d e f) collect x)
   program-error)
  t)

(deftest loop.2.15
  (signals-error
   (loop for (x . x) in '((a b) (c d)) collect x)
   program-error)
  t)

(deftest loop.2.16
  (loop for nil in nil do (return t))
  nil)

(deftest loop.2.17
  (let ((x '(a b c)))
    (values
     x
     (loop for x in '(d e f) collect (list x))
     x))
  (a b c)
  ((d) (e) (f))
  (a b c))

(deftest loop.2.18
  (loop for x of-type (integer 0 10) in '(2 4 6 7) sum x)
  19)

;;; Tests of the 'AS' form

(deftest loop.2.19
  (loop as x in '(1 2 3) sum x)
  6)

(deftest loop.2.20
  (loop as x in '(a b c)
        as y in '(1 2 3)
        collect (list x y))
  ((a 1) (b 2) (c 3)))

(deftest loop.2.21
  (loop as x in '(a b c)
        for y in '(1 2 3)
        collect (list x y))
  ((a 1) (b 2) (c 3)))

(deftest loop.2.22
  (loop for x in '(a b c)
        as y in '(1 2 3)
        collect (list x y))
  ((a 1) (b 2) (c 3)))

(deftest loop.2.23
  (let (a b (i 0))
    (values
     (loop for e in (progn (setf a (incf i))
                           '(a b c d e f g))
           by (progn (setf b (incf i)) #'cddr)
           collect e)
     a b i))
  (a c e g)
  1 2 2)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

;; (deftest loop.2.24
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for x in (expand-in-current-env (%m '(1 2 3))) sum x))
;;   6)

;; (deftest loop.2.25
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for (x . y) in (expand-in-current-env (%m '((a . b) (c . d) (e . f))))
;;          collect (list x y)))
;;   ((a b) (c d) (e f)))

;; (deftest loop.2.26
;;   (macrolet
;;    ((%m (z) z))
;;    (loop as x in (expand-in-current-env (%m '(1 2 3))) sum x))
;;   6)
