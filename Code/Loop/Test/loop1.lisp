;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Fri Oct 25 19:07:19 2002
;;;; Contains: Tests of extended loop, part 1

(in-package :sicl-loop-test)

;;; Tests of variable initialization and stepping clauses

;;; for-as-arithmetic

(deftest loop.1.1
  (loop for x from 1 to 10 collect x)
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.1.2
  (loop for x from 6 downto 1 collect x)
  (6 5 4 3 2 1))

(deftest loop.1.3
  (loop for x from 1 to 1 collect x)
  (1))

(deftest loop.1.4
  (loop for x from 1 to 0 collect x)
  nil)

(deftest loop.1.5
  (loop for x to 5 collect x)
  (0 1 2 3 4 5))

(deftest loop.1.6
  (loop for x downfrom 5 to 0 collect x)
  (5 4 3 2 1 0))

(deftest loop.1.7
  (loop for x upfrom 1 to 5 collect x)
  (1 2 3 4 5))

(deftest loop.1.8
  (loop for x from 1.0 to 5.0 count x)
  5)

(deftest loop.1.9
  (loop for x from 1 to 9 by 2 collect x)
  (1 3 5 7 9))

(deftest loop.1.10
  (loop for x from 1 to 10 by 2 collect x)
  (1 3 5 7 9))

(deftest loop.1.11
  (loop for x to 10 from 1 collect x)
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.1.12
  (loop for x to 10 by 2 from 1 collect x)
  (1 3 5 7 9))

(deftest loop.1.13
  (loop for x by 2 to 10 from 1 collect x)
  (1 3 5 7 9))

(deftest loop.1.14
  (loop for x by 2 to 10 collect x)
  (0 2 4 6 8 10))

(deftest loop.1.15
  (loop for x to 10 by 2 collect x)
  (0 2 4 6 8 10))

(deftest loop.1.16
  (let ((n 0))
    (loop for x from (incf n) to (+ n 5) collect x))
  (1 2 3 4 5 6))

(deftest loop.1.17
  (let ((n 0))
    (loop for x to (+ n 5) from (incf n) collect x))
  (1 2 3 4 5))

(deftest loop.1.18
  (let ((n 0))
    (loop for x from (incf n) to (+ n 9) by (incf n) collect x))
  (1 3 5 7 9))

(deftest loop.1.19
  (let ((n 0))
    (loop for x from (incf n) by (incf n) to (+ n 9) collect x))
  (1 3 5 7 9 11))

(deftest loop.1.20
  (let ((a 0) (b 5) (c 1))
    (loop for x from a to b by c
          collect (progn (incf a) (incf b 2) (incf c 3) x)))
  (0 1 2 3 4 5))

(deftest loop.1.21
  (loop for x from 0 to 5 by 1/2 collect x)
  (0 1/2 1 3/2 2 5/2 3 7/2 4 9/2 5))

(deftest loop.1.22
  (loop for x from 1 below 5 collect x)
  (1 2 3 4))

(deftest loop.1.23
  (loop for x from 1 below 5.01 collect x)
  (1 2 3 4 5))

(deftest loop.1.24
  (loop for x below 5 from 2 collect x)
  (2 3 4))

(deftest loop.1.25
  (loop for x from 10 above 4 collect x)
  (10 9 8 7 6 5))

(deftest loop.1.26
  (loop for x from 14 above 6 by 2 collect x)
  (14 12 10 8))

(deftest loop.1.27
  (loop for x above 6 from 14 by 2 collect x)
  (14 12 10 8))

(deftest loop.1.28
  (loop for x downfrom 16 above 7 by 3 collect x)
  (16 13 10))

(deftest loop.1.29
  (let (a b c (i 0))
    (values
     (loop for x from (progn (setq a (incf i)) 0)
           below (progn (setq b (incf i)) 9)
           by (progn (setq c (incf i)) 2)
           collect x)
     a b c i))
  (0 2 4 6 8)
  1 2 3 3)

(deftest loop.1.30
  (let (a b c (i 0))
    (values
     (loop for x from (progn (setq a (incf i)) 0)
           by (progn (setq c (incf i)) 2)
           below (progn (setq b (incf i)) 9)
           collect x)
     a b c i))
  (0 2 4 6 8)
  1 3 2 3)

(deftest loop.1.31
  (let (a b c (i 0))
    (values
     (loop for x
           below (progn (setq b (incf i)) 9)
           by (progn (setq c (incf i)) 2)
           from (progn (setq a (incf i)) 0)
           collect x)
     a b c i))
  (0 2 4 6 8)
  3 1 2 3)

(deftest loop.1.32
  (let (a b c (i 0))
    (values
     (loop for x
           by (progn (setq c (incf i)) 2)
           below (progn (setq b (incf i)) 9)
           from (progn (setq a (incf i)) 0)
           collect x)
     a b c i))
  (0 2 4 6 8)
  3 2 1 3)

(deftest loop.1.33
  (loop for x from 1 upto 5 collect x)
  (1 2 3 4 5))

(deftest loop.1.34
  (loop for x from 1 to 4.0 collect x)
  (1 2 3 4))

(deftest loop.1.35
  (loop for x below 5 collect x)
  (0 1 2 3 4))

(deftest loop.1.36
  (loop for x below 20 by 3 collect x)
  (0 3 6 9 12 15 18))

(deftest loop.1.37
  (loop for x by 3 below 20 collect x)
  (0 3 6 9 12 15 18))

(deftest loop.1.38
  (loop for x of-type fixnum from 1 to 5 collect x)
  (1 2 3 4 5))

;;; The following provides an example where an incorrect
;;; implementation will assign X an out-of-range value
;;; at the end.
(deftest loop.1.39
  (loop for x of-type (integer 1 5) from 1 to 5 collect x)
  (1 2 3 4 5))

;;; Test that the index variable achieves the inclusive
;;; upper bound, but does not exceed it.
(deftest loop.1.40
  (loop for x from 1 to 5 do (progn) finally (return x))
  5)

;;; Test that the index variable acheives the exclusive
;;; upper bound, but does not exceed it.
(deftest loop.1.41
  (loop for x from 1 below 5 do (progn) finally (return x))
  4)

(deftest loop.1.42
  (loop for x from 10 downto 0 do (progn) finally (return x))
  0)

(deftest loop.1.43
  (loop for x from 10 above 0 do (progn) finally (return x))
  1)

;;; The arithmetic loop form says the types are numbers, not
;;; reals, so arguably they should work on complexes (which are
;;; numbers.)  Comparing these for termination could be problematic,
;;; but a clause without termination should work just fine.

(deftest loop.1.44
  (loop for i from 1 to 5 for c from #c(0 1) collect c)
  (#c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(4 1)))

(deftest loop.1.45
  (loop for i from 1 to 5 for c from #c(0 1) by 2 collect c)
  (#c(0 1) #c(2 1) #c(4 1) #c(6 1) #c(8 1)))

(deftest loop.1.46
  (loop for i from 1 to 5 for c downfrom #c(5 1) collect c)
  (#c(5 1) #c(4 1) #c(3 1) #c(2 1) #c(1 1)))

(deftest loop.1.47
  (loop for i from 1 to 5 for c downfrom #c(10 1) by 2 collect c)
  (#c(10 1) #c(8 1) #c(6 1) #c(4 1) #c(2 1)))

(deftest loop.1.48
  (loop for i from 1 to 5 for c upfrom #c(0 1) collect c)
  (#c(0 1) #c(1 1) #c(2 1) #c(3 1) #c(4 1)))

(deftest loop.1.49
  (loop for i from 1 to 5 for c upfrom #c(0 1) by 2 collect c)
  (#c(0 1) #c(2 1) #c(4 1) #c(6 1) #c(8 1)))

;;; The variable in the loop for-as-arithmetic clause
;;; can be a d-var-spec, so 'NIL' should mean don't bind anything

(deftest loop.1.50
  (let ((i 0))
    (loop for nil from 10 to 15 collect (incf i)))
  (1 2 3 4 5 6))

(deftest loop.1.51
  (let ((i 0))
    (loop for nil from 10 below 15 collect (incf i)))
  (1 2 3 4 5))

(deftest loop.1.52
  (loop for nil from 10 to 0 collect 'a)
  nil)

(deftest loop.1.53
  (let ((i 0))
    (loop for nil from 0 to 10 by 2 collect (incf i)))
  (1 2 3 4 5 6))

(deftest loop.1.54
  (let ((i 0))
    (loop for nil from 1 to 4
          for nil from 1 to 10 collect (incf i)))
  (1 2 3 4))

(deftest loop.1.55
  (let ((i 0))
    (loop for nil from 5 downto 0 collect (incf i)))
  (1 2 3 4 5 6))

(deftest loop.1.56
  (let ((i 0))
    (loop for nil from 5 above 0 collect (incf i)))
  (1 2 3 4 5))

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.1.57
  (macrolet
   ((%m (z) z))
   (loop for i from (expand-in-current-env (%m 1)) to 5 collect i))
  (1 2 3 4 5))

(deftest loop.1.58
  (macrolet
   ((%m (z) z))
   (loop for i from 1 to (expand-in-current-env (%m 5)) collect i))
  (1 2 3 4 5))

(deftest loop.1.59
  (macrolet
   ((%m (z) z))
   (loop for i from 1 to 5 by (expand-in-current-env (%m 2)) collect i))
  (1 3 5))

(deftest loop.1.60
  (macrolet
   ((%m (z) z))
   (loop for i downfrom (expand-in-current-env (%m 10))
         to 3
         collect i))
  (10 9 8 7 6 5 4 3))

(deftest loop.1.61
  (macrolet
   ((%m (z) z))
   (loop for i downfrom 10
         to (expand-in-current-env (%m 3))
         collect i))
  (10 9 8 7 6 5 4 3))

;; (deftest loop.1.62
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for i from (expand-in-current-env (%m 10))
;;          downto 3
;;          collect i))
;;   (10 9 8 7 6 5 4 3))

;; (deftest loop.1.63
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for i from 10
;;          downto (expand-in-current-env (%m 3))
;;          collect i))
;;   (10 9 8 7 6 5 4 3))

;; (deftest loop.1.64
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for i from (expand-in-current-env (%m 1)) below 5 collect i))
;;   (1 2 3 4))

;; (deftest loop.1.65
;;   (macrolet
;;    ((%m (z) z))
;;    (loop for i from 1 below (expand-in-current-env (%m 5)) collect i))
;;   (1 2 3 4))
