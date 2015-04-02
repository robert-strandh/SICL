;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Oct 27 22:46:39 2002
;;;; Contains: Tests for LOOP FOR-AS-EQUAL-THEN

(cl:in-package :sicl-loop-test)

(deftest loop.4.1
  (loop
   for x = 1 then (1+ x)
   until (> x 5)
   collect x)
  (1 2 3 4 5))

(deftest loop.4.2
  (loop
   for i from 1 to 10
   for j = (1+ i) collect j)
  (2 3 4 5 6 7 8 9 10 11))

(deftest loop.4.3
  (loop
   for i from 1 to 10
   for j of-type integer = (1+ i) collect j)
  (2 3 4 5 6 7 8 9 10 11))

(deftest loop.4.4
  (loop for e on '(a b c d e)
        for (x . y) = e
        collect x)
  (a b c d e))

(deftest loop.4.5
  (loop for (x . y) = '(a b c d e) then y
        while x
        collect x)
  (a b c d e))

;;; Error cases

;; (deftest loop.4.6
;;   (signals-error
;;    (loop for (x . x) = '(nil nil nil)
;;          until x count t)
;;    program-error)
;;   t)

;; (deftest loop.4.7
;;   (signals-error
;;    (macroexpand '(loop for (x . x) = '(nil nil nil)
;;                        until x count t))
;;    program-error)
;;   t)

;; (deftest loop.4.8
;;   (signals-error
;;    (macroexpand '(loop for x = '(nil nil nil)
;;                        for x = 1 count x until t))
;;    program-error)
;;   t)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.4.9
  (macrolet
   ((%m (z) z))
   (loop
    for x = (expand-in-current-env (%m 1)) then (1+ x)
    until (> x 5)
    collect x))
  (1 2 3 4 5))

(deftest loop.4.10
  (macrolet
   ((%m (z) z))
   (loop
    for x = 1 then (expand-in-current-env (%m (1+ x)))
    until (> x 5)
    collect x))
  (1 2 3 4 5))

(deftest loop.4.11
  (macrolet
   ((%m (z) z))
   (loop
    for x = 1 then (1+ x)
    until (expand-in-current-env (%m (> x 5)))
    collect x))
  (1 2 3 4 5))

(deftest loop.4.12
  (macrolet
   ((%m (z) z))
   (loop
    for x = 1 then (1+ x)
    while (expand-in-current-env (%m (<= x 5)))
    collect x))
  (1 2 3 4 5))

(deftest loop.4.13
  (macrolet
   ((%m (z) z))
   (loop
    for x = 1 then (1+ x)
    until (> x 5)
    collect (expand-in-current-env (%m x))))
  (1 2 3 4 5))
