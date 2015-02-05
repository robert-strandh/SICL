;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 17 08:47:43 2002
;;;; Contains: Tests for ALWAYS, NEVER, THEREIS

(cl:in-package :sicl-loop-test)

;;; Tests of ALWAYS clauses

(deftest loop.12.1
  (loop for i in '(1 2 3 4) always (< i 10))
  t)

(deftest loop.12.2
  (loop for i in nil always nil)
  t)

(deftest loop.12.3
  (loop for i in '(a) always nil)
  nil)

(deftest loop.12.4
  (loop for i in '(1 2 3 4 5 6 7)
        always t
        until (> i 5))
  t)

(deftest loop.12.5
  (loop for i in '(1 2 3 4 5 6 7)
        always (< i 6)
        until (>= i 5))
  t)

(deftest loop.12.6
  (loop for x in '(a b c d e) always x)
  t)

(deftest loop.12.7
  (loop for x in '(1 2 3 4 5 6)
        always (< x 20)
        never (> x 10))
  t)

(deftest loop.12.8
  (loop for x in '(1 2 3 4 5 6)
        always (< x 20)
        never (> x 5))
  nil)

(deftest loop.12.9
  (loop for x in '(1 2 3 4 5 6)
        never (> x 5)
        always (< x 20))
  nil)

(deftest loop.12.10
  (loop for x in '(1 2 3 4 5)
        always (< x 10)
        finally (return 'good))
  good)

(deftest loop.12.11
  (loop for x in '(1 2 3 4 5)
        always (< x 3)
        finally (return 'bad))
  nil)

(deftest loop.12.12
  (loop for x in '(1 2 3 4 5 6)
        always t
        when (= x 4) do (loop-finish))
  t)

(deftest loop.12.13
  (loop for x in '(1 2 3 4 5 6)
        do (loop-finish)
        always nil)
  t)

;;; Tests of NEVER

(deftest loop.12.21
  (loop for i in '(1 2 3 4) never (> i 10))
  t)

(deftest loop.12.22
  (loop for i in nil never t)
  t)

(deftest loop.12.23
  (loop for i in '(a) never t)
  nil)

(deftest loop.12.24
  (loop for i in '(1 2 3 4 5 6 7)
        never nil
        until (> i 5))
  t)

(deftest loop.12.25
  (loop for i in '(1 2 3 4 5 6 7)
        never (>= i 6)
        until (>= i 5))
  t)

(deftest loop.12.26
  (loop for x in '(a b c d e) never (not x))
  t)

(deftest loop.12.30
  (loop for x in '(1 2 3 4 5)
        never (>= x 10)
        finally (return 'good))
  good)

(deftest loop.12.31
  (loop for x in '(1 2 3 4 5)
        never (>= x 3)
        finally (return 'bad))
  nil)

(deftest loop.12.32
  (loop for x in '(1 2 3 4 5 6)
        never nil
        when (= x 4) do (loop-finish))
  t)

(deftest loop.12.33
  (loop for x in '(1 2 3 4 5 6)
        do (loop-finish)
        never t)
  t)

;;; Tests of THEREIS

(deftest loop.12.41
  (loop for x in '(1 2 3 4 5)
        thereis (and (eql x 3) 'good))
  good)

(deftest loop.12.42
  (loop for x in '(nil nil a nil nil)
        thereis x)
  a)

(deftest loop.12.43
  (loop for x in '(1 2 3 4 5)
        thereis (eql x 4)
        when (eql x 2) do (loop-finish))
  nil)

;;; Error cases

(deftest loop.12.error.50
  (signals-error
   (loop for i from 1 to 10
         collect i
         always (< i 20))
   program-error)
  t)

(deftest loop.12.error.50a
  (signals-error
   (loop for i from 1 to 10
         always (< i 20)
         collect i)
   program-error)
  t)

(deftest loop.12.error.51
  (signals-error
   (loop for i from 1 to 10
         collect i
         never (> i 20))
   program-error)
  t)

(deftest loop.12.error.51a
  (signals-error
   (loop for i from 1 to 10
         never (> i 20)
         collect i)
   program-error)
  t)

(deftest loop.12.error.52
  (signals-error
   (loop for i from 1 to 10
         collect i
         thereis (> i 20))
   program-error)
  t)

(deftest loop.12.error.52a
  (signals-error
   (loop for i from 1 to 10
         thereis (> i 20)
         collect i)
   program-error)
  t)

;;; Non-error cases

(deftest loop.12.53
  (loop for i from 1 to 10
        collect i into foo
        always (< i 20))
  t)

(deftest loop.12.53a
  (loop for i from 1 to 10
        always (< i 20)
        collect i into foo)
  t)

(deftest loop.12.54
  (loop for i from 1 to 10
        collect i into foo
        never (> i 20))
  t)

(deftest loop.12.54a
  (loop for i from 1 to 10
        never (> i 20)
        collect i into foo)
  t)

(deftest loop.12.55
  (loop for i from 1 to 10
        collect i into foo
        thereis i)
  1)

(deftest loop.12.55a
  (loop for i from 1 to 10
        thereis i
        collect i into foo)
  1)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.12.56
  (macrolet
   ((%m (z) z))
   (loop for i in '(1 2 3 4) always (expand-in-current-env (%m (< i 10)))))
  t)

(deftest loop.12.57
  (macrolet
   ((%m (z) z))
   (loop for i in '(1 2 3 4) always (expand-in-current-env (%m t))))
  t)

(deftest loop.12.58
  (macrolet
   ((%m (z) z))
   (loop for i in '(1 2 3 4) never (expand-in-current-env (%m (>= i 10)))))
  t)

(deftest loop.12.59
  (macrolet
   ((%m (z) z))
   (loop for i in '(1 2 3 4) never (expand-in-current-env (%m t))))
  nil)

(deftest loop.12.60
  (macrolet
   ((%m (z) z))
   (loop for i in '(1 2 3 4)
         thereis (expand-in-current-env (%m (and (>= i 2) (+ i 1))))))
  3)
