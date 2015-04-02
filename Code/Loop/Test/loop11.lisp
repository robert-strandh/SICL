;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 16 21:39:33 2002
;;;; Contains: Tests for loop termination clauses REPEAT, WHILE and UNTIL

(cl:in-package :sicl-loop-test)

;;; Tests of REPEAT

(deftest loop.11.1
  (let ((z 0))
    (values
     (loop repeat 10 do (incf z))
     z))
  nil
  10)

(deftest loop.11.2
  (loop repeat 10 collect 'a)
  (a a a a a a a a a a))

(deftest loop.11.3
  (let ((z 0))
    (loop repeat 0 do (incf z))
    z)
  0)

(deftest loop.11.4
  (let ((z 0))
    (loop repeat -1 do (incf z))
    z)
  0)

(deftest loop.11.5
  (let ((z 0))
    (loop repeat -1.5 do (incf z))
    z)
  0)

(deftest loop.11.6
  (let ((z 0))
    (loop repeat -1000000000000 do (incf z))
    z)
  0)

(deftest loop.11.7
  (let ((z 0))
    (loop repeat 10 do (incf z) (loop-finish))
    z)
  1)

;;; This test is wrong because REPEAT is a main clause whereas FOR is
;;; a variable clause, and no main clause can precede a variable
;;; clause.
;;;
;;; (deftest loop.11.8
;;;  (loop repeat 3 for i in '(a b c d e) collect i)
;;;  (a b c))

;; Enough implementors have complained about this test that
;; I'm removing it.  The standard is self-contradictory
;; on whether REPEAT can occur later in a LOOP form.

;; (deftest loop.11.9
;;  (loop for i in '(a b c d e) collect i repeat 3)
;;  (a b c))


;; Tests of WHILE

(deftest loop.11.10
  (loop with i = 0 while (< i 10) collect (incf i))
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.11.11
  (loop with i = 0 while (if (< i 10) t (return 'good))
        collect (incf i))
  good)

(deftest loop.11.12
  (loop with i = 0
        while (< i 10) collect (incf i)
        while (< i 10) collect (incf i)
        while (< i 10) collect (incf i))
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.11.13
  (loop with i = 0 while (< i 10) collect (incf i)
        finally (return 'done))
  done)

(deftest loop.11.14
  (loop for i in '(a b c)
        while nil
        collect i)
  nil)

(deftest loop.11.15
  (loop for i in '(a b c)
        collect i
        while nil)
  (a))

(deftest loop.11.16
  (loop for i in '(a b c)
        while t
        collect i)
  (a b c))

(deftest loop.11.17
  (loop for i in '(a b c)
        collect i
        while t)
  (a b c))

(deftest loop.11.18
  (loop for i from 1 to 10
        while (< i 6)
        finally (return i))
  6)

;; Tests of UNTIL

(deftest loop.11.20
  (loop with i = 0 until (>= i 10) collect (incf i))
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.11.21
  (loop with i = 0 while (if (< i 10) t (return 'good))
        collect (incf i))
  good)

(deftest loop.11.22
  (loop with i = 0
        until (>= i 10) collect (incf i)
        until (>= i 10) collect (incf i)
        until (>= i 10) collect (incf i))
  (1 2 3 4 5 6 7 8 9 10))

(deftest loop.11.23
  (loop with i = 0 until (>= i 10) collect (incf i)
        finally (return 'done))
  done)

(deftest loop.11.24
  (loop for i in '(a b c)
        until t
        collect i)
  nil)

(deftest loop.11.25
  (loop for i in '(a b c)
        collect i
        until t)
  (a))

(deftest loop.11.26
  (loop for i in '(a b c)
        until nil
        collect i)
  (a b c))

(deftest loop.11.27
  (loop for i in '(a b c)
        collect i
        until nil)
  (a b c))

(deftest loop.11.28
  (loop for i from 1 to 10
        until (>= i 6)
        finally (return i))
  6)

;; More tests of a bug that showed up in c.l.l

(deftest loop.11.29
  (loop for i in '(4 8 9 A 13)
        when (eq i 'a) return :good
        while (< i 12) collect i)
  :good)

(deftest loop.11.30
  (loop for i in '(4 8 9 A 13)
        unless (numberp i) return :good
        while (< i 12) collect i)
  :good)

(deftest loop.11.31
  (loop for i in '(4 8 9 A 13)
        when (eq i 'a) return :good
        until (> i 12) collect i)
  :good)

(deftest loop.11.32
  (loop for i in '(4 8 9 A 13)
        unless (numberp i) return :good
        until (> i 12) collect i)
  :good)

(deftest loop.11.33
  (loop for i in '(4 8 9 A 13)
        if (not (numberp i)) return :good end
        while (< i 12) collect i)
  :good)

(deftest loop.11.34
  (loop for i in '(4 8 9 A 13)
        if (not (numberp i)) return :good end
        until (> i 12) collect i)
  :good)

;; Test that explicit calls to macroexpand in subforms
;; are done in the correct environment

(deftest loop.11.35
  (macrolet
   ((%m (z) z))
   (loop repeat (expand-in-current-env (%m 5)) collect 'x))
  (x x x x x))
