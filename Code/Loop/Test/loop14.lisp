;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Wed Nov 20 06:33:21 2002
;;;; Contains: Tests of LOOP conditional execution clauses

(cl:in-package :sicl-loop-test)

(deftest loop.14.1
  (loop for x from 1 to 6
        when (evenp x)
        collect x)
  (2 4 6))

(deftest loop.14.2
  (loop for x from 1 to 6
        unless (evenp x)
        collect x)
  (1 3 5))

(deftest loop.14.3
  (loop for x from 1 to 10
        when (evenp x)
          collect x into foo
          and count t into bar
        finally (return (values foo bar)))
  (2 4 6 8 10)
  5)

(deftest loop.14.4
  (loop for x from 1 to 10
        when (evenp x) collect x end)
  (2 4 6 8 10))

(deftest loop.14.5
  (loop for x from 1 to 10
        when (evenp x) collect x into evens
        else collect x into odds
        end
        finally (return (values evens odds)))
  (2 4 6 8 10)
  (1 3 5 7 9))

(deftest loop.14.6
  (loop for x from 1 to 10
        unless (oddp x)
          collect x into foo
          and count t into bar
        finally (return (values foo bar)))
  (2 4 6 8 10)
  5)

(deftest loop.14.7
  (loop for x from 1 to 10
        unless (oddp x) collect x end)
  (2 4 6 8 10))

(deftest loop.14.8
  (loop for x from 1 to 10
        unless (oddp x) collect x into evens
        else collect x into odds
        end
        finally (return (values evens odds)))
  (2 4 6 8 10)
  (1 3 5 7 9))

(deftest loop.14.9
  (loop for x from 1 to 6
        if (evenp x)
        collect x)
  (2 4 6))

(deftest loop.14.10
  (loop for x from 1 to 10
        if (evenp x)
          collect x into foo
          and count t into bar
        finally (return (values foo bar)))
  (2 4 6 8 10)
  5)

(deftest loop.14.11
  (loop for x from 1 to 10
        if (evenp x) collect x end)
  (2 4 6 8 10))

(deftest loop.14.12
  (loop for x from 1 to 10
        if (evenp x) collect x into evens
        else collect x into odds
        end
        finally (return (values evens odds)))
  (2 4 6 8 10)
  (1 3 5 7 9))

;;; Test that else associates with the nearest conditional unclosed
;;; by end

(deftest loop.14.13
  (loop for i from 1 to 20
        if (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
            else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (2 4 8 10 14 16 20))

(deftest loop.14.14
  (loop for i from 1 to 20
        when (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
            else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (2 4 8 10 14 16 20))

(deftest loop.14.15
  (loop for i from 1 to 20
        if (evenp i)
          when (= (mod i 3) 0)
            collect i into list1
            else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (2 4 8 10 14 16 20))

(deftest loop.14.16
  (loop for i from 1 to 20
        if (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(deftest loop.14.17
  (loop for i from 1 to 20
        when (evenp i)
          if (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(deftest loop.14.18
  (loop for i from 1 to 20
        if (evenp i)
          when (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(deftest loop.14.19
  (loop for i from 1 to 20
        when (evenp i)
          when (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(deftest loop.14.20
  (loop for i from 1 to 20
        unless (oddp i)
          if (= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(deftest loop.14.21
  (loop for i from 1 to 20
        if (evenp i)
          unless (/= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

(deftest loop.14.22
  (loop for i from 1 to 20
        unless (oddp i)
          unless (/= (mod i 3) 0)
            collect i into list1
          end
          else collect i into list2
        finally (return (values list1 list2)))
  (6 12 18)
  (1 3 5 7 9 11 13 15 17 19))

;;; More tests conditionals

(deftest loop.14.23
  (loop for i from 1 to 20
        if (evenp i)
          collect i into list1
        else if (= (mod i 3) 0)
          collect i into list2
        else collect i into list3
        finally (return (values list1 list2 list3)))
  (2 4 6 8 10 12 14 16 18 20)
  (3 9 15)
  (1 5 7 11 13 17 19))

;;; Tests of 'IT'

(deftest loop.14.24
  (loop for x in '((a) nil (b) (c) (nil) (d))
        when (car x) collect it)
  (a b c d))

(deftest loop.14.25
  (loop for x in '((a) nil (b) (c) (nil) (d))
        if (car x) collect it)
  (a b c d))

(deftest loop.14.26
  (loop for x in '(nil (a) nil (b) (c) (nil) (d))
        when (car x) return it)
  a)

(deftest loop.14.27
  (loop for x in '(nil (a) nil (b) (c) (nil) (d))
        if (car x) return it)
  a)

(deftest loop.14.28
  (loop for x in '((a) nil (b) (c) (nil) (d))
        when (car x) collect it and collect 'foo)
  (a foo b foo c foo d foo))

;; (deftest loop.14.29
;;   (let ((it 'z))
;;     (loop for x in '(a b c d)
;;           when x collect it and collect it))
;;   (a z b z c z d z))

;; (deftest loop.14.30
;;   (let ((it 'z))
;;     (loop for x in '(a b c d)
;;           if x collect it end
;;           collect it))
;;   (a z b z c z d z))

(deftest loop.14.31
  (loop for it on '(a b c d)
        when (car it) collect it)
  (a b c d))

(deftest loop.14.32
  (loop for x in '(a b nil c d nil e)
        when x collecting it)
  (a b c d e))

(deftest loop.14.33
  (loop for x in '(a b nil c d nil e)
        when x append (list x))
  (a b c d e))

(deftest loop.14.34
  (loop for x in '(a b nil c d nil e)
        when x appending (list x))
  (a b c d e))

(deftest loop.14.35
  (loop for x in '(a b nil c d nil e)
        when x nconc (list x))
  (a b c d e))

(deftest loop.14.36
  (loop for x in '(a b nil c d nil e)
        when x nconcing (list x))
  (a b c d e))

(deftest loop.14.37
  (loop for it on '(a b c d)
        when (car it) collect it into foo
        finally (return foo))
  (a b c d))

(deftest loop.14.38
  (loop for x in '(1 2 nil 3 4 nil 5 nil)
        when x count it)
  5)

(deftest loop.14.39
  (loop for x in '(1 2 nil 3 4 nil 5 nil)
        when x counting it)
  5)

(deftest loop.14.40
  (loop for x in '(1 2 nil 3 4 nil 6 nil)
        when x maximize it)
  6)

(deftest loop.14.41
  (loop for x in '(1 2 nil 3 4 nil 6 nil)
        when x maximizing it)
  6)

(deftest loop.14.42
  (loop for x in '(1 2 nil 3 4 nil 6 nil)
        when x minimize it)
  1)

(deftest loop.14.43
  (loop for x in '(1 2 nil 3 4 nil 6 nil)
        when x minimizing it)
  1)

(deftest loop.14.44
  (loop for x in '(1 2 nil 3 4 nil 6 nil)
        when x sum it)
  16)

(deftest loop.14.45
  (loop for x in '(1 2 nil 3 4 nil 6 nil)
        when x summing it)
  16)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.14.46
  (macrolet
   ((%m (z) z))
   (loop for x from 1 to 6
         when (expand-in-current-env (%m (evenp x)))
         collect x))
  (2 4 6))

(deftest loop.14.47
  (macrolet
   ((%m (z) z))
   (loop for x from 1 to 6
         unless (expand-in-current-env (%m (evenp x)))
         collect x))
  (1 3 5))

(deftest loop.14.48
  (macrolet
   ((%m (z) z))
   (loop for x from 1 to 6
         when (expand-in-current-env (%m t))
         sum x))
  21)

(deftest loop.14.49
  (macrolet
   ((%m (z) z))
   (loop for x from 1 to 10
         if  (expand-in-current-env (%m (evenp x)))
         collect x end))
  (2 4 6 8 10))
