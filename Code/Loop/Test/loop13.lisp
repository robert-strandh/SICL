;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Nov 17 12:37:45 2002
;;;; Contains: Tests of DO, DOING, RETURN in LOOP.  Tests of NAMED loops

(cl:in-package :sicl-loop-test)

(deftest loop.13.1
  (loop do (return 10))
  10)

(deftest loop.13.2
  (loop doing (return 10))
  10)

(deftest loop.13.3
  (loop for i from 0 below 100 by 7
        when (> i 50) return i)
  56)

(deftest loop.13.4
  (let ((x 0))
    (loop do
          (incf x)
          (when (= x 10) (return x))))
  10)

(deftest loop.13.5
  (loop return 'a)
  a)

(deftest loop.13.6
  (loop return (values)))

(deftest loop.13.7
  (loop return (values 1 2))
  1 2)

(deftest loop.13.8
  (let* ((limit (min 1000 (1- (min call-arguments-limit
                                   multiple-values-limit))))
         (vals (make-list limit :initial-element :a))
         (vals2 (multiple-value-list (eval `(loop return (values ,@vals))))))
    (equal vals vals2))
  t)

(deftest loop.13.9
  (loop named foo return 'a)
  a)

(deftest loop.13.10
  (block nil
    (return (loop named foo return :good))
    :bad)
  :good)

(deftest loop.13.11
  (block nil
    (loop named foo do (return :good))
    :bad)
  :good)

(deftest loop.13.12
  (loop named foo with a = (return-from foo :good) return :bad)
  :good)

(deftest loop.13.13
  (loop named foo
        with b = 1
        and a = (return-from foo :good) return :bad)
  :good)

(deftest loop.13.14
  (loop named foo
        for a = (return-from foo :good) return :bad)
  :good)

(deftest loop.13.15
  (loop named foo for a in (return-from foo :good))
  :good)

(deftest loop.13.16
  (loop named foo for a from (return-from foo :good) return :bad)
  :good)

(deftest loop.13.17
  (loop named foo for a on (return-from foo :good) return :bad)
  :good)

(deftest loop.13.18
  (loop named foo for a across (return-from foo :good) return :bad)
  :good)

(deftest loop.13.19
  (loop named foo for a being the hash-keys of (return-from foo :good)
        return :bad)
  :good)

(deftest loop.13.20
  (loop named foo for a being the symbols of (return-from foo :good)
        return :bad)
  :good)

(deftest loop.13.21
  (loop named foo repeat (return-from foo :good) return :bad)
  :good)

(deftest loop.13.22
  (loop named foo for i from 0 to (return-from foo :good) return :bad)
  :good)

(deftest loop.13.23
  (loop named foo for i from 0 to 10 by (return-from foo :good) return :bad)
  :good)

(deftest loop.13.24
  (loop named foo for i from 10 downto (return-from foo :good) return :bad)
  :good)

(deftest loop.13.25
  (loop named foo for i from 10 above (return-from foo :good) return :bad)
  :good)

(deftest loop.13.26
  (loop named foo for i from 10 below (return-from foo :good) return :bad)
  :good)

(deftest loop.13.27
  (loop named foo for i in '(a b c) by (return-from foo :good) return :bad)
  :good)

(deftest loop.13.28
  (loop named foo for i on '(a b c) by (return-from foo :good) return :bad)
  :good)

(deftest loop.13.29
  (loop named foo for i = 1 then (return-from foo :good))
  :good)

(deftest loop.13.30
  (loop named foo for x in '(a b c) collect (return-from foo :good))
  :good)

(deftest loop.13.31
  (loop named foo for x in '(a b c) append (return-from foo :good))
  :good)

(deftest loop.13.32
  (loop named foo for x in '(a b c) nconc (return-from foo :good))
  :good)

(deftest loop.13.33
  (loop named foo for x in '(a b c) count (return-from foo :good))
  :good)

(deftest loop.13.34
  (loop named foo for x in '(a b c) sum (return-from foo :good))
  :good)

(deftest loop.13.35
  (loop named foo for x in '(a b c) maximize (return-from foo :good))
  :good)

(deftest loop.13.36
  (loop named foo for x in '(a b c) minimize (return-from foo :good))
  :good)

(deftest loop.13.37
  (loop named foo for x in '(a b c) thereis (return-from foo :good))
  :good)

(deftest loop.13.38
  (loop named foo for x in '(a b c) always (return-from foo :good))
  :good)

(deftest loop.13.39
  (loop named foo for x in '(a b c) never (return-from foo :good))
  :good)

(deftest loop.13.40
  (loop named foo for x in '(a b c) until (return-from foo :good))
  :good)

(deftest loop.13.41
  (loop named foo for x in '(a b c) while (return-from foo :good))
  :good)

(deftest loop.13.42
  (loop named foo for x in '(a b c) when (return-from foo :good) return :bad)
  :good)

(deftest loop.13.43
  (loop named foo for x in '(a b c) unless (return-from foo :good) return :bad)
  :good)

(deftest loop.13.44
  (loop named foo for x in '(a b c) if (return-from foo :good) return :bad)
  :good)

(deftest loop.13.45
  (loop named foo for x in '(a b c) return (return-from foo :good))
  :good)

(deftest loop.13.46
  (loop named foo initially (return-from foo :good) return :bad)
  :good)

(deftest loop.13.47
  (loop named foo do (loop-finish) finally (return-from foo :good))
  :good)


(deftest loop.13.52
  (block nil
    (loop named foo with a = (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.53
  (block nil
    (loop named foo
          with b = 1
          and a = (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.54
  (block nil
    (loop named foo
          for a = (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.55
  (block nil
    (loop named foo for a in (return :good))
    :bad)
  :good)

(deftest loop.13.56
  (block nil
    (loop named foo for a from (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.57
  (block nil
    (loop named foo for a on (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.58
  (block nil
    (loop named foo for a across (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.59
  (block nil
    (loop named foo for a being the hash-keys of (return :good)
          return :bad)
    :bad)
  :good)

(deftest loop.13.60
  (block nil
    (loop named foo for a being the symbols of (return :good)
          return :bad)
    :bad)
  :good)

(deftest loop.13.61
  (block nil
    (loop named foo repeat (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.62
  (block nil
    (loop named foo for i from 0 to (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.63
  (block nil
    (loop named foo for i from 0 to 10 by (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.64
  (block nil
    (loop named foo for i from 10 downto (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.65
  (block nil
    (loop named foo for i from 10 above (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.66
  (block nil
    (loop named foo for i from 10 below (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.67
  (block nil
    (loop named foo for i in '(a b c) by (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.68
  (block nil
    (loop named foo for i on '(a b c) by (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.69
  (block nil
    (loop named foo for i = 1 then (return :good))
    :bad)
  :good)

(deftest loop.13.70
  (block nil
    (loop named foo for x in '(a b c) collect (return :good))
    :bad)
  :good)

(deftest loop.13.71
  (block nil
    (loop named foo for x in '(a b c) append (return :good))
    :bad)
  :good)

(deftest loop.13.72
  (block nil
    (loop named foo for x in '(a b c) nconc (return :good))
    :bad)
  :good)

(deftest loop.13.73
  (block nil
    (loop named foo for x in '(a b c) count (return :good))
    :bad)
  :good)

(deftest loop.13.74
  (block nil
    (loop named foo for x in '(a b c) sum (return :good))
    :bad)
  :good)

(deftest loop.13.75
  (block nil
    (loop named foo for x in '(a b c) maximize (return :good))
    :bad)
  :good)

(deftest loop.13.76
  (block nil
    (loop named foo for x in '(a b c) minimize (return :good))
    :bad)
  :good)

(deftest loop.13.77
  (block nil
    (loop named foo for x in '(a b c) thereis (return :good))
    :bad)
  :good)

(deftest loop.13.78
  (block nil
    (loop named foo for x in '(a b c) always (return :good))
    :bad)
  :good)

(deftest loop.13.79
  (block nil
    (loop named foo for x in '(a b c) never (return :good))
    :bad)
  :good)

(deftest loop.13.80
  (block nil
    (loop named foo for x in '(a b c) until (return :good))
    :bad)
  :good)

(deftest loop.13.81
  (block nil
    (loop named foo for x in '(a b c) while (return :good))
    :bad)
  :good)

(deftest loop.13.82
  (block nil
    (loop named foo for x in '(a b c) when (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.83
  (block nil
    (loop named foo for x in '(a b c) unless (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.84
  (block nil
    (loop named foo for x in '(a b c) if (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.85
  (block nil
    (loop named foo for x in '(a b c) return (return :good))
    :bad)
  :good)

(deftest loop.13.86
  (block nil
    (loop named foo initially (return :good) return :bad)
    :bad)
  :good)

(deftest loop.13.87
  (block nil
    (loop named foo do (loop-finish) finally (return :good))
    :bad)
  :good)

;;; Test that explicit calls to macroexpand in subforms
;;; are done in the correct environment

(deftest loop.13.88
  (macrolet
   ((%m (z) z))
   (loop do (expand-in-current-env (%m (return 10)))))
  10)

(deftest loop.13.89
  (macrolet
   ((%m (z) z))
   (loop for i from 0 below 100 by 7
         when (> i 50) return (expand-in-current-env (%m i))))
  56)

(deftest loop.13.90
  (macrolet
   ((%m (z) z))
   (loop return (expand-in-current-env (%m 'a))))
  a)
