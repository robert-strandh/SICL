(in-package #:sicl-conditionals-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for the or macro

(define-test or-0
  (assert-equal 'nil
                (or)))

(define-test or-1
  (assert-equal 'nil
                (or nil)))

(define-test or-2
  (assert-equal '0
                (or 0)))

(define-test or-3
  (assert-equal '0
                (or 0 nil)))

(define-test or-4
  (let ((x 0))
    (assert-equal '0
                  (or 0 (/ x)))))

(define-test or-5
  (let ((x 0))
    (assert-equal '0
                  (or nil 0 (/ x)))))

(define-test or-6
  (assert-equal '(0)
                (multiple-value-list (or nil (values 0 1) nil))))

(define-test or-7
  (assert-equal '(0 1)
                (multiple-value-list (or nil (values 0 1)))))

(define-test or-error-0
  (assert-error 'program-error
                (macroexpand-1 '(or . 1))))

(define-test or-error-1
  (assert-error 'program-error
                (macroexpand-1 '(or t . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for the and macro

(define-test and-0
  (assert-equal 't
                (and)))

(define-test and-1
  (assert-equal 'nil
                (and nil)))

(define-test and-2
  (assert-equal '0
                (and 0)))

(define-test and-3
  (assert-equal 'nil
                (and nil 0)))

(define-test and-4
  (assert-equal '0
                (and 1 0)))

(define-test and-5
  (assert-equal 'nil
                (and 1 nil 0)))

(define-test and-6
  (assert-equal '(0 1)
                (multiple-value-list (and 0 2 (values 0 1)))))

(define-test and-7
  (assert-equal '(2)
                (multiple-value-list (and 0 (values 0 1) 2))))

(define-test and-error-0
  (assert-error 'program-error
                (macroexpand-1 '(and . 1))))

(define-test and-error-1
  (assert-error 'program-error
                (macroexpand-1 '(and nil . 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for the cond macro

(define-test cond-1
  (assert-equal 'nil
                (cond)))

(define-test cond-2
  (assert-equal 'nil
                (cond (nil))))

(define-test cond-3
  (assert-equal 't
                (cond (t))))

(define-test cond-4
  (assert-equal '0
                (cond (0))))

(define-test cond-5
  (assert-equal '1
                (cond (0 1))))

(define-test cond-6
  (assert-equal '2
                (cond (0 1 2))))

(define-test cond-7
  (assert-equal '2
                (cond (t 2)
                      (t 1))))

(define-test cond-8
  (assert-equal '1
                (cond (nil 2)
                      (t 1))))

(define-test cond-error-1
  (assert-error 'program-error
                (macroexpand-1 '(cond x))))

(define-test cond-error-2
  (assert-error 'program-error
                (macroexpand-1 '(cond t))))

(define-test cond-error-3
  (assert-error 'program-error
                (macroexpand-1 '(cond ()))))

(define-test cond-error-4
  (assert-error 'program-error
                (macroexpand-1 '(cond . 1))))

(define-test cond-error-5
  (assert-error 'program-error
                (macroexpand-1 '(cond (t 2) . 1))))
