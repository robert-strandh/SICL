(in-package #:sicl-conditionals-test)

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

(define-test cond-error-1
  (assert-error 'program-error
		(macroexpand-1 '(cond x))))

(define-test cond-error-2
  (assert-error 'program-error
		(macroexpand-1 '(cond t))))

(define-test cond-error-3
  (assert-error 'program-error
		(macroexpand-1 '(cond ()))))

(define-test cond-error-3
  (assert-error 'program-error
		(macroexpand-1 '(cond ()))))

