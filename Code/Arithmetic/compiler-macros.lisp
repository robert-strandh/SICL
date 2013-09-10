(cl:in-package #:sicl-arithmetic)

(define-compiler-macro + (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	((null (cddr args)) `(binary-+ ,(car args) ,(cadr args)))
	(t `(binary-+ (+ ,@(butlast args)) ,(car (last args))))))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro - (x &rest args)
  (cond ((null args) `(negate ,x))
	((null (cdr args)) `(binary-- ,x (car args)))
	(t `(binary-- (- ,@(butlast args)) ,(car (last args))))))

(define-compiler-macro * (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	((null (cddr args)) `(binary-* ,(car args) ,(cadr args)))
	(t `(binary-* (* ,@(butlast args)) ,(car (last args))))))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro / (x &rest args)
  (cond ((null args) `(invert ,x))
	((null (cdr args)) `(binary-/ ,x (car args)))
	(t `(binary-/ (/ ,@(butlast args)) ,(car (last args))))))

