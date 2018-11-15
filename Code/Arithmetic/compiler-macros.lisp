(cl:in-package #:sicl-arithmetic)

(define-compiler-macro + (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	((null (cddr args)) `(binary-add ,(car args) ,(cadr args)))
	(t `(binary-add (+ ,@(butlast args)) ,(car (last args))))))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro - (x &rest args)
  (cond ((null args) `(negate ,x))
	((null (cdr args)) `(binary-sub ,x ,(car args)))
	(t `(binary-sub (- ,@(butlast args)) ,(car (last args))))))

(define-compiler-macro * (&rest args)
  (cond ((null args) 1)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	((null (cddr args)) `(binary-mul ,(car args) ,(cadr args)))
	(t `(binary-mul (* ,@(butlast args)) ,(car (last args))))))

;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro / (x &rest args)
  (cond ((null args) `(invert ,x))
	((null (cdr args)) `(binary-div ,x ,(car args)))
	(t `(binary-div (/ ,@(butlast args)) ,(car (last args))))))

(define-compiler-macro < (x &rest args)
  (if (null args)
      t
      `(and (binary-less ,x ,(car args))
	    (< ,@args))))

(define-compiler-macro <= (x &rest args)
  (if (null args)
      t
      `(and (binary-not-greater ,x ,(car args))
	    (<= ,@args))))

(define-compiler-macro > (x &rest args)
  (if (null args)
      t
      `(and (binary-greater ,x ,(car args))
	    (> ,@args))))

(define-compiler-macro >= (x &rest args)
  (if (null args)
      t
      `(and (binary-not-less ,x ,(car args))
	    (>= ,@args))))

(define-compiler-macro = (x &rest args)
  (if (null args)
      t
      `(and (binary-equal ,x ,(car args))
	    (= ,@args))))

