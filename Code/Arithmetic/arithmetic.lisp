(in-package :sicl.arithmetic)

;;; addition
(defun binary-+ (x y)
  (check-type x number "a number")
  (check-type y number "a number")
  (typecase x
    (fixnum (typecase y
	      (fixnum (fixnum-fixnum-+ x y))
	      (bignum (fixnum-bignum-+ x y))
	      (rational (fixnum-rational-+ x y))
	      (float (fixnum-float-+ x y))))
    (bignum (typecase y
	      (fixnum (bignum-fixnum-+ x y))
	      (bignum (bignum-bignum-+ x y))
	      (rational (bignum-rational-+ x y))
	      (float (bignum-float-+ x y))))
    (rational (typecase y
		(fixnum (ratio-fixnum-+ x y))
		(bignum (ratio-bignum-+ x y))
		(rational (ratio-rational-+ x y))
		(float (ratio-float-+ x y))))
    (float (typecase y
	     (fixnum (float-fixnum-+ x y))
	     (bignum (float-bignum-+ x y))
	     (rational (float-rational-+ x y))
	     (float (float-float-+ x y))))))

(defun + (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	(t (apply #'+ (binary-+ (car args) (cadr args)) (cddr args)))))
 
(define-compiler-macro + (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	((null (cddr args)) `(binary-+ ,(car args) ,(cadr args)))
	(t `(binary-+ (+ ,@(butlast args)) ,(car (last args))))))

;;; subtraction
(defun binary-- (x y)
  (check-type x number "a number")
  (check-type y number "a number")
  (typecase x
    (fixnum (typecase y
	      (fixnum (fixnum-fixnum-- x y))
	      (bignum (fixnum-bignum-- x y))
	      (rational (fixnum-rational-- x y))
	      (float (fixnum-float-- x y))))
    (bignum (typecase y
	      (fixnum (bignum-fixnum-- x y))
	      (bignum (bignum-bignum-- x y))
	      (rational (bignum-rational-- x y))
	      (float (bignum-float-- x y))))
    (rational (typecase y
		(fixnum (ratio-fixnum-- x y))
		(bignum (ratio-bignum-- x y))
		(rational (ratio-rational-- x y))
		(float (ratio-float-- x y))))
    (float (typecase y
	     (fixnum (float-fixnum-- x y))
	     (bignum (float-bignum-- x y))
	     (rational (float-rational-- x y))
	     (float (float-float-- x y))))))

(defun - (x &rest args)
  (cond ((null args) (negate x))
	((null (cdr args)) (binary-- x (car args)))
	(t (apply #'- (binary-- x (car args)) (cdr args)))))
 
;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro - (x &rest args)
  (cond ((null args) `(negate ,x))
	((null (cdr args)) `(binary-- ,x (car args)))
	(t `(binary-- (- ,@(butlast args)) ,(car (last args))))))

;;; multiplication
(defun binary-* (x y)
  (check-type x number "a number")
  (check-type y number "a number")
  (typecase x
    (fixnum (typecase y
	      (fixnum (fixnum-fixnum-* x y))
	      (bignum (fixnum-bignum-* x y))
	      (rational (fixnum-rational-* x y))
	      (float (fixnum-float-* x y))))
    (bignum (typecase y
	      (fixnum (bignum-fixnum-* x y))
	      (bignum (bignum-bignum-* x y))
	      (rational (bignum-rational-* x y))
	      (float (bignum-float-* x y))))
    (rational (typecase y
		(fixnum (ratio-fixnum-* x y))
		(bignum (ratio-bignum-* x y))
		(rational (ratio-rational-* x y))
		(float (ratio-float-* x y))))
    (float (typecase y
	     (fixnum (float-fixnum-* x y))
	     (bignum (float-bignum-* x y))
	     (rational (float-rational-* x y))
	     (float (float-float-* x y))))))

(defun * (&rest args)
  (cond ((null args) 1)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	(t (apply #'* (binary-* (car args) (cadr args)) (cddr args)))))
 
(define-compiler-macro * (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	((null (cddr args)) `(binary-* ,(car args) ,(cadr args)))
	(t `(binary-* (* ,@(butlast args)) ,(car (last args))))))

;;; division
(defun binary-/ (x y)
  (check-type x number "a number")
  (check-type y number "a number")
  (typecase x
    (fixnum (typecase y
	      (fixnum (fixnum-fixnum-/ x y))
	      (bignum (fixnum-bignum-/ x y))
	      (rational (fixnum-rational-/ x y))
	      (float (fixnum-float-/ x y))))
    (bignum (typecase y
	      (fixnum (bignum-fixnum-/ x y))
	      (bignum (bignum-bignum-/ x y))
	      (rational (bignum-rational-/ x y))
	      (float (bignum-float-/ x y))))
    (rational (typecase y
		(fixnum (ratio-fixnum-/ x y))
		(bignum (ratio-bignum-/ x y))
		(rational (ratio-rational-/ x y))
		(float (ratio-float-/ x y))))
    (float (typecase y
	     (fixnum (float-fixnum-/ x y))
	     (bignum (float-bignum-/ x y))
	     (rational (float-rational-/ x y))
	     (float (float-float-/ x y))))))

(defun / (x &rest args)
  (cond ((null args) (invert x))
	((null (cdr args)) (binary-/ x (car args)))
	(t (apply #'/ (binary-/ x (car args)) (cdr args)))))
 
;;; FIXME: do this better by not having a required argument
;;; and instead reporting a compilation error when no
;;; arguments are given
(define-compiler-macro / (x &rest args)
  (cond ((null args) `(invert ,x))
	((null (cdr args)) `(binary-/ ,x (car args)))
	(t `(binary-/ (/ ,@(butlast args)) ,(car (last args))))))


