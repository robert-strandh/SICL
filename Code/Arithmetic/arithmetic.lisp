(cl:in-package :sicl-arithmetic)

(defun + (&rest args)
  (cond ((null args) 0)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	(t (apply #'+ (binary-+ (car args) (cadr args)) (cddr args)))))
 
(defun - (x &rest args)
  (cond ((null args) (negate x))
	((null (cdr args)) (binary-- x (car args)))
	(t (apply #'- (binary-- x (car args)) (cdr args)))))

(defun * (&rest args)
  (cond ((null args) 1)
	;; FIXME: check that we have a number
	((null (cdr args)) (car args))
	(t (apply #'* (binary-* (car args) (cadr args)) (cddr args)))))

(defun / (x &rest args)
  (cond ((null args) (invert x))
	((null (cdr args)) (binary-/ x (car args)))
	(t (apply #'/ (binary-/ x (car args)) (cdr args)))))
 

