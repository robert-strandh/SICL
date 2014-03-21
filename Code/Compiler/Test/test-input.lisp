(cl:in-package #:common-lisp-user)

(defparameter *ast4*
  (p1:convert-top-level-form
   '40))

(defparameter *ast3*
  (p1:convert-top-level-form
   '(let ((x 10))
     (let ((y (sicl-word:u+ x 20)))
       y))))

(defparameter *ast2* 
	   (p1:convert-top-level-form 
	    '(block nil
	      (let* ((x '(a b c d e))
		     (y nil)
		     (z 'd))
		(tagbody
		 again
		   (unless 
		       (sicl-word:== 
			(sicl-word:& x (sicl-word:word 3))
			(sicl-word:word 1))
		     (go out))
		   (setq y (sicl-word:memref
			    (sicl-word:u- x (sicl-word:word 1))))
		   (when (sicl-word:== y z)
		     (return-from nil y))
		   (setq x 
			 (sicl-word:memref 
			  (sicl-word:u+ x (sicl-word:word 3))))
		   (go again)
		 out)))))

(defparameter *ast*
  (p1:convert-top-level-lamda-expression
   '(lambda (x)
     (if (consp x)
	 (sicl-word:memref x (sicl-word:word 3))
	 (if (null x)
	     nil
	     (error "arg must be list"))))))

(defparameter *f* (p2:compile-toplevel *ast*))

(defparameter *prog*
  (make-instance 'sicl-program:program
    :initial-instruction *f*
    :backend (make-instance 'sicl-x86-64-lir::backend-x86-64)))



