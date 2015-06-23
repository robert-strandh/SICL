(cl:in-package #:sicl-boot)

;;; Define the macro DEFGENERIC in compile-time environment C1.  We
;;; define it a bit differently from its usual definition.  Its main
;;; purpose is to define a generic function in the run-time
;;; environment R2.  However, before definining it, we remove the
;;; existing generic function if it exists.  This way, we are sure to
;;; get a fresh generic function, as opposed to one that happened to
;;; have been imported from the host.  We must, of course, make sure
;;; that we execute a DEFGENERIC form for a particular generic
;;; function exactly once, but we can do that because we completely
;;; master the boot process.  We also put the same definition in the
;;; compile-time environment C1 so that the compiler sees it when
;;; subsequent forms that use this function are compiled.
(defun define-defgeneric-c1 (boot)
  (setf (sicl-genv:macro-function 'defgeneric (c1 boot))
	(lambda (form environment)
	  (declare (ignore environment))
	  `(progn (sicl-genv:fmakunbound ',(second form) ,(r2 boot))
		  (setf (sicl-genv:fdefinition ',(second form) ,(r2 boot))
			(ensure-generic-function
			 ',(second form)
			 :name ',(second form)
			 :lambda-list ',(third form)))
		  (setf (sicl-genv:fdefinition ',(second form) ,(c1 boot))
			(sicl-genv:fdefinition ',(second form) ,(r2 boot)))))))

(defun customize-c1 (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot)))
    (message "Customizing environments~%")
    (define-defgeneric-c1 boot)
    (ld "../CLOS/make-method-lambda-support.lisp" c1 c1)
    (ld "../CLOS/make-method-lambda-defuns.lisp" c1 c1)
    (ld "../CLOS/ensure-method.lisp" c1 r1)
    (define-generic-function-method-class c1)
    (define-generic-function-method-class r1)
    (message "Finished customizing environments~%")))
