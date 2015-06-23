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

;;; The purpose of this function is to redefine the macro DEFGENERIC
;;; in the compilation environment C2.  The new definition is only
;;; used to create bridge-class accessors in run-time environment
;;; during phase 2.  Therefore, we can process only the name and the
;;; lambda-list of each DEFGENERIC form.  Furthermore, by passing all
;;; the initialization arguments to MAKE-INSTANCE, we avoid having to
;;; implement the generic-function initialization protocol in phase 2.
(defun define-defgeneric-c2 (boot)
  (setf (sicl-genv:macro-function 'defgeneric (c2 boot))
	(lambda (form environment)
	  (declare (ignore environment))
	  `(let* ((r1 ,(r1 boot))
		  (class-name 'standard-generic-function)
		  (class (sicl-genv:find-class class-name r1))
		  (method-class-name 'standard-method)
		  (method-class (sicl-genv:find-class method-class-name r1))
		  (gf (make-instance class
			:name ',(second form)
			:lambda-list ',(third form)
			:argument-precedence-order ',(third form)
			:declarations '()
			:documentation nil
			;; FIXME: supply a method-combination metaobject.
			:method-combination nil
			:method-class method-class)))
	     (setf (sicl-genv:fdefinition ',(second form) ,(r3 boot)) gf)))))

(defun customize-c1 (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot)))
    (message "Customizing environments~%")
    (define-defgeneric-c1 boot)
    (define-defgeneric-c2 boot)
    (ld "../CLOS/make-method-lambda-support.lisp" c1 c1)
    (ld "../CLOS/make-method-lambda-defuns.lisp" c1 c1)
    (ld "../CLOS/ensure-method.lisp" c1 r1)
    (define-generic-function-method-class c1)
    (define-generic-function-method-class r1)
    (message "Finished customizing environments~%")))
