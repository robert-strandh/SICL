(cl:in-package #:sicl-boot)

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
	     (setf (sicl-genv:fdefinition ',(second form) ,(r2 boot)) gf)))))

;;; The problem that we are solving here is that during class
;;; initialization, there is a test that each superclass is of type
;;; CLASS, and that test uses TYPEP like this (TYPEP c 'CLASS).  But
;;; if we use the host version of TYPEP, it will return NIL because a
;;; bridge class is not a host class.  We solve this problem by
;;; supplying a slightly modified version of TYPEP in R2.  This
;;; modified version relies on the fact that when TYPEP is called this
;;; way, it is either called with a symbol or with a bridge class.  We
;;; capture this case, and return T when we are called with a standard
;;; object.
(defun define-typep (boot)
  (setf (sicl-genv:fdefinition 'typep (r2 boot))
	(lambda (object type)
	  (if (eq type 'class)
	      (typep object 'standard-object)
	      (typep object type)))))

(defun customize-for-phase1 (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot)))
    (message "Customizing environments for phase 1~%")
    (define-temporary-ensure-method-c1-r1 boot)
    (define-class-function-r1 boot)
    (define-funcallable-standard-class)
    ;; Rather than calling MAKE-METHOD-LAMBDA, the temporary
    ;; definition of the macro DEFMETHOD calls
    ;; MAKE-METHOD-LAMBDA-DEFAULT directly.
    (ld "defmethod-defmacro-c1.lisp" c1 c1)
    ;; Load a definition of MAKE-METHOD-LAMBDA-DEFAULT to be used with
    ;; the temporary definition of the macro DEFMETHOD.
    (ld "../CLOS/make-method-lambda-support.lisp" c1 c1)
    (ld "../CLOS/ensure-method.lisp" c1 r1)
    (define-ensure-class r1)
    (message "Finished customizing environments for phase 1~%")))

(defun customize-for-phase2 (boot)
  (let ((c1 (c1 boot))
	(r1 (r1 boot))
	(r2 (r2 boot)))
    (message "Customizing environments for phase 2~%")
    (define-make-instance boot)
    (define-defgeneric-c2 boot)
    (define-find-class boot)
    (define-typep boot)
    (define-ensure-generic-function-r1 boot)
    (define-reader-method-class boot)
    (define-writer-method-class boot)
    (define-ensure-class r2)
    (define-add-method boot)
    (ld "../CLOS/invalidate-discriminating-function.lisp" c1 r1)
    (ld "../CLOS/generic-function-initialization-defmethods.lisp" c1 r1)
    (ld "../CLOS/ensure-generic-function-using-class-support.lisp" c1 r2)
    (message "Finished customizing environments for phase 2~%")))
