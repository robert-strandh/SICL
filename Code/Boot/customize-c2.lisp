(cl:in-package #:sicl-boot)

;;; The purpose of this function is to redefine the macro DEFGENERIC
;;; in the compilation environment C2.  The new definition is only
;;; used to create bridge-class accessors in run-time environment
;;; during phase 2.  Therefore, we can process only the name and the
;;; lambda-list of each DEFGENERIC form.  Furthermore, by passing all
;;; the initialization arguments to MAKE-INSTANCE, we avoid having to
;;; implement the generic-function initialization protocol in phase 2.
(defun define-defgeneric (boot)
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

(defun customize-c2 (boot)
  (message "Customizing compilation environment C2~%")
  (define-defgeneric boot)
  (message "Finished customizing compilation environment C2~%"))

;;  LocalWords:  accessors
