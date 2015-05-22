(cl:in-package #:sicl-boot-phase1)

;;; We define a subclass of the normal extrinsic environment.  This
;;; version of the extrinsic environment is used as compilation
;;; environment in phase 1 of bootstrapping.  By creating a special
;;; version of it, we can define an :AFTER method on
;;; INITIALIZE-INSTANCE where we can modify it to correspond to the
;;; needs of bootstrapping.
(defclass compilation-environment (sicl-extrinsic-environment:environment)
  ())

(defmethod initialize-instance :after
    ((environment compilation-environment) &key)
  nil)

(defun define-make-method-lambda (cenv rtenv)
  (setf (sicl-genv:fdefinition 'sicl-clos:make-method-lambda rtenv)
	(lambda (generic-function method lambda-expression env)
	  (let* ((body-fun (cleavir-env:eval lambda-expression
					     cenv
					     rtenv))
		 (body-expr `(lambda (&rest args) (apply ,body-fun args))))
	    `(lambda (&rest args)
	       (apply ,(eval (funcall #'closer-mop:make-method-lambda
				      generic-function
				      method
				      body-expr
				      env))
		      args))))))

(defun customize (compilation-environment run-time-environment)
  (define-make-method-lambda compilation-environment run-time-environment))

(defclass environment (sicl-extrinsic-environment:environment)
  ((%compilation-environment
    :initarg :compilation-environment
    :initform (make-instance 'compilation-environment)
    :reader compilation-environment)))

(defmethod initialize-instance :after ((environment environment) &key)
  (customize (compilation-environment environment) environment)
  (fill-environment environment))
