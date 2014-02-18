(cl:in-package #:sicl-clos)

;;; FIXME: check whether these methods should specialize on
;;; GENERIC-FUNCTION rather than on STANDARD-GENERIC-FUNCTION.

(defmethod initialize-instance :after
    ((generic-function standard-generic-function)
     &rest initargs
     &key
       lambda-list
       argument-precedence-order
       documentation
       declarations
       method-combination
       method-class
       name
     &allow-other-keys)
  (declare (ignore lambda-list
		   argument-precedence-order
		   documentation
		   declarations
		   method-combination
		   method-class
		   name))
  (apply #'initialize-instance-after-standard-generic-function-default
	 generic-function initargs))

(defmethod initialize-instance :around
    ((generic-function generic-function)
     &rest initargs
     &key
       documentation
       declarations
     &allow-other-keys)
  (check-documentation documentation)
  (check-declarations declarations)
  (apply #'call-next-method
	 generic-function
	 :documentation documentation
	 :declarations declarations
	 initargs))

(defmethod reinitialize-instance :after
    ((generic-function standard-generic-function)
     &rest initargs
     &key
       lambda-list
       argument-precedence-order
       documentation
       declarations
       method-combination
       method-class
       name ; FIXME: check if this belongs here.
     &allow-other-keys)
  (declare (ignore lambda-list
		   argument-precedence-order
		   documentation
		   declarations
		   method-combination
		   method-class
		   name))
  (apply #'reinitialize-instance-after-standard-generic-function-default
	 generic-function initargs))

(defmethod reinitialize-instance :around
    ((generic-function generic-function)
     &rest initargs
     &key
       documentation
       declarations
     &allow-other-keys)
  (declare (ignore generic-function initargs))
  (check-documentation documentation)
  (check-declarations declarations)
  (call-next-method))
