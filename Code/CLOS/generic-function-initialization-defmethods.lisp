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
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
       method-combination
     &allow-other-keys)
  (check-documentation documentation)
  (check-declarations declarations)
  (check-method-combination method-combination)
  (if lambda-list-p
      (let* ((parsed-lambda-list
	       (parse-generic-function-lambda-list lambda-list))
	     (required (required parsed-lambda-list)))
	(if argument-precedence-order-p
	    (check-argument-precedence-order argument-precedence-order required)
	    (setf argument-precedence-order required))
	(apply #'call-next-method
	       generic-function
	       :documentation documentation
	       :declarations declarations
	       :argument-precedence-order argument-precedence-order
	       :specializer-profile (make-list (length required)
					       :initial-element nil)
	       initargs))
      (if argument-precedence-order-p
	  (error "when argument precedence order appears,~@
                  so must lambda list")
	(apply #'call-next-method
	       generic-function
	       :documentation documentation
	       :declarations declarations
	       initargs))))

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
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
       method-combination
     &allow-other-keys)
  (check-documentation documentation)
  (check-declarations declarations)
  (check-method-combination method-combination)
  (if lambda-list-p
      (let* ((parsed-lambda-list
	       (parse-generic-function-lambda-list lambda-list))
	     (required (required parsed-lambda-list)))
	(if argument-precedence-order-p
	    (check-argument-precedence-order argument-precedence-order required)
	    (setf argument-precedence-order required))
	(apply #'call-next-method
	       generic-function
	       :argument-precedence-order argument-precedence-order
	       :specializer-profile (make-list (length required)
					       :initial-element nil)
	       initargs))
      (if argument-precedence-order-p
	  (error "when argument precedence order appears,~@
                  so must lambda list")
	  (call-next-method))))

