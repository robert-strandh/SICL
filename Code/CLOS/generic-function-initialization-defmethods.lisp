(cl:in-package #:sicl-clos)

(defmethod shared-initialize :around
    ((generic-function generic-function)
     slot-names
     &rest initargs
     &key
       documentation
       declarations
       (lambda-list nil lambda-list-p)
       (argument-precedence-order nil argument-precedence-order-p)
       method-combination
       (method-class (find-class 'standard-method))
       name
     &allow-other-keys)
  (check-documentation documentation)
  (check-declarations declarations)
  (check-method-combination method-combination)
  (check-method-class method-class)
  (if lambda-list-p
      (let* ((parsed-lambda-list
	       (cleavir-code-utilities:parse-generic-function-lambda-list
		lambda-list))
	     (required (cleavir-code-utilities:required parsed-lambda-list)))
	(if argument-precedence-order-p
	    (check-argument-precedence-order argument-precedence-order required)
	    (setf argument-precedence-order required))
	(apply #'call-next-method
	       generic-function
	       slot-names
	       :documentation documentation
	       :declarations declarations
	       :argument-precedence-order argument-precedence-order
	       :specializer-profile (make-list (length required)
					       :initial-element nil)
	       :method-class method-class
	       :name name
	       initargs))
      (if argument-precedence-order-p
	  (error "when argument precedence order appears,~@
                  so must lambda list")
	(apply #'call-next-method
	       generic-function
	       slot-names
	       :documentation documentation
	       :declarations declarations
	       :method-class method-class
	       :name name
	       initargs))))
