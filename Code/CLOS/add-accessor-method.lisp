(in-package #:sicl-clos)

(defun add-reader-method (class function-name slot)
  (let* ((slot-name (slot-definition-name slot))
	 (lambda-list '(object))
	 (generic-function (ensure-generic-function
			    function-name :lambda-list lambda-list))
	 (specializers (list class))
	 (method-function
	   (compile nil `(lambda (args next-methods)
			   (declare (ignore next-methods))
			   (let ((object (car args)))
			     (slot-value object ',slot-name)))))
	 (method-class (reader-method-class
			class slot
			:lambda-list lambda-list
			:specializers specializers
			:function method-function
			:slot-definition slot))
	 (method (make-instance method-class
				:lambda-list lambda-list
				:specializers specializers
				:function method-function
				:slot-definition slot)))
    (add-method generic-function method)))

(defun add-writer-method (class function-name slot)
  (let* ((slot-name (slot-definition-name slot))
	 (lambda-list '(new-value object))
	 (generic-function (ensure-generic-function
			    function-name :lambda-list lambda-list))
	 (specializers (list (find-class t) class))
	 (method-function
	   (compile nil `(lambda (args next-methods)
			   (declare (ignore next-methods))
			   (let ((new-value (car args))
				 (object (cadr args)))
			     (setf (slot-value object ',slot-name)
				   new-value)))))
	 (method-class (writer-method-class
			class slot
			:lambda-list lambda-list
			:specializers specializers
			:function method-function
			:slot-definition slot))
	 (method (make-instance method-class
				:lambda-list lambda-list
				:specializers specializers
				:function method-function
				:slot-definition slot)))
    (add-method generic-function method)))
