(in-package #:sicl-clos-test)

(defgeneric reader-method-class (class direct-slot &rest initargs))

(defmethod reader-method-class
    ((class standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

(defmethod reader-method-class
    ((class funcallable-standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

;;; FIXME: we should probably use a method on 
;;; make-method-lambda for this one to call.
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

(defgeneric writer-method-class (class direct-slot &rest initargs))

(defmethod writer-method-class
    ((class standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-writer-method))

(defmethod writer-method-class
    ((class funcallable-standard-class)
     (direct-slot standard-direct-slot-definition)
     &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-writer-method))

;;; FIXME: we should probably use a method on 
;;; make-method-lambda for this one to call.
(defun add-writer-method (class function-name slot)
  (let* ((slot-name (slot-definition-name slot))
	 (lambda-list '(new-value object))
	 (generic-function (ensure-generic-function
			    function-name :lambda-list lambda-list))
	 (specializers (list class))
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


  