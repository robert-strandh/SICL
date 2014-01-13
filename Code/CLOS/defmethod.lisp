(cl:in-package #:sicl-clos)

(defun parse-defmethod (args)
  (let ((name (pop args))
	(qualifiers (loop while (and (consp args) (not (listp (car args))))
			  collect (pop args)))
	(lambda-list (pop args)))
    ;; FIXME: handle declarations and documentation
    (let* ((parsed-lambda-list (parse-specialized-lambda-list lambda-list))
	   (required (required parsed-lambda-list)))
      (values name
	      qualifiers
	      (append (mapcar #'car required)
		      (subseq lambda-list (length required)))
	      (mapcar #'cadr required)
	      args))))

(defun canonicalize-specializers (specializers)
  ;; FIXME: handle eql specializers.
  `(mapcar #'find-class ',specializers))

(defmacro defmethod (&rest arguments)
  (multiple-value-bind (name qualifiers lambda-list specializers body)
      (parse-defmethod arguments)
    (let ((generic-function-var (gensym)))
      `(let ((,generic-function-var (ensure-generic-function ',name)))
	 (ensure-method
	  ,generic-function-var
	  :lambda-list ',lambda-list
	  :qualifiers ',qualifiers
	  :specializers ,(canonicalize-specializers specializers)
	  :body ',body
	  :function (make-method-lambda
		     ,generic-function-var
		     (class-prototype
		      (generic-function-method-class ,generic-function-var))
		     '(lambda ,lambda-list ,@body)
		     nil))))))

