(cl:in-package #:sicl-clos)

(defmacro defmethod (function-name &rest rest)
  (multiple-value-bind
	(qualifiers lambda-list specializers declarations documentation forms)
      (parse-defmethod rest)
    (let ((generic-function-var (gensym)))
      `(let ((,generic-function-var (ensure-generic-function ',function-name)))
	 (ensure-method
	  ,generic-function-var
	  :lambda-list ',lambda-list
	  :qualifiers ',qualifiers
	  :specializers ,(canonicalize-specializers specializers)
	  :documentation ,documentation
	  :body ',forms
	  :function (make-method-lambda
		     ,generic-function-var
		     (class-prototype
		      (generic-function-method-class ,generic-function-var))
		     '(lambda ,lambda-list
		       ,@declarations
		       ,@forms)
		     nil))))))

