(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'defgeneric))

(defmacro defgeneric (name parameters)
  `(progn
     (let* ((class (cdr (assoc 'standard-generic-function *bridge-classes*)))
	    (fun (allocate-instance class)))
       (shared-initialize-default fun t
				  :name ',name
				  :lambda-list ',parameters)
       (initialize-instance-after-standard-generic-function-default
	fun :name ',name :lambda-list ',parameters)
       (push (cons ',name fun) *target-generic-functions*))))
