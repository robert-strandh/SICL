(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'defgeneric))

(defmacro defgeneric (name parameters)
  `(progn
     (let* ((class (find-bridge-class 'standard-generic-function))
	    (fun (make-instance-default class
		   :name ',name
		   :lambda-list ',parameters
		   :method-class (find-bridge-class 'standard-method))))
       (push (cons ',name fun) *target-generic-functions*))))
