(cl:in-package #:sicl-clos)

(defmacro defgeneric (name parameters)
  `(progn
     (let ((fun (cl:make-instance 'bridge-generic-function
		  :name ',name :lambda-list ',parameters)))
       (setf (fdefinition 'temporary-name) fun)
       (cl:defmethod temporary-name ,parameters
	 (funcall (discriminating-function fun) ,@parameters))
       (push (cons ',name fun) *bridge-generic-functions*))))
