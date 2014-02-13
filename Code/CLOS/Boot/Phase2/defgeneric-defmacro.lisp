(cl:in-package #:sicl-clos)

(defmacro defgeneric (name parameters)
  `(progn
     (let ((fun (cl:make-instance 'bridge-generic-function
		  :name ',name :lambda-list ',parameters)))
       (add-bridge-generic-function ',name fun))))
