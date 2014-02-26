(cl:in-package #:sicl-clos)

(defparameter *installable-generic-functions-2*
  '(class-precedence-list
    slot-definition-name
    slot-definition-allocation
    slot-definition-storage
    initargs
    slot-definition-initfunction
    ))

(loop for name in *installable-generic-functions-2*
      do (setf (fdefinition name)
	       (find-bridge-generic-function name))
	 (delete-bridge-generic-function name))
