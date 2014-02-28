(cl:in-package #:sicl-clos)

(defparameter *more-bridge-generic-functions*
  '(symbol-name
    symbol-package
    array-dimensions
    fill-pointer
    (setf fill-pointer)
    functions
    (setf functions)
    classes
    (setf classes)))

(loop for name in *more-bridge-generic-functions*
      do (setf (fdefinition name)
	       (find-bridge-generic-function name))
	 (delete-bridge-generic-function name))


