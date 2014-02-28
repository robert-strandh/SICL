(cl:in-package #:sicl-clos)

(defparameter *more-bridge-generic-functions*
  '())

(loop for name in *more-bridge-generic-functions*
      do (setf (fdefinition name)
	       (find-bridge-generic-function name))
	 (delete-bridge-generic-function name))


