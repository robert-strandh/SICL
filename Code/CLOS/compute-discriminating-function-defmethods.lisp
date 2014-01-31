(cl:in-package #:sicl-clos)

;;;; This file contains a definition of the specified method on the
;;;; generic function COMPUTE-DISCRIMINATING-FUNCTION specialized for
;;;; STANDARD-GENERIC-FUNCTION. 

(defmethod compute-discriminating-function
    ((generic-function standard-generic-function))
  (compute-discriminating-function-default generic-function))
