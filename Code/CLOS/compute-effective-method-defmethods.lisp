(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-EFFECTIVE-METHOD.

(defmethod compute-effective-method
    ((generic-function standard-generic-function) method-combination methods)
  (compute-effective-method-default generic-function method-combination methods))
