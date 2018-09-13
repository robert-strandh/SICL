(cl:in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPUTE-EFFECTIVE-METHOD.

(defmethod compute-effective-method
    ((generic-function standard-generic-function) method-combination methods)
  (method-combination-compute-effective-method method-combination methods))

