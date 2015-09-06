(cl:in-package #:sicl-clos)

(defmethod method-combination-compute-effective-method
    (method-combination methods generic-function)
  ;; FIXME: specialize to standard-method-combination
  (declare (ignore method-combination))
  (compute-effective-method-default generic-function methods))
