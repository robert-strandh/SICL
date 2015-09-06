(cl:in-package #:sicl-clos)

(defmethod method-combination-compute-effective-method
    (method-combination methods)
  ;; FIXME: specialize to standard-method-combination
  (declare (ignore method-combination))
  (compute-effective-method-default methods))
