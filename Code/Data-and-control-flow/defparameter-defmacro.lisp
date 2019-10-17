(cl:in-package #:sicl-data-and-control-flow)

(defmacro defparameter (name initial-value &optional documentation)
  ;; FIXME: handle the documentation.
  (declare (ignore documentation))
  `(setf (sicl-genv:special-variable ',name (sicl-genv:global-environment) t)
         ,initial-value))
