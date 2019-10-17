(cl:in-package #:sicl-data-and-control-flow)

(defmacro defconstant (name initial-value &optional documentation)
  ;; FIXME: handle the documentation.
  (declare (ignore documentation))
  `(setf (sicl-genv:constant-variable ',name (sicl-genv:global-environment))
         ,initial-value))
