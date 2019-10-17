(cl:in-package #:sicl-data-and-control-flow)

(defmacro defvar (name &optional initial-value documentation)
  ;; FIXME: handle the documentation.
  (declare (ignore documentation))
  `(setf (sicl-genv:special-variable ',name (sicl-genv:global-environment) nil)
         ,initial-value))
