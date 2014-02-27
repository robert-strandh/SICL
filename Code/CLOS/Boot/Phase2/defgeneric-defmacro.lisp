(cl:in-package #:sicl-clos)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (fmakunbound 'defgeneric))

(defmacro defgeneric (name parameters)
  `(ensure-generic-function ',name :lambda-list ',parameters))
