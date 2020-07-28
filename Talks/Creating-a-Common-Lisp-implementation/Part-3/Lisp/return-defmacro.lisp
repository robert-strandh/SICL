(cl:in-package #:target-data-and-control-flow)

(defmacro return (&optional (form nil))
  `(return-from nil ,form))
