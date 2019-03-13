(cl:in-package #:sicl-data-and-control-flow)

(defmacro return (&optional form)
  `(return-from nil ,form))
