(cl:in-package #:sicl-data-and-control-flow)

(defmacro return (form)
  `(return-from nil ,form))
