(cl:in-package #:sicl-data-and-control-flow)

(defmacro shiftf (&rest arguments)
  (shiftf-expander arguments))
