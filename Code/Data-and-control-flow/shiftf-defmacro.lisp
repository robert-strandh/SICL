(cl:in-package #:sicl-data-and-control-flow)

(defmacro shiftf (&environment environment &rest arguments)
  (shiftf-expander environment arguments))
