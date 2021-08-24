(cl:in-package #:sicl-data-and-control-flow)

(defmacro rotatef (&rest places)
  (rotatef-expander places))
