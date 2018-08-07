(cl:in-package #:sicl-data-and-control-flow)

(defmacro rotatef (&environment environment &rest places)
  (rotatef-expander environment places))
