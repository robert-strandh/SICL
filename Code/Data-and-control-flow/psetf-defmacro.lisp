(cl:in-package #:sicl-data-and-control-flow)

(defmacro psetf (&environment environment &rest pairs)
  (psetf-expander environment pairs))
