(cl:in-package #:sicl-data-and-control-flow)

(defmacro psetf (&rest pairs)
  (psetf-expander pairs))
