(cl:in-package #:sicl-data-and-control-flow)

(defmacro psetq (&rest pairs)
  `(psetf ,@pairs))
