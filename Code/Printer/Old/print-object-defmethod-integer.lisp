(cl:in-package #:sicl-printer)

(defmethod print-object ((object integer) stream)
  (print-integer object *print-base* stream))
