(cl:in-package #:sicl-printer)

(defmethod print-object ((object complex) stream)
  (print-object (realpart object) stream)
  (print-object (imagpart object) stream))
