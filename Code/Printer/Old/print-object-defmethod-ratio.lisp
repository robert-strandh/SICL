(cl:in-package #:sicl-printer)

(defmethod print-object ((object ratio) stream)
  (print-ratio object *print-base* stream))
