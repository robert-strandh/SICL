(cl:in-package #:sicl-structure)

(defmethod print-object ((object structure-object) stream)
  (print-structure object stream))
