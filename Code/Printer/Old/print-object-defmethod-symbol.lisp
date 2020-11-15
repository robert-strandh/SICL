(cl:in-package #:sicl-printer)
  
(defmethod print-object ((object symbol) stream)
  (print-symbol object stream))
