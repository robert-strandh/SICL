(cl:in-package #:sicl-printer)

(defmethod print-object ((object class) stream)
  (print-unreadable-object (object stream :type t)
    (print (class-name object) stream)))
