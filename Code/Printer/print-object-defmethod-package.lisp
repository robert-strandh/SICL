(cl:in-package #:sicl-printer)

(defmethod print-object ((object package) stream)
  (print-unreadable-object (object stream :type t)
    (print (package-name object) stream)))
