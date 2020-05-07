(cl:in-package #:sicl-printer)

(defmethod print-object ((object class) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~a" (class-name object))))
