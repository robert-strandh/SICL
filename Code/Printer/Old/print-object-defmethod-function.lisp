(cl:in-package #:sicl-printer)

(defmethod print-object ((object function) stream)
  (print-unreadable-object (object stream :type t :identity t)))
