(cl:in-package #:sicl-printer)

(defmethod print-object ((object generic-function) stream)
  (print-unreadable-object (object stream :type t)
    (format stream
            "~s (~d)"
            (sicl-clos:generic-function-name object)
            (length (sicl-clos:generic-function-methods object)))))
