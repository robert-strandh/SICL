(cl:in-package #:sicl-printer)

(defmethod print-object ((object sicl-clos:eql-specializer) stream)
  (print-unreadable-object (object stream :type t)
    (print (sicl-clos:eql-specializer-object object))))
