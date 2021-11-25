(cl:in-package #:cleavir-literals)

(defmacro with-fresh-similarity-table (&body body)
  `(let ((*similarity-table* (make-instance 'similarity-table)))
     ,@body))
