(cl:in-package #:sicl-hash-table)

(defmethod equalp ((x hash-table) (y hash-table))
  (error "EQUALP on hash tables is not implemented yet."))
