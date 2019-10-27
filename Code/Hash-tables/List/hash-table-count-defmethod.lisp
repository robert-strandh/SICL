(cl:in-package #:sicl-list-hash-table)

(defmethod hash-table-count ((hash-table list-hash-table))
  (length (contents hash-table)))
