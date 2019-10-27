(cl:in-package #:sicl-list-hash-table)

(defmethod clrhash ((hash-table list-hash-table))
  (setf (contents hash-table) '()))
