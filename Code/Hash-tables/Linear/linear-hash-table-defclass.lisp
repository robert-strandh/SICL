(cl:in-package #:sicl-linear-probing-hash-table)

(defclass linear-probing-hash-table (hashing-hash-table)
  ((size :initarg :size
         :initform 16
         :accessor %bucket-hash-table-size
         :reader hash-table-size)
   (metadata :accessor hash-table-metadata)
   (data     :accessor hash-table-data)
   (count :initform 0
          :accessor %bucket-hash-table-count
          :reader hash-table-count)))
