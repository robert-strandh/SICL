(cl:in-package #:sicl-hash-table)

(defclass hash-table ()
  ((%test :initarg :test :reader hash-table-test)
   (%rehash-size :initarg :rehash-size :reader hash-table-rehash-size)
   (%rehash-threshold :initarg :rehash-threshold :reader hash-table-rehash-threshold)))
