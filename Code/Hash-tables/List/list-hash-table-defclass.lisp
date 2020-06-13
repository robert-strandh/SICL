(cl:in-package #:sicl-list-hash-table)

(defclass list-hash-table (hash-table)
  ((%size :initform 0 :initarg :size :accessor size :reader hash-table-size)
   (%contents :initform '() :accessor contents))
  (:default-initargs :rehash-size 1 :rehash-threshold 1))

(setf sicl-hash-table:*default-hash-table-class* (find-class 'list-hash-table))
