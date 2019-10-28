(cl:in-package #:sicl-list-hash-table)

(defclass list-hash-table (sicl-hash-table:hash-table)
  ((%size :initform 0 :initarg :size :accessor size :reader hash-table-size)
   (%contents :initform '() :accessor contents)))
