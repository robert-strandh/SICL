(cl:in-package #:sicl-list-hash-table)

(defclass list-hash-table (hash-table)
  ((%size :initform 0 :initarg :size :accessor size :reader hash-table-size)
   (%contents :initform '() :accessor contents))
  (:metaclass built-in-class))
