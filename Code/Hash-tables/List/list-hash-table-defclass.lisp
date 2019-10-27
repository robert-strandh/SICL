(cl:in-package #:sicl-list-hash-table)

(defclass list-hash-table (hash-table)
  ((%contents :initform '() :accessor contents))
  (:metaclass built-in-class))
