(cl:in-package #:sicl-hash-table)

(defclass hash-table (t)
  ((%test :initarg :test :reader test))
  (:metaclass built-in-class))
