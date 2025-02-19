(cl:in-package #:sicl-hir)

(defclass global-function-reference-instruction (instruction)
  ((%function-name
    :initarg :function-name
    :reader function-name)))
