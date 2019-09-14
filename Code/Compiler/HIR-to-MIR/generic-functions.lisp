(cl:in-package #:sicl-hir-to-mir)

(defgeneric process-instruction (client instruction))

(defmethod process-instruction (client instruction)
  (declare (ignore client instruction))
  nil)
