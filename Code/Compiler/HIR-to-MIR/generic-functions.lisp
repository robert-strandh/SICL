(cl:in-package #:sicl-hir-to-mir)

(defgeneric process-instruciton (client instruction))

(defmethod process-instruction (client instruction)
  (declare (ignore client instruction))
  nil)
