(cl:in-package #:sicl-hir-to-mir)

(defclass sicl () ())

(defclass x86-64 () ())

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:car-instruction)
     (implementation sicl)
     (processor x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value -1)))
    (cleavir-ir:make-memref2-instruction cons immediate successor)))
