(cl:in-package #:sicl-hir-to-mir)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:fixnum-add-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-signed-add-instruction
   :inputs inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))
