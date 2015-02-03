(cl:in-package #:sicl-hir-to-mir)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:enter-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  instruction)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:funcall-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  instruction)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:tailcall-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  instruction)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:return-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  instruction)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:assignment-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  instruction)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:eq-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-equal-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))
