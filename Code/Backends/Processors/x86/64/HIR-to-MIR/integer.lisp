(cl:in-package #:sicl-hir-to-mir)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:fixnum-add-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-signed-add-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:fixnum-sub-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-signed-sub-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:fixnum-less-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-signed-less-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:fixnum-not-greater-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-signed-not-greater-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:fixnum-equal-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-equal-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))
