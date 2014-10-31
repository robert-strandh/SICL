(cl:in-package #:sicl-hir-to-mir)

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:eq-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (cleavir-ir:make-equal-instruction
   :inputs (cleavir-ir:inputs instruction)
   :successors (cleavir-ir:successors instruction)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:car-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value -1)))
    (cleavir-ir:make-memref2-instruction cons immediate successor)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:cdr-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value 7)))
    (cleavir-ir:make-memref2-instruction cons immediate successor)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:rplaca-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (object (second (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value -1)))
    (cleavir-ir:make-memset2-instruction cons immediate object successor)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:rplacd-instruction)
     (implementation sicl-target-sicl:sicl)
     (processor cleavir-processor-x86-64:x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (object (second (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value 7)))
    (cleavir-ir:make-memset2-instruction cons immediate object successor)))
