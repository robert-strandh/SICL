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

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:cdr-instruction)
     (implementation sicl)
     (processor x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value 7)))
    (cleavir-ir:make-memref2-instruction cons immediate successor)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:rplaca-instruction)
     (implementation sicl)
     (processor x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (object (second (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value -1)))
    (cleavir-ir:make-memset2-instruction cons immediate object successor)))

(defmethod cleavir-ir:specialize
    ((instruction cleavir-ir:rplacd-instruction)
     (implementation sicl)
     (processor x86-64)
     os)
  (declare (ignore os))
  (let* ((successor (first (cleavir-ir:successors instruction)))
	 (cons (first (cleavir-ir:inputs instruction)))
	 (object (second (cleavir-ir:inputs instruction)))
	 (immediate (make-instance 'cleavir-ir:immediate-input :value 7)))
    (cleavir-ir:make-memset2-instruction cons immediate object successor)))
