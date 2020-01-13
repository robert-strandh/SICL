(cl:in-package #:sicl-hir-transformations)

(defgeneric preprocess-unwind-instruction (instruction))

(defmethod preprocess-unwind-instruction
    ((instruction t))
  nil)

(defmethod preprocess-unwind-instruction
    ((instruction cleavir-ir:unwind-instruction))
  (let* ((catch-instruction (cleavir-ir:destination instruction))
         (index (cleavir-ir:unwind-index instruction))
         (successors (cleavir-ir:successors catch-instruction))
         (successor (nth index successors)))
    (setf (cleavir-ir:successors instruction)
          (list successor))))

(defun preprocess-unwind-instructions
    (top-level-enter-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   #'preprocess-unwind-instruction top-level-enter-instruction))

