(cl:in-package #:sicl-mir-to-lir)

(defgeneric mir-to-lir (client mir))

(defun enclose-funcall-p (instruction)
  (and (typep instruction 'cleavir-ir:funcall-instruction)
       (>= (length (cleavir-ir:inputs instruction)) 2)
       (typep (second (cleavir-ir:inputs instruction))
              'sicl-hir-to-mir::entry-point-input)))

(defmethod mir-to-lir (client mir)
  (let ((worklist (list mir))
        (all-instructions '()))
    (loop until (null worklist)
          do (cleavir-ir:map-instructions-arbitrary-order
              (lambda (instruction)
                (push instruction all-instructions)
                (when (enclose-funcall-p instruction)
                  (push (sicl-hir-to-mir::enter-instruction
                         (second (cleavir-ir:inputs instruction)))
                        worklist)))
              (pop worklist)))
    (length all-instructions)))
