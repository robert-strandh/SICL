(cl:in-package #:cleavir-remove-useless-instructions)

(defgeneric instruction-may-be-removed-p (instruction))

(defmethod instruction-may-be-removed-p (instruction) t)

(defmethod instruction-may-be-removed-p ((instruction cleavir-ir:eq-instruction)) nil)

(defmethod instruction-may-be-removed-p ((instruction cleavir-ir:catch-instruction)) t)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:side-effect-mixin))
  nil)

(defmethod instruction-may-be-removed-p
    ((instruction cleavir-ir:enter-instruction))
  nil)

(defgeneric remove-instruction (instruction))

(defmethod remove-instruction (instruction)
  (error "remove-instruction is not implemented for this type of instruction."))

(defmethod remove-instruction ((instruction cleavir-ir:one-successor-mixin))
  (cleavir-ir:delete-instruction instruction))

;; We can eliminate catch instructions when the continuation is unused.
(defmethod remove-instruction ((instruction cleavir-ir:catch-instruction))
  (cleavir-ir:bypass-instruction (first (cleavir-ir:successors instruction))
                                 instruction))

(defun remove-useless-instructions-with-worklist (initial-instruction worklist)
   (let ((worklist worklist))
    (loop (when (null worklist) (return))
          (let ((instruction (pop worklist)))
            (when (and (instruction-may-be-removed-p instruction)
                       (every (lambda (output)
                                (null (cleavir-ir:using-instructions output)))
                              (cleavir-ir:outputs instruction)))
              (dolist (output (cleavir-ir:outputs instruction))
                (setf (cleavir-ir:defining-instructions output)
                      (remove instruction (cleavir-ir:defining-instructions output))))
              (dolist (input (cleavir-ir:inputs instruction))
                (setf (cleavir-ir:using-instructions input)
                      (remove instruction (cleavir-ir:using-instructions input)))
                (dolist (defining-instruction (cleavir-ir:defining-instructions input))
                  (push defining-instruction worklist)))
              (remove-instruction instruction))))))

(defun remove-useless-instructions (initial-instruction)
  (remove-useless-instructions-with-worklist initial-instruction
                                             (cleavir-ir:instructions-of-type initial-instruction t)))

;;; An incremental version of remove-useless-instructions meant for
;;; clients which know where in the graph a potentially useless
;;; instruction has appeared.
(defun remove-useless-instructions-from (initial-instruction instruction)
  (remove-useless-instructions-with-worklist initial-instruction
                                             (list instruction)))
