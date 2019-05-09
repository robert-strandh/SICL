(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate-final-instruction (client instruction context))

(defun compute-dynamic-environment-pops (successor)
  (let* ((dynamic-environment-location
           (cleavir-ir:dynamic-environment-location successor))
         (pos (position dynamic-environment-location
                        *dynamic-environment-stack*)))
    (loop repeat pos
          collect `(pop *dynamic-environment*))))

;;; Default method on TRANSLATE-FINAL-INSTRUCTION.  It is used when
;;; the final instruction is a simple instruction with a single
;;; successor that happened to be the final instruction of a basic
;;; block because its single successor (which must be the LEADER of a
;;; basic block) has multiple predecessors.
(defmethod translate-final-instruction (client instruction context)
  (let* ((successor (first (cleavir-ir:successors instruction)))
         (successor-block (basic-block-of-leader successor))
         (tag (tag-of-basic-block successor-block)))
    (append (translate client instruction context)
            (compute-dynamic-environment-pops successor)
            `((go ,tag)))))

(defmethod translate-final-instruction (client
                                       (instruction cleavir-ir:return-instruction)
                                       context)
  `((return-from ,(block-name context)
      (apply #'values ,(values-location context)))))

(defun translate-basic-block (client basic-block context)
  (let* ((instructions (instructions basic-block))
         (final-instruction (first (last instructions))))
    (append (loop for instruction in (butlast instructions)
                  append (translate client instruction context))
            (translate-final-instruction client
                                         final-instruction
                                         context))))
