(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate-last-instruction
    (client instruction context dynamic-environment-stack))

(defun compute-dynamic-environment-pops (successor dynamic-environment-stack)
  (let* ((dynamic-environment-location
           (cleavir-ir:dynamic-environment-location successor))
         (pos (position dynamic-environment-location
                        dynamic-environment-stack)))
    (loop repeat pos
          collect `(pop *dynamic-environment*))))

;;; Default method on TRANSLATE-LAST-INSTRUCTION.  It is used when the
;;; last instruction is a simple instruction with a single successor
;;; that happened to be the last instruction of a basic block because
;;; its single successor (which must be the LEADER of a basic block)
;;; has multiple predecessors.
(defmethod translate-last-instruction
    (client instruction context dynamic-environment-stack)
  (let* ((successor (first (cleavir-ir:successors instruction)))
         (successor-block (basic-block-of-leader successor))
         (tag (tag-of-basic-block successor-block)))
    (append (translate client instruction context)
            (compute-dynamic-environment-pops successor
                                              dynamic-environment-stack)
            `((go ,tag)))))

(defun translate-basic-block
    (client basic-block context dynamic-environment-stack)
  (let* ((instructions (instructions basic-block))
         (last-instruction (first (last instructions))))
    (append (loop for instruction in (butlast instructions)
                  append (translate client instruction context))
            (translate-last-instruction client
                                        last-instruction
                                        context
                                        dynamic-environment-stack))))
