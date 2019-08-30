(cl:in-package #:sicl-hir-to-cl)

(defgeneric translate-final-instruction (client instruction context))

(defmethod translate-final-instruction :around (client instruction context)
  (let ((origin (cleavir-ast-to-hir:origin instruction)))
    (if (null origin)
        (call-next-method)
        (cons `(setq source (compute-source-info source ',origin))
              (call-next-method)))))

;;; Default method on TRANSLATE-FINAL-INSTRUCTION.  It is used when
;;; the final instruction is a simple instruction with a single
;;; successor that happened to be the final instruction of a basic
;;; block because its single successor is the LEADER of a basic block.
(defmethod translate-final-instruction (client instruction context)
  (let* ((successor (first (cleavir-ir:successors instruction)))
         (successor-block (basic-block-of-leader successor))
         (tag (tag-of-basic-block successor-block)))
    (append (translate client instruction context)
            `((go ,tag)))))

(defmethod translate-final-instruction
    (client (instruction cleavir-ir:return-instruction) context)
  (let* ((values-location (first (cleavir-ir:inputs instruction)))
         (name (gethash values-location (values-locations context))))
    `((return-from ,(block-name context)
        (apply #'values ,name)))))

(defun translate-basic-block (client basic-block context)
  (let* ((instructions (instructions basic-block))
         (final-instruction (first (last instructions))))
    (append (loop for instruction in (butlast instructions)
                  append (translate client instruction context))
            (translate-final-instruction client
                                         final-instruction
                                         context))))
