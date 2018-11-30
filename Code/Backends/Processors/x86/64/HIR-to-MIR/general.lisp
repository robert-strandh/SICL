(cl:in-package #:sicl-hir-to-mir)

(defgeneric translate (client instruction))

(defun clone-instruction (instruction)
  (make-instance (class-of instruction)
    :inputs (cleavir-ir:inputs instruction)
    :outputs (cleavir-ir:outputs instruction)
    :successors (cleavir-ir:successors instruction)))

(defvar *translation-table*)

(defmethod translate ((client sicl-client:sicl-x86-64) instruction)
  (clone-instruction instruction))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:enter-instruction))
  (clone-instruction instruction))

(defmethod translate ((client sicl-client:sicl-x86-64)
                      (instruction cleavir-ir:eq-instruction))
  (clone-instruction instruction))

(defmethod translate :around ((client sicl-client:sicl-x86-64) instruction)
  (when (null (gethash instruction *translation-table*))
    (let* ((cleavir-ir:*origin* (cleavir-ir:origin instruction))
           (cleavir-ir:*policy* (cleavir-ir:policy instruction))
           (translation (call-next-method)))
      (setf (gethash instruction *translation-table*) translation))))
