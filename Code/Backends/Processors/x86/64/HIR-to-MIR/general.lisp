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
    (let ((translation (call-next-method)))
      (setf (gethash instruction *translation-table*) translation))))

(defgeneric hir-to-mir (client initial-instruction))

(defmethod hir-to-mir ((client sicl-client:sicl-x86-64) initial-instruction)
  (let ((*translation-table* (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction) (translate client instruction))
     initial-instruction)))
