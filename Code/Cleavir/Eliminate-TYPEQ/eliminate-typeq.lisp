(cl:in-package #:cleavir-eliminate-typeq)

(defvar *table*)

(defgeneric eliminate (instruction))

(defun eliminate-typeq (initial-instruction)
  (let ((*table* (make-hash-table :test #'eq)))
    (eliminate initial-instruction)))

(defmethod eliminate (instruction)
  (declare (ignore instruction))
  nil)

(defmethod eliminate :around (instruction)
  (unless (gethash instruction *table*)
    (setf (gethash instruction *table*) t)
    (call-next-method)
    (mapc #'eliminate (cleavir-ir:successors instruction))))

(defmethod eliminate :after ((instruction cleavir-ir:enclose-instruction))
  (eliminate (cleavir-ir:code instruction)))
