(cl:in-package #:cleavir-hir-transformations)

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

(defmethod eliminate ((instruction cleavir-ir:typeq-instruction))
  (let* ((object (first (cleavir-ir:inputs instruction)))
	 (typep-constant (cleavir-ir:make-constant-input 'typep))
	 (typep-function (cleavir-ir:new-temporary))
	 (fdefinition (cleavir-ir:make-fdefinition-instruction
		       typep-constant typep-function))
	 (type (cleavir-ir:value-type instruction))
	 (type-descriptor-constant (cleavir-ir:make-constant-input type))
	 (boolean-value (cleavir-ir:new-temporary))
	 (call (cleavir-ir:make-funcall-instruction
		(list typep-function object type-descriptor-constant)
		(list boolean-value)))
	 (nil-constant (cleavir-ir:make-constant-input 'nil)))
    (cleavir-ir:insert-instruction-before fdefinition instruction)
    (cleavir-ir:insert-instruction-before call instruction)
    (change-class instruction 'cleavir-ir:eq-instruction
		  :inputs (list boolean-value nil-constant)
		  :successors (reverse (cleavir-ir:successors instruction)))))
