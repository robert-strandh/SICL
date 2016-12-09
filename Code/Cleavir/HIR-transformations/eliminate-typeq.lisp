(cl:in-package #:cleavir-hir-transformations)

(defgeneric maybe-eliminate (instruction))

(defmethod maybe-eliminate (instruction)
  (declare (ignore instruction))
  nil)

(defmethod maybe-eliminate ((instruction cleavir-ir:typeq-instruction))
  (let* ((cleavir-ir:*policy* (cleavir-ir:policy instruction))
	 (object (first (cleavir-ir:inputs instruction)))
	 (typep-constant (cleavir-ir:make-constant-input 'typep))
	 (typep-function (cleavir-ir:new-temporary))
	 (fdefinition (cleavir-ir:make-fdefinition-instruction
		       typep-constant typep-function))
	 (type (cleavir-ir:value-type instruction))
	 (type-descriptor-constant (cleavir-ir:make-constant-input type))
	 (values-location (cleavir-ir:make-values-location))
	 (boolean-value (cleavir-ir:new-temporary))
	 (call (cleavir-ir:make-funcall-instruction
		(list typep-function object type-descriptor-constant)
		(list values-location)))
	 (mtf (cleavir-ir:make-multiple-to-fixed-instruction
	       values-location
	       (list boolean-value)))
	 (nil-constant (cleavir-ir:make-constant-input 'nil)))
    (cond ((and (subtypep type 'fixnum) (subtypep 'fixnum type))
	   (change-class instruction 'cleavir-ir:fixnump-instruction))
	  ((and (subtypep type 'cons) (subtypep 'cons type))
	   (change-class instruction 'cleavir-ir:consp-instruction))
	  (t
	   (cleavir-ir:insert-instruction-before fdefinition instruction)
	   (cleavir-ir:insert-instruction-before call instruction)
	   (cleavir-ir:insert-instruction-before mtf instruction)
	   (change-class instruction 'cleavir-ir:eq-instruction
			 :inputs (list boolean-value nil-constant)
			 :successors
			 (reverse (cleavir-ir:successors instruction)))))))

(defun eliminate-typeq (initial-instruction)
  (cleavir-ir:map-instructions #'maybe-eliminate initial-instruction))
