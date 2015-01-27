(cl:in-package #:cleavir-hir-transformations)

;;;; We must let the implementation determine how to handle the
;;;; FDEFINITION-INSTRUCTION.  Some implementations might have two
;;;; slots in each symbol (one for the function named SYMBOL and
;;;; another one for the function named (SETF SYMBOL)).  Such
;;;; implementations can probably leave the FDEFINITION-INSTRUCTION
;;;; intact until HIR gets translated to MIR.  If it is left intact,
;;;; then the constant input for the name of the function will be
;;;; turned into a LOAD-TIME-VALUE input in subsequent transformations. 
;;;;
;;;; An implementation with first-class global environments, such as
;;;; SICL, should transform the the FDEFINITION-INSTRUCTION into an
;;;; access of a CELL containing the function.  For SICL, since the
;;;; CELL is an ordinary CONS cell, the access will be a
;;;; CAR-INSTRUCTION, and the input will be a LOAD-TIME-VALUE-INPUT
;;;; where the FORM queries the link-time environment for the cell
;;;; corresponding to the function name.

(defgeneric process-fdefinition (fdefinition implementation processor os))

(defmethod process-fdefinition (fdefinition implementation processor os)
  (declare (ignore fdefinition implementation processor os))
  nil)

(defun process-fdefinitions (initial-instruction implementation processor os)
  (traverse
   initial-instruction
   (lambda (instruction owner)
     (declare (ignore owner))
     (when (typep instruction 'cleavir-ir:fdefinition-instruction)
       (process-fdefinition instruction implementation processor os)))))
