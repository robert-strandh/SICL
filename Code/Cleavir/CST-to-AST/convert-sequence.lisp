(cl:in-package #:cleavir-cst-to-ast)

;;; Utility function for converting a sequence of CSTs, represented
;;; as a chain of CONS-CSTs terminated by a NULL-CST.
(defun convert-sequence (csts environment system)
  (loop for cst = csts then (cst:rest cst)
        until (cst:null cst)
	collect (convert (cst:first cst) environment system)))
