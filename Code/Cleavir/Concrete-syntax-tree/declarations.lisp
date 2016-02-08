(cl:in-package #:cleavir-cst)

;;; CSTS is a list of concrete syntax trees, each one representing a
;;; DECLARE EXPRESSION, i.e., representing an expression of the form
;;; (DECLARE declaration-specifier*)
;;;
;;; Return a single concrete syntax tree, representing a single
;;; declare expression, equivalent to the declare expressions
;;; represented by the concrete syntax trees in the argument.
;;;
;;; The source location of the return value is set to the source
;;; location of the first concrete syntax tree in CSTS.  The source
;;; location of each declaration specifier is preserved.
(defun normalize-declarations (csts)
  (let (;; This variable holds the CST representing the symbol
	;; DECLARE of the first CST in CSTs.
	(declare-cst (first (children (first csts))))
	(specifier-csts (loop for cst in csts
			      append (rest (children cst)))))
    (make-instance 'cst
      :expression (cons 'declare (mapcar #'expression specifier-csts))
      :location (location (first csts))
      :children (cons declare-cst specifier-csts))))
