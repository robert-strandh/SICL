(cl:in-package #:cleavir-cst)

;;; Return true if and only if the concrete syntax tree CST represents
;;; a DECLARE expression, i.e., that the expression represented is a
;;; CONS with the symbol DECLARE in the CAR slot.  No attempt is made
;;; to verify the syntax of the declare expression.
(defun cst-is-declaration-p (cst)
  (let ((expression (expression cst)))
    (and (consp expression)
	 (eq (car expression) 'declare))))
