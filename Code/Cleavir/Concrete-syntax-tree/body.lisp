(cl:in-package #:cleavir-cst)

;;; Return true if and only if the concrete syntax tree CST represents
;;; a DECLARE expression, i.e., that the expression represented is a
;;; CONS with the symbol DECLARE in the CAR slot.  No attempt is made
;;; to verify the syntax of the declare expression.
(defun cst-is-declaration-p (cst)
  (let ((expression (expression cst)))
    (and (consp expression)
	 (eq (car expression) 'declare))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate a concrete syntax tree representing an ordinary body such
;;; as a let or let* body that may contain declarations (but no
;;; documentation) into two lists; a list of concrete syntax trees
;;; representing the declarations and a list of concrete syntax trees
;;; representing the executable forms.  Return the two lists as two
;;; values.
;;;
;;; If there are declarations after the first executable form (which
;;; is a syntax error), then those declarations will be considered
;;; part of the executable forms.
;;;
;;; We assume that it has already been checked that the represented
;;; body is a proper list.

(defun separate-ordinary-body (body-cst)
  (let* ((children (children body-cst))
	 ;; If there is an expression in the body that is NOT a
	 ;; DECLARE expression, then find its position.
	 (pos (position-if-not #'cst-is-declaration-p children)))
    (if (null pos)
	;; If POS is NIL, then all the expressions in the body are
	;; DECLARE expressions.
	(values children '())
	(values (subseq children 0 pos) (subseq children pos)))))
