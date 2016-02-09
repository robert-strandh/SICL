(cl:in-package #:cleavir-cst)

;;; Return true if and only if the concrete syntax tree CST represents
;;; a DECLARE expression, i.e., that the expression represented is a
;;; CONS with the symbol DECLARE in the CAR slot.  No attempt is made
;;; to verify the syntax of the declare expression.
(defun cst-is-declaration-p (cst)
  (let ((expression (expression cst)))
    (and (consp expression)
	 (eq (car expression) 'declare))))

;;; Return true if and only if the concrete syntax tree CST represents
;;; a literal string.
(defun cst-is-literal-string-p (cst)
  (stringp (expression cst)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Separate a concrete syntax tree representing a function body (that
;;; may contain declarations and/or a documentation string) into three
;;; things:
;;;
;;;   * a list of concrete syntax trees representing the
;;;     declarations,
;;;
;;;   * a concrete syntax tree representing a literal documentation
;;;     string, or NIL if there is no documentation string in the
;;;     body, and
;;;
;;;   * a list of concrete syntax trees representing the executable
;;;     forms.
;;;
;;; Return the result as three values.
;;;
;;; If there are declarations after the first executable form (which
;;; is a syntax error), then those declarations will be considered
;;; part of the executable forms.
;;;
;;; If there is a literal string after the first executable form, then
;;; that string is also considered an executable form.
;;;
;;; We assume that it has already been checked that the represented
;;; body is a proper list.

(defun separate-function-body (body-cst)
  (let ((declaration-csts '())
	(documentation-cst nil)
	(form-csts '())
	(children (children body-cst)))
    (loop for (expr-cst . rest-csts) on children
	  do (cond ((not (null form-csts))
		    (push expr-cst form-csts))
		   ((cst-is-declaration-p expr-cst)
		    (push expr-cst declaration-csts))
		   ((cst-is-literal-string-p expr-cst)
		    (if (or (null rest-csts)
			    (not (null documentation-cst))
			    (not (null form-csts)))
			(push expr-cst form-csts)
			(setf documentation-cst expr-cst)))
		   (t
		    (push expr-cst form-csts))))
    (values (nreverse declaration-csts)
	    documentation-cst
	    (nreverse form-csts))))
