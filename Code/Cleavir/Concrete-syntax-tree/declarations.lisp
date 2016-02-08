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

;;; A canonicalized declaration specifier is one of following:
;;;
;;;   * (declaration name)
;;;   * (dynamic-extent var)
;;;   * (dynamic-extent (function fn))
;;;   * (ftype type function-name)
;;;   * (ignore var)
;;;   * (ignore (function fn))
;;;   * (ignorable var)
;;;   * (ignorable (function fn))
;;;   * (inline function-name)
;;;   * (notinline function-name)
;;;   * (optimize (quality value))
;;;   * (special var)
;;;   * (type typespec var)

;;; Canonicalize a concrete syntax tree representing a single
;;; declaration specifier.
;;;
;;; Each declaration specifier is divided up so that each one concerns
;;; a single entity (name, variable, function name, quality).
;;;
;;; If the input represents an empty list of entities, then the empty
;;; list is returned.
;;;
;;; CST must represent a declaration specifier with an explicit
;;; declaration identifier that is not implementation specific.
;;;
;;; Return a list of concrete syntax trees, each representing a
;;; canonicalized declaration specifier.  Each concrete syntax tree in
;;; the returned list has the same first child which is the first
;;; child of CST.  When the second child is a type specifier, then
;;; that child is also shared among all the return values.  The
;;; location of each of the concrete syntax trees in the returned list
;;; has the same source location as CST.
;;;
;;; We assume that the syntax of the input has been checked.

(defun canonicalize-declaration-specifier (cst)
  (destructuring-bind (identifier-cst . argument-csts) (children cst)
    (let ((identifier (expression identifier-cst)))
      (if (member identifier '(type ftype))
	  (destructuring-bind (type-cst . name-csts) argument-csts
	    (loop with type = (expression type-cst)
		  for name-cst in name-csts
		  for name = (expression name-cst)
		  collect (make-instance 'cst
			    :expression (list identifier type name)
			    :location (location cst)
			    :children (list identifier-cst type-cst name-cst))))
	  (loop for name-cst in argument-csts
		for name = (expression name-cst)
		collect (make-instance 'cst
			  :expression (list identifier name)
			  :location (location cst)
			  :children (list identifier-cst name-cst)))))))
