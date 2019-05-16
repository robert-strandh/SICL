(cl:in-package #:cleavir-cst-to-ast)

;;; ENV is an environment that is known to contain information about
;;; the variable VARIABLE, but we don't know whether it is special or
;;; lexical.  VALUE-AST is an AST that computes the value to be given
;;; to VARIABLE.  NEXT-AST is an AST that represents the computation
;;; to take place after the variable has been given its value.  If the
;;; variable is special, this function creates a BIND-AST with
;;; NEXT-AST as its body.  If the variable is lexical, this function
;;; creates a PROGN-AST with two ASTs in it.  The first one is a
;;; SETQ-AST that assigns the value to the variable, and the second
;;; one is the NEXT-AST.
(defun set-or-bind-variable (variable-cst value-ast next-thunk env system)
  (let ((info (cleavir-env:variable-info env (cst:raw variable-cst))))
    (assert (not (null info)))
    (if (typep info 'cleavir-env:special-variable-info)
        (convert-special-binding
         variable-cst value-ast next-thunk env system)
	(cleavir-ast:make-progn-ast
	 (list (cleavir-ast:make-setq-ast
		(cleavir-env:identity info)
		value-ast
                :origin (cst:source variable-cst))
	       (funcall next-thunk))
         :origin (cst:source variable-cst)))))
