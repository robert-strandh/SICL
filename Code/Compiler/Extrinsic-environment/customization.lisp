(cl:in-package #:sicl-extrinsic-environment)

;;; Introduce immediates where appropriate.  The definition of the
;;; generic function says that we return an integer if we want to
;;; introduce an immediate value, and NIL if we do not want to
;;; introduce an immediate value.
(defmethod cleavir-generate-ast:convert-constant-to-immediate
    (constant (environment environment))
  (cond ((and (typep constant 'integer)
	      (<= #.(- (expt 2 30)) constant #.(1- (expt 2 30))))
	 (* 2 constant))
	((typep constant 'character)
	 ;; FIXME: Currently, we depend on the host having the same
	 ;; character encoding as the target.
	 (+ #b11 (ash (char-code constant) 3)))
	(t
	 nil)))

;;; When we are asked to compile the name of a global function, by
;;; default Cleavir generates an FDEFINITION-AST taking the function
;;; name as an input.  For SICL, we do not want that.  Instead we want
;;; it to generate an access to the CAR of the global function cell
;;; that contains the function.  And we want the function cell to be
;;; accessed at load time.
(defmethod cleavir-generate-ast:convert-global-function
    ((info cleavir-env:global-function-info) (env environment))
  (cleavir-ast:make-car-ast
   (cleavir-ast:make-load-time-value-ast
    `(sicl-global-environment:function-cell
      ',(cleavir-env:name info)
      sicl-global-environment:*global-environment*)
    ;; The cell is not read-only.
    nil)))

;;; When we are asked to compile the name of a special variable, by
;;; default Cleavir generates a SYMBOL-VALUE-AST taking the variable
;;; name as an input.  For SICL, we do not want that.  Instead we want
;;; it to generate a call to SICL-EXTRINSIC-ENVIRONMENT:SYMBOL-VALUE,
;;; passing it the symbol naming the variable and the environment.
(defmethod cleavir-generate-ast:convert-special-variable
    ((info cleavir-env:special-variable-info) (env environment))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-global-environment:function-cell
       'sicl-extrinsic-environment:symbol-value
       sicl-global-environment:*global-environment*)))
   (list (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t)
	 (cleavir-ast:make-load-time-value-ast
	  'sicl-global-environment:*global-environment*
	  t))))

;;; When we are asked to compile an assignment to a special variable,
;;; by default Cleavir generates a SET-SYMBOL-VALUE-AST taking the
;;; variable name and the value as an input.  For SICL, we do not want
;;; that.  Instead we want it to generate a call to (SETF
;;; SICL-EXTRINSIC-ENVIRONMENT:SYMBOL-VALUE), passing it the new
;;; value, the symbol naming the variable, and the environment.
(defmethod cleavir-generate-ast:convert-setq-special-variable
    ((info cleavir-env:special-variable-info) form-ast (env environment))
  (cleavir-ast:make-call-ast
   (cleavir-ast:make-car-ast
    (cleavir-ast:make-load-time-value-ast
     '(sicl-global-environment:function-cell
       '(setf sicl-extrinsic-environment:symbol-value)
       sicl-global-environment:*global-environment*)))
   (list form-ast
	 (cleavir-ast:make-load-time-value-ast
	  `',(cleavir-env:name info)
	  t)
	 (cleavir-ast:make-load-time-value-ast
	  'sicl-global-environment:*global-environment*
	  nil))))
