(cl:in-package #:sicl-extrinsic-file-compiler)

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
	 (+ #b11 (* 4 (char-code constant))))
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
