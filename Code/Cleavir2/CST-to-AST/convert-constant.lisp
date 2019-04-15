(cl:in-package #:cleavir-cst-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.
;;;
;;; The result of the conversion can be either an IMMEDIATE-AST if the
;;; constant is such that it can be represented as an immediate value
;;; in the resulting machine code, or it can be a LOAD-TIME-VALUE-AST
;;; if it can not be represented as an immediate.
;;;
;;; Obviously, Cleavir can not know a priori neither whether the
;;; constant can be represented as an immediate value, nor can it know
;;; in that case what the corresponding immediate representation is.
;;; For that reason, we first call the generic function named
;;; CONVERT-CONSTANT-TO-IMMEDIATE with the constant value and the
;;; environment.  The function CONVERT-CONSTANT-TO-IMMEDIATE may
;;; return NIL, meaning that this constant does not have a
;;; representation as an immediate value, or it may return a
;;; possibly-negative integer which is taken to be the representation
;;; of the constant as an immediate machine word.  A default method is
;;; provided that always returns NIL.

(defgeneric convert-constant-to-immediate (constant env system))

(defmethod convert-constant-to-immediate (constant env system)
  (declare (ignore constant env system))
  nil)

(defun convert-constant (constant-cst env system)
  (let* ((global-env (cleavir-env:global-environment env))
         (expression (cst:raw constant-cst))
         (origin (cst:source constant-cst))
	 (immediate (convert-constant-to-immediate
                     expression global-env system)))
    (if (null immediate)
	(cleavir-ast:make-load-time-value-ast `',expression t :origin origin)
	(cleavir-ast:make-immediate-ast immediate :origin origin))))
