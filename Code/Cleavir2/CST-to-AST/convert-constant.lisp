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

(defgeneric convert-constant-to-immediate (client constant lexical-environment))

(defmethod convert-constant-to-immediate (client constant lexical-environment)
  (declare (ignore constant lexical-environment client))
  nil)

(defun convert-constant (client constant-cst lexical-environment)
  (let* ((global-env (cleavir-env:global-environment lexical-environment))
         (expression (cst:raw constant-cst))
	 (immediate (convert-constant-to-immediate
                     client expression global-env)))
    (if (null immediate)
	(make-instance 'cleavir-ast:load-time-value-ast
          :form `',expression
          :read-only-p t)
	(make-instance 'cleavir-ast:immediate-ast
          :value immediate))))
