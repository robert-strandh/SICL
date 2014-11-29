(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macro for macro MULTIPLE-VALUE-BIND.

(define-compiler-macro multiple-value-bind
    (&whole form variables values-form &body body)
  (declare (ignore values-form body))
  (if (not (cleavir-code-utilities:proper-list-p variables))
      (error 'variables-must-be-proper-list :variables variables)
      (let ((non-symbol (find-if-not #'symbolp variables)))
	(if (not (null non-symbol))
	    (error 'variable-must-be-symbol :variable non-symbol)
	    form))))
