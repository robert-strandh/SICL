(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-COMPILER-MACRO.

(defmacro define-compiler-macro (name lambda-list &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (compiler-macro-function ',name)
	   ,(cleavir-code-utilities:parse-macro
	     name
	     lambda-list
	     body))))
