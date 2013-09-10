(in-package #:sicl-compiler-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro DEFINE-COMPILER-MACRO.

(defmacro define-compiler-macro (name lambda-list &body body)
  `(eval-when (:compile-toplevel)
     (funcall #'(setf compiler-macro-function)
	      (function ,(sicl-code-utilities:parse-macro
			  name
			  lambda-list
			  body))
	      ',name)))


