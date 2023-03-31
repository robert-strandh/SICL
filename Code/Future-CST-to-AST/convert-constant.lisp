(cl:in-package #:sicl-expression-to-ast)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CONVERT-CONSTANT is called when a constant is found, either in the
;;; form of a literal or in the form of a constant variable.

(defun convert-literal (client literal environment)
  (declare (ignore client environment))
  (make-instance 'ico:literal-ast :literal literal))

(defun convert-constant (client cooked-constant environment)
  (convert-literal client (c:raw cooked-constant) environment))
