(cl:in-package #:cleavir-cst-to-ast)

;;; This variable should be bound by client code to one of the symbols
;;; CL:COMPILE, CL:COMPILE-FILE, or CL:EVAL before the main entry
;;; point is called.
(defvar *compiler*)

(defun cst-to-ast (cst environment system)
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert cst environment system)))
