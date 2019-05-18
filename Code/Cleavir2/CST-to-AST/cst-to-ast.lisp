(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initform *origin* :initarg :origin :reader origin)))

(defun cst-to-ast (client cst lexical-environment)
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert client cst lexical-environment)))
