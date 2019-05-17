(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initform *origin* :initarg :origin :reader origin)))

(defun cst-to-ast (client cst lexical-environment)
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil)
        (dynamic-environment-input-ast
          (make-instance 'cleavir-ast:lexical-ast
            :name '#:dummy
            :dynamic-environment-input-ast 'do-not-use)))
    (convert client cst lexical-environment dynamic-environment-input-ast)))
