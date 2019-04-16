(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initarg :origin :reader origin)))

;;; During the conversion of a single CST, the value of this variable
;;; is the source location of that CST.
(defvar *origin*)

(defun cst-to-ast (cst environment system
                       &optional (cleavir-ast:*dynamic-environment*
                                  (cleavir-ast:make-dynamic-environment-ast
                                   '#:unused-dynamic-environment
                                   :policy (cleavir-env:environment-policy
                                            environment))))
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert cst environment system)))
