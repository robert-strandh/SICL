(cl:in-package #:cleavir-cst-to-ast)

(stealth-mixin:define-stealth-mixin ast-mixin () cleavir-ast:ast
  ((%origin :initarg :origin :reader origin)))

;;; During the conversion of a single CST, the value of this variable
;;; is the source location of that CST.
(defvar *origin*)

(defun cst-to-ast (client cst lexical-environment)
  (let ((*subforms-are-top-level-p* t)
	(*compile-time-too* nil))
    (convert client cst lexical-environment nil)))

(defmethod initialize-instance :after ((object ast-mixin) &key)
  (reinitialize-instance object :origin *origin*))
