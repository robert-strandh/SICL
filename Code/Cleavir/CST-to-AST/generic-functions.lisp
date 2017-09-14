(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (cst environment system))

(defgeneric convert-cst (cst info environment system))

(defgeneric convert-special (head cst environment system))

(defgeneric convert-special-binding
    (variable value-ast next-ast env system))

(defgeneric convert-lambda-call (cst env system))

(defgeneric convert-code (lambda-list body-cst env system &optional block-name))

(defgeneric convert-variable (cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONVERT-FUNCTION-REFERENCE.
;;;
;;; This generic function converts a reference to a function given by
;;; a description in the form of an INFO instance.  That INFO instance
;;; was obtained as a result of a form such as (FUNCTION
;;; FUNCTION-NAME) by calling FUNCTION-INFO with an environment and
;;; the FUNCTION-NAME argument.  The function signals an error if the
;;; INFO instance is not a GLOBAL-FUNCTION-INFO or a
;;; LOCAL-FUNCTION-INFO.  Client code can override the default
;;; behavior by adding methods to this function, specialized to the
;;; particular system defined by that client code.

(defgeneric convert-function-reference (info env system))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (info global-env system))

(defgeneric convert-special-variable (cst info global-env system))

(defgeneric convert-setq (var-cst form-cst info env system))

(defgeneric convert-setq-special-variable
    (var-cst form-ast info global-env system))

(defgeneric convert-let (cst environment system))
