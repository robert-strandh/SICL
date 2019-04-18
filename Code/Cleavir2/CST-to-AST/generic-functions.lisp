(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (cst environment client))

(defgeneric convert-cst (cst info environment client))

(defgeneric convert-special (head cst environment client))

(defgeneric convert-special-binding
    (variable value-ast next-ast env client))

(defgeneric convert-lambda-call (cst env client))

(defgeneric convert-code (lambda-list body-cst env client &key block-name-cst))

(defgeneric convert-variable (cst environment client))

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
;;; particular client defined by that client code.

(defgeneric convert-function-reference (cst info env client))

(defgeneric convert-called-function-reference (cst info env client))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (cst info global-env client))

(defgeneric convert-special-variable (cst info global-env client))

(defgeneric convert-setq (var-cst form-cst info env client))

(defgeneric convert-setq-special-variable
    (var-cst form-ast info global-env client))

(defgeneric convert-let (cst environment client))

(defgeneric convert-let* (cst environment client))
