(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (client cst lexical-environment dynamic-environment-ast))

(defgeneric convert-cst (client cst info lexical-environment))

(defgeneric convert-special (client head cst lexical-environment))

(defgeneric convert-special-binding
    (client variable value-ast next-ast lexical-environment))

(defgeneric convert-lambda-call (client cst lexical-environment))

(defgeneric convert-code (lambda-list body-cst lexical-environment client &key block-name-cst))

(defgeneric convert-variable (client cst lexical-environment))

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

(defgeneric convert-function-reference (client cst info lexical-environment))

(defgeneric convert-called-function-reference (client cst info lexical-environment))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (client cst info global-env))

(defgeneric convert-special-variable (client cst info global-env))

(defgeneric convert-setq (client var-cst form-cst info lexical-environment))

(defgeneric convert-setq-special-variable
    (client var-cst form-ast info global-env))

(defgeneric convert-let (client cst lexical-environment))

(defgeneric convert-let* (client cst lexical-environment))
