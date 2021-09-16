(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (client cst environment))

(defgeneric convert-cst (client cst info environment))

(defgeneric convert-special (client head cst environment))

(defgeneric convert-special-binding
    (client variable value-ast next-ast environment))

(defgeneric convert-lambda-call (client cst environment))

(defgeneric convert-code
    (client lambda-list body-cst environment &key block-name-cst))

(defgeneric convert-variable (client cst environment))

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

(defgeneric convert-function-reference (client cst info environment))

(defgeneric convert-called-function-reference (client cst info environment))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (client cst info global-env))

(defgeneric convert-special-variable (client cst info global-env))

(defgeneric convert-setq (client var-cst form-cst info environment))

(defgeneric convert-setq-special-variable
    (client var-cst form-ast info global-env))

(defgeneric convert-let (client cst environment))

(defgeneric convert-let* (client cst environment))

;;; This function returns the contents of the ORIGIN slot of an AST.
(defgeneric origin (ast))

;;; Returns whether OBJECT is so simple that we never need to hoist it.
(defgeneric trivial-constant-p (client object))

;;; Returns up to two values, where the first value is a list of equal
;;; keys, and where the second value is a list of equalp keys.
;;;
;;; Two objects o1 and o2 are considered similar if their equal keys have a
;;; common element in the sense of EQUAL, or if their equalp keys have a
;;; common element in the sense of EQUALP.  Clients that want to write
;;; additional methods for this function are encouraged to have a look at
;;; the auxiliary functions EQUAL-REPRESENTATION and EQUALP-REPRESENTATION.
(defgeneric similarity-keys (client literal-object))

;;; Similar to CL:MAKE-LOAD-FORM, but with an additional client argument.
;;; Another difference to CL:MAKE-LOAD-FORM is that OBJECT is not
;;; necessarily a generalized instance of STANDARD-OBJECT,
;;; STRUCTURE-OBJECT, or CONDITION.
(defgeneric make-load-form-using-client (client object environment))

;;; This generic function is called by CST-to-AST in order to obtain a
;;; list of symbols that have been given as a DECLARATION proclamation
;;; in the global environment.  The default method returns the empty
;;; list, but client code can specialize the CLIENT and/or the
;;; ENVIRONMENT parameter so that the global environment is consulted
;;; for this information.
(defgeneric declaration-proclamations (client environment))
