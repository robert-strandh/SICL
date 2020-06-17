(cl:in-package #:cleavir-cst-to-ast)

(defgeneric convert (cst environment system))

(defgeneric convert-cst (cst info environment system))

(defgeneric convert-special (head cst environment system))

(defgeneric convert-special-binding
    (variable value-ast next-ast env system))

(defgeneric convert-lambda-call (cst env system))

(defgeneric convert-code (lambda-list body-cst env system &key block-name-cst origin))

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

(defgeneric convert-function-reference (cst info env system))

(defgeneric convert-called-function-reference (cst info env system))

(defgeneric items-from-parameter-group (parameter-group))

(defgeneric convert-global-function-reference (cst info global-env system))

(defgeneric convert-special-variable (cst info global-env system))

(defgeneric convert-setq (var-cst form-cst info env system))

(defgeneric convert-setq-special-variable
    (var-cst form-ast info global-env system))

(defgeneric convert-let (cst environment system))

(defgeneric convert-let* (cst environment system))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TYPE-WRAP.
;;;
;;; Given an AST and a values ctype, returns a new AST that incorporates
;;; the information that the AST's value will be of that ctype, in some
;;; client-defined fashion. For example it could execute a type check,
;;; or puts in a type declaration (a the-ast), or it could just return
;;; the AST as-is, ignoring the information.
;;; There is a default method that returns the AST as-is.
;;;
;;; KLUDGE: The origin of the given AST should probably be used,
;;; rather than being explicitly passed as an argument, but this will
;;; not be correct for lexical ASTs. Similarly for policies.

(defgeneric type-wrap (ast ctype origin environment system)
  (:method (ast ctype origin environment system)
    (declare (ignore ctype origin environment system))
    ast))
