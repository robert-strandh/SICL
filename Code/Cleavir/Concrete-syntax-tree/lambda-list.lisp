(cl:in-package #:cleavir-cst)

(defclass optional-or-keyword--parameter ()
  (;; This slot contains the CST for the obligatory variable of the
   ;; parameter.
   (%variable-cst :initarg :variable-cst :reader variable-cst)
   ;; This slot contains a CST for the INIT-FORM of the parameter.  If
   ;; no INIT-FORM was supplied, then this slot contains a CST for
   ;; which both EXPRESSION and LOCATION return NIL.
   (%init-form-cst :initarg :init-form-cst :reader init-form-cst)
   ;; This slot contains a CST for the SUPPLIED-P-PARAMETER of the
   ;; parameter.  If no SUPPLIED-P-PARAMETER was given, then this slot
   ;; contains NIL.
   (%supplied-p-parameter-cst :initarg :supplied-p-parameter-cst
			      :reader supplied-p-parameter-cst)))

(defclass optional-parameter (optional-or-keyword-parameter)
  ())

(defclass optional-parameters ()
  (;; This slot contains a CST for the lambda-list keyword.
   ;; Typically, this keyword will be &OPTIONAL, but client code may
   ;; define other lambda-list keywords that work the same way as
   ;; &OPTIONAL.
   (%keyword-cst :initarg :keyword-cst :reader keyword-cst)
   ;; This slot contains a list of instances of the class
   ;; OPTIONAL-PARAMETER.
   (%parameters :initarg :parameters :reader parameters)))
