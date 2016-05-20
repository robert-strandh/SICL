(cl:in-package #:cleavir-cst)

(defclass parameter ()
  (;; This slot contains the CST for the obligatory variable of the
   ;; parameter.
   (%variable-cst :initarg :variable-cst :reader variable-cst)))

(defclass required-parameter (parameter)
  ())

(defclass facultative-parameter (parameter)
  (;; This slot contains a CST for the INIT-FORM of the parameter.  If
   ;; no INIT-FORM was supplied, then this slot contains a CST for
   ;; which both EXPRESSION and LOCATION return NIL.
   (%init-form-cst :initarg :init-form-cst :reader init-form-cst)
   ;; This slot contains a CST for the SUPPLIED-P-PARAMETER of the
   ;; parameter.  If no SUPPLIED-P-PARAMETER was given, then this slot
   ;; contains NIL.
   (%supplied-p-parameter-cst :initarg :supplied-p-parameter-cst
			      :reader supplied-p-parameter-cst)))

(defclass optional-parameter (facultative-parameter)
  ())

;;; This class is used to represent a list of parameters that is not
;;; required to be present in a lambda list.
(defclass facultative-parameters ()
  (;; This slot contains a CST for the lambda-list keyword that is
   ;; used to introduce the list of parameters.
   (%keyword-cst :initarg :keyword-cst :reader keyword-cst)
   ;; This slot contains a list of the parameters given.  Each
   ;; subclass defines what kind of objects this list may contain.
   (%parameters :initarg :parameters :reader parameters)))

(defclass optional-parameters (facultative-parameters)
  ())

(defclass keyword-parameter (facultative-parameter)
  (;; This slot contains a CST for the KEYWORD-NAME of the keyword
   ;; parameter.  If no KEYWORD-NAME was given, then this slot
   ;; contains a CST with the KEYWORD-NAME derived from the VARIABLE
   ;; name, and with a LOCATION of NIL.
   (%keyword-name-cst :initarg :keyword-name-cst :reader keyword-name-cst)))

(defclass keyword-parameters (facultative-parameters)
  (;; This slot contains a CST for the lambda-list keyword
   ;; &ALLOW-OTHER-KEYS.  If no &ALLOW-OTHER-KEYS was given, then this
   ;; slot contains NIL.
   (%allow-other-keys-cst :initarg :allow-other-keys-cst
			  :reader allow-other-keys-cst)))
