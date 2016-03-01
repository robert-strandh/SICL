(cl:in-package #:cleavir-cst)

(defclass optional-parameter ()
  (;; This slot contains the CST for the obligatory variable of the
   ;; optional parameter.
   (%variable-cst :initarg :variable-cst :reader variable-cst)
   ;; This slot contains a CST for the INIT-FORM of the optional
   ;; parameter.  If no INIT-FORM was supplied, then this slot
   ;; contains a CST for which both EXPRESSION and LOCATION return
   ;; NIL.
   (%init-form-cst :initarg :init-form-cst :reader init-form-cst)
   ;; This slot contains a CST for the SUPPLIED-P-PARAMETER of the
   ;; optional parameter.  If no SUPPLIED-P-PARAMETER was given, then
   ;; this slot contains NIL.
   (%supplied-p-parameter-cst :initform :supplied-p-parameter-cst
			      :reader supplied-p-parameter-cst)))
