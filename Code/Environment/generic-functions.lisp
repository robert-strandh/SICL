(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPECIAL-OPERATOR.
;;;
;;; if FUNCTION-NAME has a definition as a special operator in
;;; ENVIRONMENT, then that definition is returned.  The definition is
;;; an the object that was used as an argument to (SETF
;;; SPECIAL-OPERATOR).  The exact nature of this object is not
;;; specified, other than that it can not be NIL.  If FUNCTION-NAME
;;; does not have a definition as a special operator in ENVIRONMENT,
;;; then NIL is returned. 

(defgeneric special-operator (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FDEFINITION.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:FDEFINITION. 
;;;
;;; If FUNCTION-NAME has a definition in the function namespece of
;;; ENVIRONMENT (i.e., if FBOUNDP returns true), then a call to this
;;; function succeeds.  Otherwise an error of type UNDEFINED-FUNCTION
;;; is signaled.
;;;
;;; If FUNCTION-NAME is defined as an ordinary function or a generic
;;; function, then a call to this function returns the associated
;;; function object.
;;;
;;; If FUNCTION-NAME is defined as a macro, then a list of the form
;;; (CL:MACRO-FUNCTION <function>) is returned, where <function> is
;;; the macro expansion function associated with the macro.
;;;
;;; If FUNCTION-NAME is defined as a special operator, then a list of
;;; the form (CL:SPECIAL <object>) is returned, where the nature of
;;; <object> is currently not specified. 

(defgeneric fdefinition (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FDEFINITION).
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:FDEFINITION. 
;;;
;;; NEW-DEFINITION must be a function.  If FUNCTION-NAME already names
;;; a function or a macro, then the previous definition is lost.  If
;;; FUNCTION-NAME already names a special operator, then an error is
;;; signaled.

(defgeneric (setf fdefinition) (new-definition function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MACRO-FUNCTION.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:MACRO-FUNCTION.
;;;
;;; If SYMBOL has a definition as a macro in ENVIRONMENT, then the
;;; corresponding macro expansion function is returned.
;;;
;;; If SYMBOL has no definition in the function namespace of
;;; ENVIRONMENT, or if the definition is not a macro, then this
;;; function returns NIL.

(defgeneric macro-function (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF MACRO-FUNCTION)
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function (SETF CL:MACRO-FUNCTION).
;;;
;;; NEW-FUNCTION must be a macro expansion function.  A call to this
;;; function then always succeeds.  If the symbol already names a
;;; macro or a function, then the previous definition is lost.  If the
;;; symbol already names a special operator, that definition is kept.

(defgeneric (setf macro-function) (new-function symbol environment))
