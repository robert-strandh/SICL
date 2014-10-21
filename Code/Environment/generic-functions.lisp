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
;;; Generic function (SETF SPECIAL-OPERATOR).
;;;
;;; Set the definition of FUNCTION-NAME to be a special operator.  The
;;; exact nature of NEW-DEFINITION is not specified, except that a
;;; value of NIL means that FUNCTION-NAME no longer has a definition
;;; as a special operator in ENVIRONMENT.
;;;
;;; If a value other than NIL is given for NEW-DEFINITION, and
;;; FUNCTION-NAME already has a definition as an ordinary function, as
;;; a generic function, or as a macro, then an error is signaled.  As
;;; a consequence, if it is desirable for FUNCTION-NAME to have a
;;; definition both as a special operator and as a macro, then the
;;; definition as a special operator should be set first.

(defgeneric (setf special-operator) (new-definition function-name environment))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function COMPILER-MACRO-FUNCTION.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:COMPILER-MACRO-FUNCTION.
;;;
;;; If FUNCTION-NAME has a definition as a compiler macro in
;;; ENVIRONMENT, then the corresponding compiler macro function is
;;; returned.
;;;
;;; If FUNCTION-NAME has no definition as a compiler macro in
;;; ENVIRONMENT, then this function returns NIL.

(defgeneric compiler-macro-function (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF COMPILER-MACRO-FUNCTION).
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function (SETF CL:COMPILER-MACRO-FUNCTION).
;;;
;;; NEW-DEFINITION can be a compiler macro function or NIL.  When it
;;; is a compiler macro function, then it establishes NEW-DEFINITION
;;; as a compiler macro for FUNCTION-NAME and any existing definition
;;; is lost.  A value of NIL means that FUNCTION-NAME no longer has a
;;; compiler macro associated with it in ENVIRONMENT. 

(defgeneric (setf compiler-macro-function) (new-definition function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function CONSTANT-VARIABLE
;;;
;;; This function returns two values.  The first value is the value of
;;; SYMBOL as a constant variable in ENVIRONMENT, or NIL if SYMBOL
;;; does not have a value as a constant variable in ENVIRONMENT.  The
;;; second value is true if SYMBOL does have a value as a constant
;;; variable in ENVIRONMENT and false otherwise.

(defgeneric constant-variable (symbol environment))
