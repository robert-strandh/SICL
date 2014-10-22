(cl:in-package #:sicl-global-environment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FBOUNDP.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:FBOUNDP. 
;;;
;;; It returns true if FUNCTION-NAME has a definition in ENVIRONMENT
;;; as an ordinary function, a generic function, a macro, or a special
;;; operator.

(defgeneric fboundp (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FMAKUNBOUND.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:FMAKUNBOUND.
;;;
;;; Makes FUNCTION-NAME unbound in the function namespace of ENVIRONMENT. 
;;;
;;; If FUNCTION-NAME already has a definition in ENVIRONMENT as an
;;; ordinary function, as a generic function, as a macro, or as a
;;; special operator, then that definition is lost.

(defgeneric fboundp (function-name environment))

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
;;; Generic function BOUNDP.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:BOUNDP. 
;;;
;;; It returns true if SYMBOL has a definition in ENVIRONMENT as a
;;; constant variable, as a special variable, or as a symbol macro.
;;; Otherwise, it returns false.

(defgeneric boundp (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function MAKUNBOUND.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:MAKUNBOUND. 
;;;
;;; It makes SYMBOL unbound in ENVIRONMENT.  If SYMBOL is already
;;; defined in ENVIRONMENT as a constant variable, as a lexical
;;; variable, or as a symbol macro, then that definition is lost.

(defgeneric makunbound (symbol environment))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF CONSTANT-VARIABLE). 
;;;
;;; This function is used in order to define SYMBOL as a constant
;;; variable in ENVIRONMENT, with VALUE as its value.
;;;
;;; If SYMBOL already has a definition as a special variable or as a
;;; symbol macro in ENVIRONMENT, then an error is signaled.
;;;
;;; If SYMBOL already has a definition as a constant variable, and its
;;; current value is not EQL to VALUE, then an error is signaled.

(defgeneric (setf constant-variable) (value symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SPECIAL-VARIABLE
;;;
;;; This function returns two values.  The first value is the value of
;;; SYMBOL as a special variable in ENVIRONMENT, or NIL if SYMBOL
;;; does not have a value as a special variable in ENVIRONMENT.  The
;;; second value is true if SYMBOL does have a value as a special
;;; variable in ENVIRONMENT and false otherwise.
;;;
;;; Notice that the symbol can have a value even though this function
;;; returns NIL and NIL.  The first such case is when the symbol has a
;;; value as a constant variable in ENVIRONMENT.  The second case is
;;; when the symbol was assigned a value using (SETF SYMBOL-VALUE)
;;; without declaring the variable as SPECIAL.

(defgeneric special-variable (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF SPECIAL-VARIABLE). 
;;;
;;; This function is used in order to define SYMBOL as a special
;;; variable in ENVIRONMENT.
;;;
;;; If SYMBOL already has a definition as a constant variable or as a
;;; symbol macro in ENVIRONMENT, then an error is signaled.
;;; Otherwise, SYMBOL is defined as a special variable in ENVIRONMENT. 
;;;
;;; If SYMBOL already has a definition as a special variable, and
;;; INITIALIZE-P is false, then this function has no effect.  The
;;; current value is not altered, or if SYMBOL is currently unbound,
;;; then it remains unbound.
;;;
;;; If INITIALIZE-P is true, then VALUE becomes the new value of the
;;; special variable SYMBOL. 

(defgeneric (setf special-variable)
    (value symbol environment &optional (initialize-p t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SYMBOL-MACRO.
;;;
;;; This function returns two values.  The first value is a macro
;;; expansion function associated with the symbol macro named by
;;; SYMBOL, or NIL if SYMBOL does not have a definition as a symbol
;;; macro.  The second value is the form that SYMBOL expands to as a
;;; macro, or NIL if symbol does not have a definition as a symbol macro. 
;;;
;;; It is guaranteed that the same (in the sense of EQ) function is
;;; returned by two consecutive calls as the first argument as long as
;;; the definition of SYMBOL does not change.

(defgeneric symbol-macro (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF SYMBOL-MACRO).
;;;
;;; This function is used in order to define SYMBOL as a symbol macro
;;; with the given EXPANSION in ENVIRONMENT. 
;;;
;;; If SYMBOL already has a definition as a constant variable, or as a
;;; special variable, then an error of type PROGRAM-ERROR is signaled.

(defgeneric (setf symbol-macro) (expansion symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-CLASS.
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function CL:FIND-CLASS. 
;;;
;;; If SYMBOL has a definition as a class in ENVIRONMENT, then that
;;; class metaobject is returned.  Otherwise NIL is returned. 

(defgeneric find-class (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FIND-CLASS).
;;;
;;; This generic function is a generic version of the Common Lisp
;;; function (SETF CL:FIND-CLASS). 
;;;
;;; This function is used in order to associate a class with a class
;;; name in ENVIRONMENT.  
;;;
;;; If NEW-CLASS is a class metaobject, then that class metaobject is
;;; associated with the name SYMBOL in ENVIRONMENT.  If SYMBOL already
;;; names a class in ENVIRONMENT than that association is lost.
;;;
;;; If NEW-CLASS is NIL, then SYMBOL is no longer associated with a
;;; class in ENVIRONMENT.

(defgeneric (setf find-class) (new-class symbol environment))
