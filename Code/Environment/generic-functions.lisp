(cl:in-package #:sicl-global-environment)

(defclass environment () ())

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
;;;
;;; If FUNCTION-NAME has a SETF expander associated with it, then that
;;; SETF expander is lost.

(defgeneric fmakunbound (function-name environment))

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
;;;
;;; If FUNCTION-NAME is a symbol and it has an associated setf
;;; expander, then that setf expander is preserved.

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
;;; NEW-FUNCTION must be a macro expansion function or NIL.  A call to
;;; this function then always succeeds.  A value of NIL means that the
;;; SYMBOL no longer has a macro function associated with it.  If
;;; SYMBOL already names a macro or a function, then the previous
;;; definition is lost.  If SYMBOL already names a special operator,
;;; that definition is kept.
;;;
;;; If SYMBOL already names a function, then any proclamation of the
;;; type of that function is lost.  In other words, if at some later
;;; point SYMBOL is again defined as a function, its proclaimed type
;;; will be T.
;;;
;;; If SYMBOL already names a function, then any INLINE or NOTINLINE
;;; proclamation of the type of that function is lost.  In other
;;; words, if at some later point SYMBOL is again defined as a
;;; function, its proclaimed inline information will be NIL. 
;;;
;;; If FUNCTION-NAME is a symbol and it has an associated setf
;;; expander, then that setf expander is preserved.

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
;;; Generic function FUNCTION-TYPE.
;;;
;;; This generic function returns the proclaimed type of the function
;;; associated with FUNCTION-NAME in ENVIRONMENT.
;;;
;;; If FUNCTION-NAME is not associated with an ordinary function or a
;;; generic function in ENVIRONMENT, then NIL is returned.
;;;
;;; If FUNCTION-NAME is associated with an ordinary function or a
;;; generic function in ENVIRONMENT, but no type proclamation for that
;;; function has been made, then this generic function returns T.

(defgeneric function-type (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FUNCTION-TYPE).
;;;
;;; This generic function is used to set the proclaimed type of the
;;; function associated with FUNCTION-NAME in ENVIRONMENT to NEW-TYPE.
;;;
;;; If FUNCTION-NAME is associated with a macro or a special operator
;;; in ENVIRONMENT, then an error is signaled.

(defgeneric (setf function-type) (new-type function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-INLINE.
;;;
;;; This generic function returns the proclaimed inline information of
;;; the function associated with FUNCTION-NAME in ENVIRONMENT.
;;;
;;; If FUNCTION-NAME is not associated with an ordinary function or a
;;; generic function in ENVIRONMENT, then an error is signaled.  
;;;
;;; If FUNCTION-NAME is associated with an ordinary function or a
;;; generic function in ENVIRONMENT, then the return value of this
;;; function is either NIL, INLINE, or NOTINLINE.  If no inline
;;; proclamation has been made, then this generic function returns
;;; NIL.

(defgeneric function-inline (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FUNCTION-INLINE).
;;;
;;; This generic function is used to set the proclaimed inline
;;; information of the function associated with FUNCTION-NAME in
;;; ENVIRONMENT to NEW-INLINE.
;;;
;;; NEW-INLINE must have one of the values NIL, INLINE, or NOTINLINE.
;;;
;;; If FUNCTION-NAME is not associated with an ordinary function or a
;;; generic function in ENVIRONMENT, then an error is signaled.  

(defgeneric (setf function-inline) (new-inline function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-CELL.
;;;
;;; A call to this function always succeeds.  It returns a CONS cell,
;;; in which the CAR always holds the current definition of the
;;; function named FUNCTION-NAME.  When FUNCTION-NAME has no
;;; definition as a function, the CAR of this cell will contain a
;;; function that, when called, signals an error of type
;;; UNDEFINED-FUNCTION.  The return value of this function is always
;;; the same (in the sense of EQ) when it is passed the same (in the
;;; sense of EQUAL) function name and the same (in the sense of EQ)
;;; environment.

(defgeneric function-cell (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-UNBOUND.
;;;
;;; A call to this function always succeeds.  It returns a function
;;; that, when called, signals an error of type UNDEFINED-FUNCTION.
;;; When FUNCTION-NAME has no definition as a function, the return
;;; value of this function is the contents of the CONS cell returned
;;; by FUNCTION-CELL.  The return value of this function is always the
;;; same (in the sense of EQ) when it is passed the same (in the sense
;;; of EQUAL) function name and the same (in the sense of EQ)
;;; environment.  Client code can use the return value of this
;;; function to determine whether FUNCTION-NAME is unbound and if so
;;; signal an error when an attempt is made to evaluate the form
;;; (FUNCTION FUNCTION-NAME).

(defgeneric function-unbound (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-LAMBDA-LIST.
;;;
;;; This function returns two values.  The first value is an ordinary
;;; lambda list, or NIL if no lambda list has been defined for
;;; FUNCTION-NAME.  The second value is true if and only if a lambda
;;; list has been defined for function-name. 

(defgeneric function-lambda-list (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-AST.
;;;
;;; This function returns the abstract syntax tree corresponding to
;;; the FUNCTION-NAME, or NIL if no abstract syntax tree has been
;;; associated with the function.

(defgeneric function-ast (function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FUNCTION-AST).
;;;
;;; This function associates an abstract syntax tree with the name of
;;; a function.  It always succeeds.

(defgeneric (setf function-ast) (new-ast function-name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FUNCTION-NAMES.
;;;
;;; This function returns a list of names associated with FUNCTION in
;;; ENVIRONMENT.  If FUNCTION is currently not associated with any
;;; names in ENVIRONMENT, then the empty list is returned.  The value
;;; returned by this function is determined by previous calls to other
;;; functions such as (SETF FDEFINITION), FMAKUNBOUND, etc.  The
;;; consequences are undefined if FUNCTION is not a function.
;;;
;;; This function can be seen as an alternative to "named lambdas" and
;;; other similar mechanisms for associating names with functions.  It
;;; is useful for showing names of functions in error messages and
;;; backtraces.

(defgeneric function-names (function environment))

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
;;; SYMBOL as a special variable in ENVIRONMENT, or NIL if SYMBOL does
;;; not have a value as a special variable in ENVIRONMENT.  The second
;;; value is true if SYMBOL is a special variable in ENVIRONMENT and
;;; false otherwise.  There can be two reasons why a symbol does not
;;; have a value as a special variable in ENVIRONMENT.  Either it is
;;; not a special variable at all, or it is a special variable, but it
;;; is currently unbound.
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

(defgeneric (setf special-variable) (value symbol environment initialize-p))

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
;;; Generic function SYMBOL-PLIST.
;;;
;;; This function returns the PROPERTY LIST of SYMBOL in ENVIRONMENT.

(defgeneric symbol-plist (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF SYMBOL-PLIST).
;;;
;;; This function sets the PROPERTY LIST of SYMBOL in ENVIRONMENT to
;;; NEW-PLIST.

(defgeneric (setf symbol-plist) (new-plist symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-TYPE.
;;;
;;; This generic function returns the proclaimed type of the variable
;;; associated with SYMBOL in ENVIRONMENT.
;;;
;;; If SYMBOL has a definition as a constant variable in ENVIRONMENT,
;;; then the result of calling TYPE-OF on its value is returned. 
;;;
;;; If SYMBOL does not have a definition as a constant variable in
;;; ENVIRONMENT, and no previous type proclamation has been made for
;;; SYMBOL in ENVIRONMENT, then this function returns T.

(defgeneric variable-type (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF VARIABLE-TYPE).
;;;
;;; This generic function is used to set the proclaimed type of the
;;; variable associated with SYMBOL in ENVIRONMENT.
;;;
;;; If SYMBOL has a definition as a constant variable in ENVIRONMENT,
;;; then an error is signaled.
;;;
;;; It is meaningful to set the proclaimed type even if SYMBOL has not
;;; previously been defined as a special variable or as a symbol
;;; macro, because it is meaningful to use (SETF SYMBOL-VALUE) on such
;;; a symbol.
;;; 
;;; Recall that the HyperSpec defines the meaning of proclaiming the
;;; type of a symbol macro.  Therefore, it is meaningful to call this
;;; function when SYMBOL has a definition as a symbol macro in
;;; ENVIRONMENT.

(defgeneric (setf variable-type) (new-type symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-CELL.
;;;
;;; A call to this function always succeeds.  It returns a CONS cell,
;;; in which the CAR always holds the current definition of the
;;; variable named SYMBOL.  When SYMBOL has no definition as a
;;; variable, the CAR of this cell will contain an object that
;;; indicates that the variable is unbound.  This object is the return
;;; value of the function VARIABLE-UNBOUND.  The return value of this
;;; function is always the same (in the sense of EQ) when it is passed
;;; the same symbol and the same environment.

(defgeneric variable-cell (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function VARIABLE-UNBOUND.
;;;
;;; A call to this function always succeeds.  It returns an object
;;; that indicates that the variable is unbound.  The CONS cell
;;; returned by the function VARIABLE-CELL contains this object
;;; whenever the variable named SYMBOL is unbound.  The return value
;;; of this function is always the same (in the sense of EQ) when it
;;; is passed the same symbol and the same environment.  Client code
;;; can use the return value of this function to determine whether
;;; SYMBOL is unbound.

(defgeneric variable-unbound (symbol environment))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function SETF-EXPANDER.
;;;
;;; This generic function returns the setf expander associated with
;;; SYMBOL in ENVIRONMENT.  If SYMBOL is not associated with any setf
;;; expander in ENVIRONMENT, then NIL is returned.

(defgeneric setf-expander (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF SETF-EXPANDER).
;;;
;;; This generic function is used to set the setf expander associated
;;; with SYMBOL in ENVIRONMENT.
;;;
;;; If SYMBOL is not associated with an ordinary function, a generic
;;; function, or a macro in ENVIRONMENT, then an error is signaled.  
;;;
;;; If there is already a setf expander associated with SYMBOL in
;;; ENVIRONMENT, then the old setf expander is lost. 
;;; 
;;; If a value of NIL is given for NEW-EXPANDER, then any current setf
;;; expander associated with SYMBOL is removed.  In this case, no
;;; error is signaled, even if SYMBOL is not associated with any
;;; ordinary function, generic function, or macro in ENVIRONMENT.

(defgeneric (setf setf-expander) (new-expander symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DEFAULT-SETF-EXPANDER.
;;;
;;; This generic function returns the default SETF expander, to be
;;; used when the function SETF-EXPANDER returns NIL.  This function
;;; always returns a valid SETF expander.

(defgeneric default-setf-expander (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF DEFAULT-SETF-EXPANDER).
;;;
;;; This generic function is used to set the default SETF expander
;;; in ENVIRONMENT.  

(defgeneric (setf default-setf-expander) (new-expander environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function TYPE-EXPANDER.
;;;
;;; This generic function returns the type expander associated with
;;; SYMBOL in ENVIRONMENT.  If SYMBOL is not associated with any type
;;; expander in ENVIRONMENT, then NIL is returned.

(defgeneric type-expander (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (TYPE TYPE-EXPANDER).
;;;
;;; This generic function is used to set the type expander associated
;;; with SYMBOL in ENVIRONMENT.
;;;
;;; If there is already a type expander associated with SYMBOL in
;;; ENVIRONMENT, then the old type expander is lost. 

(defgeneric (setf type-expander) (new-expander symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function PACKAGES.

(defgeneric packages (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF PACKAGES).

(defgeneric (setf packages) (new-packages environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-PACKAGE.
;;;
;;; Contrary to CL:FIND-PACKAGE, this function does not accept a
;;; package object as its argument.  It has to be a string.

(defgeneric find-package (name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-METHOD-COMBINATION-CLASS.
;;;
;;; If SYMBOL has a definition as a subclass of the class
;;; METHOD-COMBINATION in ENVIRONMENT, then that class metaobject is
;;; returned.  Otherwise NIL is returned.

(defgeneric find-method-combination-class (symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FIND-METHOD-COMBINATION-CLASS).
;;;
;;; This function is used in order to associate a subclass of the
;;; class METHOD-COMBINATION class with a method-combination name in
;;; ENVIRONMENT.
;;;
;;; If NEW-CLASS is a class metaobject, then that class metaobject is
;;; associated with the name SYMBOL in ENVIRONMENT.  If SYMBOL already
;;; names a method-combination class in ENVIRONMENT than that
;;; association is lost.
;;;
;;; If NEW-CLASS is NIL, then SYMBOL is no longer associated with a
;;; method-combination class in ENVIRONMENT.

(defgeneric (setf find-method-combination-class) (new-class symbol environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function FIND-STANDARD-METHOD-COMBINATION.
;;;
;;; Return the unique instance of the method-combination class named
;;; STANDARD.  This generic function exists in order to avoid
;;; allocating a new instance of the STANDARD method-combination class
;;; each time a generic function using the standard method combination
;;; is created.
;;;
;;; If no preceding call to (SETF FIND-STANDARD-METHOD-COMBINATION)
;;; has been made, then this call signals an error.

(defgeneric find-standard-method-combination (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF FIND-METHOD-COMBINATION-CLASS).
;;;
;;; Set the unique instance of the method-combination class named
;;; STANDARD.  After this generic function has been called, subsequent
;;; calls to FIND-STANDARD-METHOD-COMBINATION will return NEW-INSTANCE.

(defgeneric (setf find-standard-method-combination) (new-instance environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function DECLARATION.
;;;
;;; Return true if NAME has been proclaimed as a DECLARATION, and
;;; false otherwise.

(defgeneric declaration (name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function (SETF DECLARATION).
;;;
;;; If VALUE is true, record that NAME has been proclaimed as a
;;; DECLARATION in ENVIRONMENT, so that a subsequent call to
;;; DECLARATION returns true.  If value is false, delete the record
;;; that NAME has been proclaimed as a DECLARATION in ENVIRONMENT, so
;;; that a subsequent call to DECLARATION returns false.

(defgeneric (setf declaration) (value name environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function OPTIMIZE-QUALITY-VALUES.
;;;
;;; Return an association list containing pairs of the form
;;; (QUALITY-NAME . VALUE) for all the OPTIMIZE qualities that this
;;; environement allows.

(defun optimize-quality-values (environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic function POLICY-VALUES.
;;;
;;; Return an association list containing pairs of the form
;;; (POLICY-NAME . VALUE) for each policy that this environement
;;; allows.

(defun policy-values (environment))
