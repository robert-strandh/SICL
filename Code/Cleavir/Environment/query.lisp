(cl:in-package #:cleavir-environment)

;;;; This file contains definitions of generic functions for querying
;;;; the environment, and classes that are returned as a result of
;;;; such queries.  With respect to the environment functions defined
;;;; in CLtL2, these classes can be thought of as containing the
;;;; information returned by CLtL2 functions such as
;;;; VARIABLE-INFORAMTION, FUNCTION-INFORMATION, and
;;;; DECLARATION-INFORMATION, except that this information has been
;;;; bundled up into a CLOS standard object.
;;;;
;;;; These query functions are called by several Cleavir tools; in
;;;; particular the minimal compiler (in the sense of "minimal
;;;; compilation") and the AST generator that converts a form to an
;;;; abstract syntax tree.
;;;;
;;;; Implementations that use these Cleavir tools need to supply
;;;; methods on the generic function defined here, specialized to
;;;; their particular representation of environments.  Implementations
;;;; that do not support first-class global environments, must
;;;; nevertheless supply a class that represents the global
;;;; environment, but that class is only used for generic dispatch.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-INFO.

;;; Cleavir tools call this function in order to obtain information
;;; about a symbol in a variable position.
(defgeneric variable-info (environment symbol))

;;; As a response to a query about a symbol in a variable position, an
;;; instance of this class must be returned if the symbol stands for a
;;; lexical variable.  
(defclass lexical-variable-info ()
  (;; The name of the variable.  It can be different from the symbol
   ;; supplied in the query as a result of errors and restarts,
   ;; allowing the user to substitute a different symbol.  Therefore,
   ;; the tool that calls VARIABLE-INFO should always use the NAME
   ;; reader for further processing.
   (%name :initarg :name :reader name)
   ;; Since there can be several lexical variables with the same name,
   ;; the IDENTITY allows the tools to distinguish between different
   ;; ones.  A tool that calls ADD-LEXICAL-VARIABLE supplying the
   ;; optional IDENTITY argument expects the IDENTITY reader to return
   ;; the same object as was supplied for that particular variable. 
   (%identity :initarg :identity :reader identity)
   ;; The type of the variable.
   (%type :initform t :initarg :type :reader type)
   ;; There are three possible values here, namely NIL, IGNORE, and
   ;; IGNORABLE.  NIL means that no IGNORE information has been
   ;; supplied.  IGNORE means and IGNORE declaration is in scope, and
   ;; IGNORABLE means and IGNORABLE declaration is in scope.
   (%ignore :initform nil :initarg :ignore :reader ignore)
   ;; True if and only if a dynamic-extent declaration is in scope for
   ;; this variable.
   (%dynamic-extent :initform nil :initarg :dynamic-extent :reader dynamic-extent)))

(defclass special-variable-info ()
  (;; The name of the variable.  It can be different from the symbol
   ;; supplied in the query as a result of errors and restarts,
   ;; allowing the user to substitute a different symbol.  Therefore,
   ;; the tool that calls VARIABLE-INFO should always use the NAME
   ;; reader for further processing.
   (%name :initarg :name :reader name)
   ;; The type of the variable.
   (%type :initform t :initarg :type :reader type)
   ;; There are three possible values here, namely NIL, IGNORE, and
   ;; IGNORABLE.  NIL means that no IGNORE information has been
   ;; supplied.  IGNORE means and IGNORE declaration is in scope, and
   ;; IGNORABLE means and IGNORABLE declaration is in scope.
   (%ignore :initform nil :initarg :ignore :reader ignore)
   ;; GLOBAL-P should be true if and only if the special variable is
   ;; globally special, i.e., a proclamation has been used to declare
   ;; it special.
   (%global-p :initform nil :initarg :global-p :reader global-p)))

(defclass constant-variable-info ()
  (;; The name of the variable.  It can be different from the symbol
   ;; supplied in the query as a result of errors and restarts,
   ;; allowing the user to substitute a different symbol.  Therefore,
   ;; the tool that calls VARIABLE-INFO should always use the NAME
   ;; reader for further processing.
   (%name :initarg :name :reader name)
   ;; The value of the variable.
   (%value :initarg :value :reader value)))

(defclass symbol-macro-info ()
  (;; The name of the symbol macro.  It can be different from the
   ;; symbol supplied in the query as a result of errors and restarts,
   ;; allowing the user to substitute a different symbol.  Therefore,
   ;; the tool that calls VARIABLE-INFO should always use the NAME
   ;; reader for further processing.
   (%name :initarg :name :reader name)  
   ;; The type of the symbol macro.  
   (%type :initform t :initarg :type :reader type)
   ;; The expansion of the symbol macro as a form rather than as an
   ;; expander function.
   (%expansion :initarg :expansion :reader expansion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-INFO.

;;; Attributes are miscellaneous information about functions, not
;;; slotted into any of the other qualities of an info.

;;; Only attribute right now is :DYN-CALL, which means that the
;;; function does not call (and cannot cause to be called) any of its
;;; arguments except in the dynamic environment the function itself
;;; was called in. So it doesn't do unwind-protect or special
;;; bindings or anything else that needs to be unwound around it,
;;; or store it anywhere some other function could do arbitrary
;;; things with it, or signal a condition that makes it accessible
;;; to arbitrary condition handlers.
;;; If a function calls itself in a different dynamic environment,
;;; this attribute can still be in place.

(defun make-attributes (&rest attributes)
  (loop with result = 0
        for attr in attributes
        for flag = (ecase attr
                     ((:dyn-call) 1))
        do (setf result (logior result flag))
        finally (return result)))

(defun default-attributes () 0)

(defun has-attribute-p (attributes attribute-name)
  (plusp
   (logand attributes
           (ecase attribute-name
             ((:dyn-call) 1)))))

;;; Cleavir tools call this function in order to obtain information
;;; about a symbol in a function position.  It could also be used to
;;; obtain information about a function name that is not a symbol.
(defgeneric function-info (environment function-name))

(defclass local-function-info ()
  (;; The name of the local function.  It can be different from the
   ;; function name supplied in the query as a result of errors and
   ;; restarts, allowing the user to substitute a different name.
   ;; Therefore, the tool that calls FUNCTION-INFO should always use
   ;; the NAME reader for further processing.
   (%name :initarg :name :reader name)
   ;; Since there can be several local functions with the same name,
   ;; the IDENTITY allows the tools to distinguish between different
   ;; ones.  A tool that calls ADD-LOCAL-FUNCTION supplying the
   ;; optional IDENTITY argument expects the IDENTITY reader to return
   ;; the same object as was supplied for that particular function.
   (%identity :initarg :identity :reader identity)
   ;; The type of the local function. 
   (%type :initform t :initarg :type :reader type)
   ;; There are three possible values here, namely NIL, INLINE and
   ;; NOTINLINE.  NIL means that there is neither an INLINE nor a
   ;; NOTINLINE declaration in scope.  INLINE means that there is an
   ;; INLINE declaration in scope, and NOTINLINE means that there is a
   ;; NOTINLINE declaration in scope.
   (%inline :initform nil :initarg :inline :reader inline
	    :type (member nil inline notinline))
   ;; There are three possible values here, namely NIL, IGNORE, and
   ;; IGNORABLE.  NIL means that no IGNORE information has been
   ;; supplied.  IGNORE means and IGNORE declaration is in scope, and
   ;; IGNORABLE means and IGNORABLE declaration is in scope.
   (%ignore :initform nil :initarg :ignore :reader ignore)
   ;; The value of this slot is either an abstract syntax tree for the
   ;; function, or NIL if there is no abstract syntax tree available
   ;; for this function.
   (%ast :initform nil :initarg :ast :reader ast)
   ;; True if and only if a dynamic-extent declaration is in scope for
   ;; this function.
   (%dynamic-extent :initform nil
		    :initarg :dynamic-extent
		    :reader dynamic-extent)
   ;; Miscellaneous attributes.
   (%attributes :initform (default-attributes)
                :initarg :attributes
                :reader attributes)))
  
(defclass global-function-info ()
  (;; The name of the global function.  It can be different from the
   ;; function name supplied in the query as a result of errors and
   ;; restarts, allowing the user to substitute a different name.
   ;; Therefore, the tool that calls FUNCTION-INFO should always use
   ;; the NAME reader for further processing.
   (%name :initarg :name :reader name)
   ;; The type of the global function. 
   (%type :initform t :initarg :type :reader type)
   ;; There are three possible values here, namely NIL, INLINE and
   ;; NOTINLINE.  NIL means that there is neither an INLINE nor a
   ;; NOTINLINE declaration in scope.  INLINE means that there is an
   ;; INLINE declaration in scope, and NOTINLINE means that there is a
   ;; NOTINLINE declaration in scope.
   (%inline :initform nil :initarg :inline :reader inline
	    :type (member nil inline notinline))
   ;; If the value of this slot is NIL, it means that there is no
   ;; compiler macro associated with this function.  If not, the value
   ;; must be a compiler macro function.
   (%compiler-macro :initform nil :initarg :compiler-macro :reader compiler-macro)
   ;; There are three possible values here, namely NIL, IGNORE, and
   ;; IGNORABLE.  NIL means that no IGNORE information has been
   ;; supplied.  IGNORE means and IGNORE declaration is in scope, and
   ;; IGNORABLE means and IGNORABLE declaration is in scope.
   (%ignore :initform nil :initarg :ignore :reader ignore)
   ;; The value of this slot is either an abstract syntax tree for the
   ;; function, or NIL if there is no abstract syntax tree available
   ;; for this function.
   (%ast :initform nil :initarg :ast :reader ast)
   ;; True if and only if a dynamic-extent declaration is in scope for
   ;; this function.
   (%dynamic-extent :initform nil
		    :initarg :dynamic-extent
		    :reader dynamic-extent)
   ;; Miscellaneous attributes.
   (%attributes :initform (default-attributes)
                :initarg :attributes
                :reader attributes)))

(defclass local-macro-info ()
  (;; The name of the local macro.  It can be different from the
   ;; function name supplied in the query as a result of errors and
   ;; restarts, allowing the user to substitute a different name.
   ;; Therefore, the tool that calls FUNCTION-INFO should always use
   ;; the NAME reader for further processing.
   (%name :initarg :name :reader name)
   ;; A macro function allowing tools to expand the macro.
   (%expander :initarg :expander :reader expander)))

(defclass global-macro-info ()
  (;; The name of the global macro.  It can be different from the
   ;; function name supplied in the query as a result of errors and
   ;; restarts, allowing the user to substitute a different name.
   ;; Therefore, the tool that calls FUNCTION-INFO should always use
   ;; the NAME reader for further processing.
   (%name :initarg :name :reader name)
   ;; A macro function allowing tools to expand the macro.
   (%expander :initarg :expander :reader expander)
   ;; If the value of this slot is NIL, it means that there is no
   ;; compiler macro associated with this macro.  If not, the value
   ;; must be a compiler macro function.
   (%compiler-macro :initform nil
		    :initarg :compiler-macro
		    :reader compiler-macro)))

(defclass special-operator-info ()
  (;; The name of the special operator.  It can be different from the
   ;; function name supplied in the query as a result of errors and
   ;; restarts, allowing the user to substitute a different name.
   ;; Therefore, the tool that calls FUNCTION-INFO should always use
   ;; the NAME reader for further processing.
   (%name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLOCK-INFO.

;;; Cleavir tools call this function in order to obtain information
;;; about a symbol used as an argument to RETURN-FROM. 
(defgeneric block-info (environment symbol))

(defclass block-info ()
  (;; The name of the block.  It can be different from the symbol
   ;; supplied in the query as a result of errors and restarts,
   ;; allowing the user to substitute a different name.  Therefore,
   ;; the tool that calls BLOCK-INFO should always use the NAME
   ;; reader for further processing.
   (%name :initarg :name :reader name)
   ;; Since there can be several blocks with the same name, the
   ;; IDENTITY allows the tools to distinguish between different ones.
   ;; A tool that calls ADD-BLOCK supplying the optional IDENTITY
   ;; argument expects the IDENTITY reader to return the same object
   ;; as was supplied for that particular block.
   (%identity :initarg :identity :reader identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TAG-INFO.

;;; Cleavir tools call this function in order to obtain information
;;; about a symbol used as an argument to GO. 
(defgeneric tag-info (environment tag))

(defclass tag-info ()
  (;; The name of the tag.  It can be different from the symbol
   ;; supplied in the query as a result of errors and restarts,
   ;; allowing the user to substitute a different name.  Therefore,
   ;; the tool that calls TAG-INFO should always use the NAME
   ;; reader for further processing.
   (%name :initarg :name :reader name)
   ;; Since there can be several tags with the same name, the IDENTITY
   ;; allows the tools to distinguish between different ones.  A tool
   ;; that calls ADD-TAG supplying the optional IDENTITY argument
   ;; expects the IDENTITY reader to return the same object as was
   ;; supplied for that particular tag.
   (%identity :initarg :identity :reader identity)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIMIZE-INFO.

;;; Cleavir tools call this function in order to obtain information
;;; about the policy and optimization levels that are in scope.

(defgeneric optimize-info (environment))

(defclass optimize-info ()
  ((%optimize :initarg :optimize :reader optimize)
   (%policy :initarg :policy :reader policy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function GLOBAL-ENVIRONMENT.
;;;
;;; Given any environment object, this function returns the part of
;;; the environment that constitutes the global environment.

(defgeneric global-environment (environment))
