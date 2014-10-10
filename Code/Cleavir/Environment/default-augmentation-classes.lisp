(cl:in-package #:cleavir-environment)

;;;; This file contains classes that can be used to augment an
;;;; environment with local information.  Existing implementations can
;;;; not use these classes, because the existing macro expander must
;;;; know how to deal with them.  They can be used in new
;;;; implementations where the creator does not have any particular
;;;; opinion about how to represent locally augmented environments.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ENTRY. 
;;;
;;; This class is the root of all locally augmented environments.  We
;;; represent a locally augmented environment as a chain, i.e., each
;;; instance has a NEXT slot that refers to the less specific
;;; environment.  The chain ends with a reference to an object
;;; representing the global environment.  Implementations that do not
;;; have first-class global environments must still create a dummy
;;; class that represents their particular global environment.
(defclass entry ()
  ((%next :initarg :next :reader next)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; LEXICAL-VARIABLE.

;;; This class is used to augment an environment with a lexical
;;; variable introduced by LET or LET*.  Client code can supply an
;;; IDENTITY object that is used to distinguish between different
;;; lexical variables with the same name.
(defclass lexical-variable (entry)
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defmethod add-lexical-variable
    (environment symbol &optional (identity (gensym)))
  (make-instance 'lexical-variable
    :next environment
    :name symbol
    :identity identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SPECIAL-VARIABLE.

;;; This class is used to augment an environment with a special
;;; variable.  This class is only used when a binding of a special
;;; variable is seen, and that variable is not globally special.  By
;;; `globally special', we mean that a proclamation has been used
;;; directly or indirectly to proclaim the variable as special.  
;;;
;;; Whereas a lexical variable needs some kind of identity in order to
;;; distinguish between different lexical variables with the same
;;; name, no such identity is needed for special variables, simply
;;; because there is only one special variable with a particular name.
;;; Thus, the name of the special variable is sufficient to determine
;;; which special variable is meant.
(defclass special-variable (entry)
  ((%name :initarg :name :reader name)))

(defmethod add-special-variable (environment symbol)
  (make-instance 'special-variable
    :next environment
    :name symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SYMBOL-MACRO.

;;; This class is used to augment an environment with a symbol macro.
;;; We use the expansion of the symbol macro rather than expander
;;; function.
(defclass symbol-macro (entry)
  ((%name :initarg :name :reader name)
   (%expansion :initarg :expansion :reader expansion)))

(defmethod add-local-symbol-macro (environment symbol expansion)
  (make-instance 'symbol-macro
    :next environment
    :name symbol
    :expansion expansion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION.

;;; This class is used to augment an environment with a local function
;;; introduced by FLET or LABELS.  Client code can supply an IDENTITY
;;; object that is used to distinguish between different local
;;; functions with the same name.
(defclass function (entry)
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defmethod add-local-function
    (environment function-name &optional (identity (gensym)))
  (make-instance 'function
    :next environment
    :name function-name
    :identity identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MACRO.

;;; This class is used to augment an environment with a local macro
;;; introduced by MACROLET.  
(defclass macro (entry)
  ((%name :initarg :name :reader name)
   (%expander :initarg :expander :reader expander)))

(defmethod add-local-macro (environment symbol expander)
  (make-instance 'macro
    :next environment
    :name symbol
    :expander expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BLOCK.

;;; This class is used to augment an environment with a block
;;; introduced by BLOCK.  Client code can supply an IDENTITY object
;;; that is used to distinguish between different blocks with the same
;;; name.
(defclass block (entry)
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defmethod add-block (environment symbol &optional (identity (gensym)))
  (make-instance 'block
    :next environment
    :name symbol
    :identity identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; TAG.

;;; This class is used to augment an environment with a tag introduced
;;; by TAGBODY for each go tag.  Client code can supply an IDENTITY
;;; object that is used to distinguish between different tags with
;;; the same name.
(defclass tag (entry)
  ((%name :initarg :name :reader name)
   (%identity :initarg :identity :reader identity)))

(defmethod add-tag (environment symbol &optional (identity (gensym)))
  (make-instance 'tag
    :next environment
    :name symbol
    :identity identity))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-TYPE.

;;; This class is used to augment an environment with the type of a
;;; variable, introduced by a declaration in a special form that
;;; allows such declarations.
(defclass variable-type (entry)
  ((%name :initarg :name :reader name)
   (%type :initarg :type :reader type)))

(defmethod add-variable-type (environment symbol type)
  (make-instance 'variable-type
    :next environment
    :name symbol
    :type type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-TYPE.

;;; This class is used to augment an environment with the type of a
;;; function, introduced by a declaration in a special form that
;;; allows such declarations.
(defclass function-type (entry)
  ((%name :initarg :name :reader name)
   (%type :initarg :type :reader type)))

(defmethod add-function-type (environment function-name type)
  (make-instance 'function-type
    :next environment
    :name function-name
    :type type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-IGNORE.

;;; This class is used to augment an environment with an IGNORE
;;; declarations of a variable, introduced by a declaration in a
;;; special form that allows such declarations.
(defclass variable-ignore (entry)
  ((%name :initarg :name :reader name)
   (%ignore :initarg :ignore :reader ignore)))

(defmethod add-variable-ignore (environment symbol ignore)
  (make-instance 'variable-ignore
    :next environment
    :name symbol
    :ignore ignore))

(defclass function-ignore (entry)
  ((%name :initarg :name :reader name)
   (%ignore :initarg :ignore :reader ignore)))

(defmethod add-function-ignore (environment function-name ignore)
  (make-instance 'function-ignore
    :next environment
    :name function-name
    :ignore ignore))

(defclass variable-dynamic-extent (entry)
  ((%name :initarg :name :reader name)))

(defmethod add-variable-dynamic-extent (environment symbol)
  (make-instance 'variable-dynamic-extent
    :next environment
    :name symbol))

(defclass function-dynamic-extent (entry)
  ((%name :initarg :name :reader name)))

(defmethod add-function-dynamic-extent (environment function-name)
  (make-instance 'function-dynamic-extent
    :next environment
    :name function-name))

(defclass optimize (entry)
  ((%quality :initarg :quality :reader quality)
   (%value :initarg :value :reader value)))

(defmethod add-optimize (environment quality value)
  (make-instance 'optimize
    :next environment
    :quality quality
    :value value))

(defclass inline (entry)
  ((%name :initarg :name :reader name)
   (%inline :initarg :inline :reader inline)))

(defmethod add-inline (environment function-name inline)
  (make-instance 'inline
    :next environment
    :name function-name
    :inline inline))
