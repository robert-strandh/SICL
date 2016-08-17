(cl:in-package #:cleavir-environment)

;;;; This file contains classes that can be used to augment an
;;;; environment with local information.  Existing implementations can
;;;; not use these classes, because the existing macro expander must
;;;; know how to deal with them.  They can be used in new
;;;; implementations where the creator does not have any particular
;;;; opinion about how to represent locally augmented environments.
;;;;
;;;; An alternative strategy for an existing implementation is to
;;;; modify its implementation of MACROEXPAND-1 so that it can handle
;;;; both the native representation of augmented environments, and
;;;; this one supplied by Cleavir.

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

(defmethod print-object ((object lexical-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (identity object))))

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

(defmethod print-object ((object special-variable) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (name object))))

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

(defmethod print-object ((object symbol-macro) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (expansion object))))

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

(defmethod print-object ((object function) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (identity object))))

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

(defmethod print-object ((object macro) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (name object))))

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

(defmethod print-object ((object block) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (identity object))))

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

(defmethod print-object ((object tag) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (identity object))))

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

(defmethod print-object ((object variable-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (type object))))

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

(defmethod print-object ((object function-type) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (type object))))

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

(defmethod print-object ((object variable-ignore) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (ignore object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-IGNORE.

;;; This class is used to augment an environment with an IGNORE
;;; declarations of a function, introduced by a declaration in a
;;; special form that allows such declarations.
(defclass function-ignore (entry)
  ((%name :initarg :name :reader name)
   (%ignore :initarg :ignore :reader ignore)))

(defmethod add-function-ignore (environment function-name ignore)
  (make-instance 'function-ignore
    :next environment
    :name function-name
    :ignore ignore))

(defmethod print-object ((object function-ignore) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (ignore object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; VARIABLE-DYNAMIC-EXTENT.

;;; This class is used to augment an environment with a DYNAMIC-EXTENT
;;; declarations of a variable, introduced by a declaration in a
;;; special form that allows such declarations.
(defclass variable-dynamic-extent (entry)
  ((%name :initarg :name :reader name)))

(defmethod add-variable-dynamic-extent (environment symbol)
  (make-instance 'variable-dynamic-extent
    :next environment
    :name symbol))

(defmethod print-object ((object variable-dynamic-extent) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; FUNCTION-DYNAMIC-EXTENT.

;;; This class is used to augment an environment with a DYNAMIC-EXTENT
;;; declarations of a function, introduced by a declaration in a
;;; special form that allows such declarations.
(defclass function-dynamic-extent (entry)
  ((%name :initarg :name :reader name)))

(defmethod add-function-dynamic-extent (environment function-name)
  (make-instance 'function-dynamic-extent
    :next environment
    :name function-name))

(defmethod print-object ((object function-dynamic-extent) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (name object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIMIZE.

;;; This class is used to augment an environment with an OPTIMIZE
;;; declarations, introduced by a declaration in a special form that
;;; allows such declarations.
(defclass optimize (entry)
  ((%quality :initarg :quality :reader quality)
   (%value :initarg :value :reader value)))

(defmethod add-optimize (environment quality value)
  (make-instance 'optimize
    :next environment
    :quality quality
    :value value))

(defmethod print-object ((object optimize) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (quality object) (value object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; INLINE.

;;; This class is used to augment an environment with an INLINE
;;; declarations of a function, introduced by a declaration in a
;;; special form that allows such declarations.
(defclass inline (entry)
  ((%name :initarg :name :reader name)
   (%inline :initarg :inline :reader inline)))

(defmethod add-inline (environment function-name inline)
  (make-instance 'inline
    :next environment
    :name function-name
    :inline inline))

(defmethod print-object ((object inline) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (inline object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; INLINE-EXPANSION.

;;; This class is used to augment an environment with an inline
;;; expansion for a function (i.e., the :ast initarg to
;;; local- and global-function-info).
;;; It is separate from INLINE because having an expansion doesn't
;;; mean using it, and separate from the defining info because
;;; sometimes you have the name but want to add the expansion lower
;;; down.
(defclass inline-expansion (entry)
  ((%name :initarg :name :reader name)
   (%ast :initarg :ast :reader ast)))

(defmethod add-inline-expansion (environment function-name expansioN)
  (make-instance 'inline-expansion
    :next environment
    :name function-name
    :ast expansion))

(defmethod print-object ((object inline-expansion) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s" (name object))))
