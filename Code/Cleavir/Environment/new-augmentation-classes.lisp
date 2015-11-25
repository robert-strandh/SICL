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
;;; Class ENVIRONMENT.

(defclass environment ()
  ((%global-environment :initarg :global-environment
			:accessor global-environment)
   (%augmentations :initarg :augmentations :initform '()
		   :accessor augmentations)))

(defun augment-environment (environment augmentation)
  (make-instance 'environment
    :global-environment (global-environment environment)
    :augmentations (cons augmentation (augmentations environment))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class ENTRY. 

(defclass entry () ())

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
  (augment-environment environment
		       (make-instance 'lexical-variable
			 :name symbol
			 :identity identity)))

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
  (augment-environment environment
		       (make-instance 'special-variable
			 :name symbol)))

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
  (augment-environment environment
		       (make-instance 'symbol-macro
			 :name symbol
			 :expansion expansion)))

(defmethod print-object ((object symbol-macro) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (expansion object))))

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
  (augment-environment environment
		       (make-instance 'block
			 :name symbol
			 :identity identity)))

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
  (augment-environment environment
		       (make-instance 'tag
			 :name symbol
			 :identity identity)))

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
  (augment-environment environment
		       (make-instance 'variable-type
			 :name symbol
			 :type type)))

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
  (augment-environment environment
		       (make-instance 'function-type
			 :name function-name
			 :type type)))

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
  (augment-environment environment
		       (make-instance 'variable-ignore
			 :name symbol
			 :ignore ignore)))

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
  (augment-environment environment
		       (make-instance 'function-ignore
			 :name function-name
			 :ignore ignore)))

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
  (augment-environment environment
		       (make-instance 'variable-dynamic-extent
			 :name symbol)))

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
  (augment-environment environment
		       (make-instance 'function-dynamic-extent
			 :name function-name)))

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
  (augment-environment environment
		       (make-instance 'optimize
			 :quality quality
			 :value value)))

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
  (augment-environment environment
		       (make-instance 'inline
			 :name function-name
			 :inline inline)))

(defmethod print-object ((object inline) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~s ~s" (name object) (inline object))))
