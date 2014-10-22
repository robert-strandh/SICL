(in-package #:sicl-global-environment)

;;;; This file contains definitions of classes, functions, and
;;;; variables for creating and manipulating global environments.  It
;;;; contains no definitions involving symbols in the COMMON-LISP
;;;; package.  Such definitions are located in separate files. 

;;;; An environment contains ENTRIES of various kinds.  An entry can
;;;; be a BASE ENTRY.  This is the kind of entry used for variables,
;;;; functions, and macros, but also for autonomous declarations such
;;;; as OPTIMIZE.  Other entries are AUXILIARY entries, in that they
;;;; provide additional information about other entries.  This type of
;;;; entry is used for declarations of type, inline, and dynamic
;;;; extent, but a compiler-macro entry is also an auxiliary entry.
;;;;
;;;; When the information about some entity is wanted at some point in
;;;; the compilation process, the current environment is first
;;;; searched for a BASE ENTRY.  Then, it is searched again for
;;;; auxiliary entries that provide additional information about the
;;;; particular base entry that was found.  All this information is
;;;; then combined into an object called an INFO object which is
;;;; finally returned.

(deftype function-name ()
  `(or symbol (cons (eql setf) (cons symbol null))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unbound cells.
;;;
;;; We do not have a single universal bit pattern indicating that a
;;; cell is unbound.
;;; 
;;; In the native environment, we use a special immediate value for
;;; variable values and slot values.  In the cross compiler, we use a
;;; unique CONS cell.

(cl:defvar +unbound+ (list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global environment.
;;;
;;; The global environment is divided into namespaces, but a namespace
;;; may be represented in several different slots.  So for instance,
;;; what is usually considered the FUNCTION namespace, we have divided
;;; into four separate lists, consisting of global function entries,
;;; global macro entries, special operator entries and compiler-macro
;;; entries.  Likewise, what is usually considered the VARIABLE
;;; namespace, we have divided into three separate lists, consisting
;;; of special variable entries, constant variable entries and global
;;; symbol macro entries.

(defclass global-environment ()
  (;; The package namespace.  A list of packages.
   (%packages :initform '() :accessor packages)
   ;; The class namespace.  A list of classes.
   (%classes :initform '() :accessor classes)
   ;; The type namespace.
   (%types :initform '() :accessor types)
   ;; This slot holds a list of variable entries.
   (%variable-entries :initform '() :accessor variable-entries)
   ;; This slot holds a list of function entries.
   (%function-entries :initform '() :accessor function-entries)
   ;; Some entries in this list are base entries, such as OPTIMIZE and
   ;; DECLARATION.  Others are auxiliary entries such as TYPE, INLINE,
   ;; DYNAMIC-EXTENT.
   (%proclamations :initform '() :accessor proclamations)
   ;; This slot holds an association list, mapping names to SETF
   ;; expanders.
   (%setf-expanders :initform '() :accessor setf-expanders)
   ;; This slot holds an association list, mapping names to method
   ;; combinations.
   (%method-combinations :initform '() :accessor method-combinations)))

(cl:defvar *global-environment*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment entries.

(defclass entry ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; BASE-ENTRY.
;;; 
;;; A base entry is an entry that does not refer to any other entry.
;;; Entries that represent functions, variables, macros, and optimize
;;; declarations are examples of base entries. 
(defclass base-entry (entry)
  ())

(defgeneric base-entry-p (entry))

(defmethod base-entry-p (entry)
  (declare (ignore entry))
  nil)

(defmethod base-entry-p ((entry base-entry))
  (declare (ignorable entry))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; AUXILIARY-ENTRY.
;;; 
;;; An auxiliary entry contains a reference to a base entry.  Type
;;; declarations, dynamic-extent declarations, and ignore declarations
;;; are examples of auxiliary entries, because they all refer to some
;;; other entity (a variable name or a function name).
(defclass auxiliary-entry (entry)
  ((%base-entry :initarg :base-entry :reader base-entry)))

(defgeneric auxiliary-entry-p (entry))

(defmethod auxiliary-entry-p (entry)
  (declare (ignore entry))
  nil)

(defmethod auxiliary-entry-p ((entry auxiliary-entry))
  (declare (ignorable entry))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; NAMED-ENTRY.
;;; 
;;; A mixin class for entries having a name associated with them. 
(defclass named-entry ()
  ((%name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The nature of the entry.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DEFINITION-ENTRY.
;;;
;;; For entries that have a complete definition in the environment.
;;; This is the case for macros, symbol macros, and constant
;;; variables.

(defclass definition-entry ()
  ((%definition :initarg :definition :accessor definition)))

(defgeneric definition-entry-p (object))

(defmethod definition-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod definition-entry-p ((object definition-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class VARIABLE-ENTRY.

(defclass variable-entry (base-entry named-entry)
  (;; This slot contains true if and only if this entry represents a
   ;; constant variable.
   (%constantp :initform nil :accessor constantp)
   ;; This slot contains true if and only if this entry represents a
   ;; special variable.
   (%specialp :initform nil :accessor specialp)
   ;; This slot contains the value associated with the variable.  If
   ;; there is no value associated with the variable, then this slot
   ;; contains the value of the variable +UNBOUND+.
   (%value :initform +unbound+ :accessor value)
   ;; The value of this slot is either a macro function, or NIL if
   ;; this entry does not currently represent a symbol macro.
   (%macro-function :initform nil :accessor macro-function)
   ;; When this entry represents a symbol macro, then this slot
   ;; contains the expansion for that symbol macro, i.e., a form that
   ;; the symbol macro expands to.  When this entry does not represent
   ;; a symbol macro, this slot contains NIL.
   (%expansion :initform nil :accessor expansion)
   ;; The value of this slot is the proclaimed type of the variable
   ;; whenever this entry is associated with a variable.
   (%type :initform t :accessor type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-ENTRY.
;;;
;;; A function entry represents information of a name used as a
;;; function, whether it is an ordinary function, a generic function,
;;; a macro, or a special operator.
;;;
;;; Whenever some non-trivial information about some function name is
;;; required, an instance of this class is created, and then never
;;; removed.

(defclass function-entry (base-entry named-entry)
  (;; The value of this slot is a CONS cell in which the CAR is always
   ;; a function.  When the function is unbound, it contains the
   ;; function in the UNBOUND slot, which signals an error.
   (%function-cell :initform (list nil) :reader function-cell)
   ;; The value of this slot is a function that signals an error
   ;; indicating that the function is unbound.  The function
   ;; represented by this entry is unbound if and only if the function
   ;; in this slot is the contents of the CAR of the function cell in
   ;; the FUNCTION-CELL slot.
   (%unbound :reader unbound)
   ;; The value of this slot is either a macro function, or NIL if
   ;; this entry does not currently represent a macro. 
   (%macro-function :initform nil :accessor macro-function)
   ;; The value of this slot is a generalized boolean indicating
   ;; whether this entry represents a special operator.
   (%special-operator :initform nil :accessor special-operator)
   ;; The value of this slot is either a compiler macro function, or
   ;; NIL if this entry does not currently have a compiler macro
   ;; associated with it.
   (%compiler-macro-function :initform nil :accessor compiler-macro-function)
   ;; The value of this slot is the proclaimed type of the function
   ;; whenever this entry has a function associated with it.
   (%type :initform t :accessor type)
   ;; The value of this slot is the proclaimed inline information
   ;; whenever this entry has a function associated with it.
   (%inline :initform nil :accessor inline))

(defgeneric function-entry-p (object)
  (:method (object)
    (declare (ignore object))
    nil)
  (:method ((object function-entry))
    (declare (ignorable object))
    t))

(defun find-function-entry (environment name)
  (find name (function-entries environment)
	:test #'equal :key #'name))

(defun ensure-function-entry (environment name)
  (let ((entry (find-function-entry environment name)))
    (when (null entry)
      (setf entry
	    (make-instance 'function-entry))
      (push entry (function-entries environment)))
    entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPE-ENTRY.

(defclass type-entry
    (base-entry named-entry definition-entry)
  ())

(defgeneric type-entry-p (object))

(defmethod type-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod type-entry-p ((object type-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declaration entries.

(defclass declaration-entry (entry) ())

(defgeneric declaration-entry-p (object))

(defmethod declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod declaration-entry-p ((object declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPE-DECLARATION-ENTRY.
;;;
;;; We do not have a separate declaration entry for FTYPE.

(defclass type-declaration-entry (auxiliary-entry)
  ((%type :initarg :type :reader type)))

(defgeneric type-declaration-entry-p (object))

(defmethod type-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod type-declaration-entry-p ((object type-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class AUTONOMOUS-DECLARATION-ENTRY.
;;;
;;; This class is the base class for declaration entries that are base
;;; entries, i.e., declaration entries that do not refer to any other
;;; entry.

(defclass autonomous-declaration-entry (declaration-entry)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPTIMIZE-DECLARATION-ENTRY.

(defclass optimize-declaration-entry (base-entry)
  ((%quality :initarg :quality)
   (%value :initarg :value)))

(defgeneric optimize-declaration-entry-p (object))

(defmethod optimize-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod optimize-declaration-entry-p
    ((object optimize-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DECLARATION-DECLARATION-ENTRY.

(defclass declaration-declaration-entry (base-entry named-entry)
  ((%name :initarg :name :reader name)))

(defgeneric declaration-declaration-entry-p (object))

(defmethod declaration-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod declaration-declaration-entry-p
    ((object declaration-declaration-entry))
  (declare (ignorable object))
  t)
