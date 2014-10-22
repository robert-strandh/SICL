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

;;; For function entries, we use a particular function to mean that
;;; the function entry with a particular name is not fbound.  That
;;; particular function accepts an arbitrary number of arguments, and
;;; signals an error.  With this method, code that calls a named
;;; function in the normal way, does not have to check whether the
;;; cell contains a valid function.

(cl:defvar +funbound+
  (lambda (&rest arguments)
    (declare (ignore arguments))
    ;; Fixme, do something much smarter here.
    (error "Attempt to call an undefined function.")))

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
   ;; This slot holds a list of special variable entries.  These
   ;; entries are all base entries.
   (%special-variables :initform '() :accessor special-variables)
   ;; This slot holds a list of constant variable entries.  These
   ;; entries are all base entries.
   (%constant-variables :initform '() :accessor constant-variables)
   ;; This slot holds a list of global symbol macro entries.  These
   ;; entries are all base entries.
   (%symbol-macros :initform '() :accessor symbol-macros)
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
;;; Locations. 
;;;
;;; Locations are the values of slots of entries representing places
;;; that need to be accessed at runtime.  This is the case for
;;; variables (special and lexical) and functions (global and
;;; lexical).  A location can be a global location, a lexical
;;; location, or a special location.

(defclass location ()
  ((%name :initarg :name :reader name)))

;;; A global location is a location that has global storage associated
;;; with it, and that storage is allocated directly in the global
;;; environment.  This is the case for globally defined functions.
(defclass global-location (location)
  ((%storage :initarg :storage :initform (list +funbound+) :reader storage)))

;;; Like a global location, a special location also has storage
;;; associated with it, but it serves a somewhat different purpose.
;;; It is used only when an attempt to find a special binding in the
;;; dynamic environment fails.  
(defclass special-location (location)
  ((%storage :initform (list +unbound+) :reader storage)))

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
;;; LOCATION-ENTRY.
;;;
;;; This type of entry has some kind of location associated with it.
;;; This is the case for special variables, lexical variables, and
;;; functions.

(defclass location-entry ()
  ((%location :initarg :location :reader location)))

(defgeneric location-entry-p (object))

(defmethod location-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod location-entry-p ((object location-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-VARIABLE-ENTRY.
;;;
;;; These entries are base entreis.  They occur in a list contained in
;;; the SPECIAL-OPERATORS slot of the global environment.  A constant
;;; variable entry belongs to the variable namespace.  It does not
;;; require any storage to be accessed at runtime becuase its value is
;;; propagated at compile time.

(defclass constant-variable-entry (base-entry named-entry definition-entry)
  ())

(defgeneric constant-variable-entry-p (object))

(defmethod constant-variable-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod constant-variable-entry-p ((object constant-variable-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-VARIABLE-ENTRY.
;;;
;;; Special variable entries are base entries.  They occur in a list
;;; contained in the SPECIAL-VARIABLES slot of the global environment,
;;; but they can also occur in a local environment.  a special
;;; variable entry belongs to the variable namespace.
;;;
;;; A special variable entry can be in two stages of existence. 
;;;
;;;  1: It exists, and it has a location associated with it, but
;;;     it it has not been DEFINED. 
;;;
;;;  2: It exists, it has a location associated with it, and it has
;;;     been DEFINED.
;;;
;;; A special variable entry in stage 1 is created for a variable in
;;; two different situations: The first situation is when a
;;; proclamation that refers to it is evaluated, typically stating the
;;; type for the variable.  The second situation is when the compiler
;;; sees a free variable for which no entry exists, or when it sees a
;;; SPECIAL declaration of a variable for which no entry exists.  Code
;;; that refers to such a variable must be able to refer to the global
;;; value of the variable in case it is not bound in the dynamic
;;; environment.  The code does that by keeping a copy of the storage
;;; cell associated with the variable in its linkage vector, so the
;;; location must exist in this stage.  Notice that the storage must
;;; exist even though the variable has not been DEFINED, because it
;;; might become DEFINED after the code has been compiled, and there
;;; is no way to modify the code a posteriori so that it refers to a
;;; location that initially did not exist.  Whenever the compiler
;;; processes a variable binding, it checks whether a globally special
;;; variable with that name exists (i.e., the entry exists in stage
;;; 2), and if so, uses its special binding.  An entry in stage 1 does
;;; not trigger that behavior in the compiler, so if the entry found
;;; is in stage 1, any new binding of the variable is considered to be
;;; a lexical binding.
;;;
;;; Stage 2 happens when a DEFVAR or DEFPARAMETER form has been
;;; evaluated that refers to the variable.  The main difference with
;;; stage 1, is that in stage 2, when the compiler processes a binding
;;; of a variable with this name, then the binding is always a special
;;; binding.

(defclass special-variable-entry
    (base-entry named-entry location-entry)
  ((%defined-p :initform nil :initarg :defined-p :accessor defined-p)))

(defgeneric special-variable-entry-p (object))

(defmethod special-variable-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod special-variable-entry-p ((object special-variable-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-VARIABLE-ENTRY.
;;;

(defclass lexical-variable-entry
    (base-entry named-entry location-entry)
  ())

(defgeneric lexical-variable-entry-p (object))

(defmethod lexical-variable-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod lexical-variable-entry-p ((object lexical-variable-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SYMBOL-MACRO-ENTRY.
;;;

(defclass symbol-macro-entry
    (base-entry named-entry definition-entry)
  ())

(defgeneric symbol-macro-entry-p (object))

(defmethod symbol-macro-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod symbol-macro-entry-p ((object symbol-macro-entry))
  (declare (ignorable object))
  t)

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
   (%type :initform t :accessor type)))

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

(defclass location-declaration-entry (declaration-entry)
  ((%location :initarg :location :reader location)))

(defgeneric location-declaration-entry-p (object))

(defmethod location-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod location-declaration-entry-p ((object location-declaration-entry))
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
;;; Class INLINE-OR-NOTINLINE-DECLARATION-ENTRY.
;;;
;;; This class is not meant to be directly instantiated.  It just
;;; serves as a base class for the two classes
;;; INLINE-DECLARATION-ENTRY and NOTINLINE-DECLARATION-ENTRY. 

(defclass inline-or-notinline-declaration-entry (auxiliary-entry)
  ())

(defgeneric inline-or-notinline-declaration-entry-p (object))

(defmethod inline-or-notinline-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod inline-or-notinline-declaration-entry-p
    ((object inline-or-notinline-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INLINE-DECLARATION-ENTRY.

(defclass inline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defgeneric inline-declaration-entry-p (object))

(defmethod inline-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod inline-declaration-entry-p ((object inline-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NOTINLINE-DECLARATION-ENTRY.

(defclass notinline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defgeneric notinline-declaration-entry-p (object))

(defmethod notinline-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod notinline-declaration-entry-p ((object notinline-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DYNAMIC-EXTENT-DECLARATION-ENTRY.

(defclass dynamic-extent-declaration-entry (auxiliary-entry)
  ())

(defgeneric dynamic-extent-declaration-entry-p (object))

(defmethod dynamic-extent-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod dynamic-extent-declaration-entry-p
    ((object dynamic-extent-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IGNORE-DECLARATION-ENTRY.

(defclass ignore-declaration-entry (auxiliary-entry)
  ())

(defgeneric ignore-declaration-entry-p (object))

(defmethod ignore-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod ignore-declaration-entry-p ((object ignore-declaration-entry))
  (declare (ignorable object))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IGNORABLE-DECLARATION-ENTRY.

(defclass ignorable-declaration-entry (auxiliary-entry)
  ())

(defgeneric ignorable-declaration-entry-p (object))

(defmethod ignorable-declaration-entry-p (object)
  (declare (ignore object))
  nil)

(defmethod ignorable-declaration-entry-p ((object ignorable-declaration-entry))
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
