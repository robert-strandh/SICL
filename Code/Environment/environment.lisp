(in-package #:sicl-compiler-environment)

;;;; An environment contains ENTRIES of various kinds.  An entry can
;;;; be a BASE ENTRY.  This is the kind of entry used for variables,
;;;; functions, and macros, but also for autonomous declarations such
;;;; as OPTIMIZE.  Other entries are AUXILIARY entries, in that they
;;;; provide additional information about other entries.  This type of
;;;; entry is used for declarations of type, inline, and dynamic
;;;; extent, but a compiler-macro entry is also an auxiliary entry.
;;;; The class of a BASE ENTRY determines for which namespace it is
;;;; relevant.
;;;;
;;;; When the information about some entity is wanted at some point in
;;;; the compilation process, the current environment is first
;;;; searched for a BASE ENTRY.  Then, it is searched again for
;;;; auxiliary entries that provide additional information about the
;;;; particular base entry that was found.  All this information is
;;;; then combined into an object called an INFO object which is
;;;; finally returned.
;;;;
;;;; An environment is divided into disconneted parts.  
;;;;
;;;; One part is always the same, and that is the global environment,
;;;; which is the value of the variable *global-environment*.  The
;;;; global environment is a class instance and it is divided into
;;;; namespaces.  
;;;;
;;;; For compilation, another part consists of the extensions to the
;;;; startup environment that happen when lexial variables, functions,
;;;; macros etc, are introduced during the compilation of a form.
;;;; This part is represented as a simply linked list of entries, with
;;;; no regard to namespaces.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; In the native compiler, we use a special immediate value to
;;; indicate that some location is unbound.  In the cross compiler, we
;;; use a unique CONS cell. 

(cl:defvar +unbound+ (list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global environment.
;;;
;;; The global envrionment is divided into namespaces, but a namespace
;;; may be represented in several different slots.  So for instance,
;;; what is usually considered the FUNCTION namespace, we have divided
;;; into four separate lists, consisting of global function entries,
;;; global macro entries, special operator entries and compiler-macro
;;; entries.  Likewise, what is usually considered the VARIABLE
;;; namespace, we have divided into three separate lists, consisting
;;; of special variable entries, constant variable entries and global
;;; symbol macro entries.
;;;
;;; The global environment also contains storage cells for functions
;;; that must be directly accessible when a FASL file is loaded.  The
;;; normal way for a FASL file to find functions and variables is:
;;;
;;;  1. Build a string for the package name and use FIND-PACKAGE to
;;;     find the package with that name.
;;;
;;;  2. Build a string for the symbol name and use INTERN to find the
;;;     symbol. 
;;;
;;;  3. Optionally use CONS to build a list representing a function
;;;     name of the form (SETF <symbol>).
;;;     
;;;  4. Use either FIND-FUNCTION-CELL or FIND-VALUE-CELL to get to
;;;     either the function cell or the value cell corresponding to
;;;     the name. 
;;;
;;; Clearly, FIND-PACKAGE, INTER, CONS, FIND-FUNCTION-CELL, and
;;; FIND-VALUE-CELL must then all be accessible some other way.  We
;;; store their function cells each in a separate slot in the
;;; environment.  Furthermore, MAKE-STRING and (SETF SCHAR) must be
;;; available this way, so that the initial strings can be built.  By
;;; storing their cells rather than their values, we make it possible
;;; to redefine them later on.  We take advantage of this possibility
;;; by starting off with embryonic versions of these functions that
;;; will later be replaced by final versions as the bootstrapping
;;; process finishes.  Finally, the symbol NIL must be available to
;;; build lists.

(defclass global-environment ()
  (;; The symbol NIL.
   (%symbol-nil :accessor symbol-nil)
   ;; The storage cell for MAKE-STRING.
   (%cell-of-make-string :accessor cell-of-make-string)
   ;; The storage cell for SCHAR.
   (%cell-of-setf-schar :accessor cell-of-setf-schar)
   ;; The storage cell for FIND-PACKAGE.
   (%cell-of-find-package :accessor cell-of-find-package)
   ;; The storage cell for INTERN.
   (%cell-of-intern :accessor cell-of-intern)
   ;; The storage cell for CONS.
   (%cell-of-cons :accessor cell-of-cons)
   ;; The storage cell for FIND-FUNCTION-CELL.
   (%cell-of-find-function-cell :accessor cell-of-find-function-cell)
   ;; The storage cell for FIND-VALUE-CELL.
   (%cell-of-find-value-cell :accessor cell-of-find-value-cell)
   ;; The package namespace.  A list of packages.
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
   ;; This slot holds a list of global function entries.  These entries
   ;; are all base entries.
   (%functions :initform '() :accessor functions)
   ;; This slot holds a list of global macro entries.  These entries
   ;; are all base entries.
   (%macros :initform '() :accessor macros)
   ;; This slot holds a list of special operator entries.  These
   ;; entries are all base entries.
   (%special-operators :initform '() :accessor special-operators)
   ;; This slot holds a list of compiler macro entries.  These entries
   ;; are auxiliary entries, and an entry in this list refers to a
   ;; base entry either in the FUNCTIONS slot, or in the MACROS slot.
   (%compiler-macros :initform '() :accessor compiler-macros)
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

(cl:defparameter *global-environment*
  (make-instance 'global-environment))

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
  ((%storage :initarg :storage :initform (list +unbound+) :reader storage)))

(defun make-global-location (name)
  (make-instance 'global-location :name name))

;;; Like a global location, a special location also has storage
;;; associated with it, but it serves a somewhat different purpose.
;;; It is used only when an attempt to find a special binding in the
;;; dynamic environment fails.  
(defclass special-location (location)
  ((%storage :initform (list +unbound+) :reader storage)))

(defun make-special-location (name)
  (make-instance 'special-location :name name))

;;; A lexical location does not have any storage associated with it.
;;; The storage for a lexical location is determined by the compiler,
;;; and it can sometimes be in a local lexical environment, sometimes
;;; on the stack and sometimes in a register.  The name of a lexical
;;; location is just used to display it in error messages and for
;;; debugging purposes.
(defclass lexical-location (location)
  ())

(defun make-lexical-location (name)
  (make-instance 'lexical-location :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Entries in the package namespace.
 
(defclass package ()
  ((%name :initarg :name :accessor name)
   (%nicknames :initarg :nicknames :initform '() :accessor nicknames)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Augmenting an environment.

(defun add-to-environment (environment entry)
  (cons entry environment))

(defun augment-environment (environment entries)
  (append entries environment))

(defun augment-environment-with-declarations (environment declarations)
  (let ((declaration-specifiers
	  (sicl-code-utilities:canonicalize-declaration-specifiers
	   (reduce #'append (mapcar #'cdr declarations)))))
    (augment-environment
     environment
     (loop for spec in declaration-specifiers
	   collect (make-entry-from-declaration spec environment)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Environment entries.

(defclass entry ()
  ())

;;; A base entry is an entry that does not refer to any other entry.
;;; Entries that represent functions, variables, macros, and optimize
;;; declarations are examples of base entries. 
(defclass base-entry (entry)
  ())

;;; An auxiliary entry contains a reference to a base entry.  Type
;;; declarations, dynamic-extent declarations, and ignore declarations
;;; are examples of auxiliary entries, because they all refer to some
;;; other entity (a variable name or a function name).
(defclass auxiliary-entry (entry)
  ((%base-entry :initarg :base-entry :reader base-entry)))

;;; A mixin class for entries having a name associated with them. 
(defclass named-entry ()
  ((%name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We define a mixin class for each namespace.  In the local
;;; environment, entries are just a linked list, so the namespaces are
;;; all mixed up.  By making the namespace part of the entry type, we
;;; can locate entries in a particular namespace in the local
;;; environment.

(defclass function-space () ())
(defclass variable-space () ())
(defclass block-space () ())
(defclass tag-space () ())
(defclass type-space () ())
(defclass declaration-space () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The nature of the entry.

;;; For entries that have a complete definition in the environment.
;;; This is the case for macros, symbol macros, constant variables,
;;; blocks, and go tags.
(defclass definition-entry ()
  ((%definition :initarg :definition :accessor definition)))

;;; This type of entry has some kind of location associated with it.
;;; This is the case for special variables, lexical variables, and
;;; functions.
(defclass location-entry ()
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-VARIABLE-ENTRY.
;;;
;;; These entries are base entreis.  They occur in a list contained in
;;; the SPECIAL-OPERATORS slot of the global environment.  A constant
;;; variable entry belongs to the variable namespace.  It does not
;;; require any storage to be accessed at runtime becuase its value is
;;; propagated at compile time.

(defclass constant-variable-entry
    (base-entry named-entry variable-space definition-entry)
  ())

(defun make-constant-variable-entry (name definition)
  (make-instance 'constant-variable-entry
		 :name name
		 :definition definition))

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
    (base-entry named-entry variable-space location-entry)
  ((%defined-p :initform nil :initarg :defined-p :accessor defined-p)))

(defun make-special-variable-entry (name &optional defined-p)
  (make-instance 'special-variable-entry
		 :name name
		 :location (make-special-location name)
		 :defined-p defined-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-VARIABLE-ENTRY.
;;;

(defclass lexical-variable-entry
    (base-entry named-entry variable-space location-entry)
  ())

(defun make-lexical-variable-entry (name)
  (make-instance 'lexical-variable-entry
		 :name name
		 :location (make-lexical-location name)))

(defun add-lexical-variable-entry (env name)
  (add-to-environment env (make-lexical-variable-entry name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SYMBOL-MACRO-ENTRY.
;;;

(defclass symbol-macro-entry
    (base-entry named-entry variable-space definition-entry)
  ())

(defun make-symbol-macro-entry (name expansion)
  (let ((expander (lambda (form environment)
		    (declare (ignore form environment))
		    expansion)))
    (make-instance 'symbol-macro-entry
		   :name name
		   :definition expander)))

(defun add-symbol-macro-entry (env name expansion)
  (add-to-environment env (make-symbol-macro-entry name expansion)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FUNCTION-ENTRY.
;;;
;;; This class is not meant to be instantiated directly, and exists
;;; only as the common parent class of the classes
;;; GLOBAL-FUNCTION-ENTRY and LOCAL-FUNCTION-ENTRY.

(defclass function-entry
    (base-entry named-entry function-space location-entry)
  (;; The AST is present if the function was declared INLINE when it
   ;; was defined.  Otherwise, the value of this slot is NIL.
   (%ast :initform nil :initarg :ast :accessor ast)
   ;; When the AST i present (i.e., non-NIL), this slot is valid and
   ;; contains a list of LEXICAL-ASTs, one for each required
   ;; parameter.
   (%parameters :initform nil :initarg :parameters :accessor parameters)
   (%lambda-list :initform :none :initarg :lambda-list :accessor lambda-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-FUNCTION-ENTRY.
;;;
;;; Global function entries are base entries.  They occur in the list
;;; contained in the FUNCTIONS slot of a global environment.  A global
;;; function entry represents a globally defined ordinary function, as
;;; opposed to a macro or a special operator.
;;;
;;; If a global function entry exists for some name N, then there can
;;; not simultaneously be a special operator entry for N.  An attempt
;;; to create a global function entry when there is already a special
;;; operator entry will fail. 
;;; 
;;; Creating a global function entry with a name N when there is
;;; already a global macro entry with the name N causes the global
;;; macro entry to be removed.  However, creating a global macro entry
;;; with a name N when there is already a global function entry with a
;;; name N doesn not cause the global function entry to be removed.
;;; The reason for not removing it is that it might be referred to by
;;; existing code and when that code was compiled, it was assumed that
;;; the name N referred to a function.  For that reason, the entry can
;;; not be removed.  Therefore, it is possible that there
;;; simultaneously exist a global macro entry and a global function
;;; entry for the same name N.  However, in that case, the storage
;;; cell of the location of the global function entry always contains
;;; +unbound+.
;;;
;;; A global function entry can come into existence in several ways:
;;; 
;;;  * Using (SETF FDEFINITION) on a name that is not the name of a
;;;    special operator.  A LOCATION for the entry is created, and the
;;;    storage cell will be set to the new definition.
;;;
;;;  * Proclaiming FTYPE, INLINE, NOTINLINE or DYNAMIC extent with
;;;    FUNCTION using the name.  Again, A LOCATION for the entry is
;;;    created, but the storage cell will be set +unbound+.  The
;;;    appropriate auxiliary entry is created and will refer to the
;;;    base entry.
;;;
;;;  * When the compiler sees a compound form with the CAR containing
;;;    a symbol that is not associated with an entry in the FUNCTION
;;;    namespace.  In this case, a LOCATION for the entry is created
;;;    and the storage cell of the entry is initialized to +unbound+.
;;;    A warning is also signaled, indicating that the function is
;;;    undefined.
;;;
;;; A global function entry is never removed for reasons mentioned
;;; above.

(defclass global-function-entry (function-entry)
  ())

(defun make-global-function-entry (name)
  (make-instance 'global-function-entry
		 :name name
		 :location (make-global-location name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOCAL-FUNCTION-ENTRY.

(defclass local-function-entry (function-entry)
  ())

(defun make-local-function-entry (name)
  (make-instance 'local-function-entry
		 :name name
		 :location (make-lexical-location name)))

(defun add-local-function-entry (env name)
  (add-to-environment env (make-local-function-entry name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MACRO-ENTRY.
;;;
;;; This class is not meant to be instantiated directly, and exists
;;; only as the common parent class of the classes
;;; GLOBAL-MACRO-ENTRY and LOCAL-MACRO-ENTRY.

(defclass macro-entry
    (base-entry named-entry function-space definition-entry)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-MACRO-ENTRY.
;;;
;;; Global macro entries are base entries.  They occur in list
;;; contained in the MACROS slot of a global environment.  A global
;;; macro entry represents a globally defined macro, as opposed to an
;;; ordinary function or a special operator.
;;;
;;; There can simultaneously be a global macro entry and either a
;;; special operator entry or a global function entry (but not both)
;;; with the same name.  The HyperSpec specifically allows for a macro
;;; and a special operator for the same name to exist.  A global
;;; function entry can exist at the same time as a global macro entry
;;; as a result of the global macro entry being created using (SETF
;;; MACRO-FUNCTION) but the global function entry could not be removed,
;;; for reasons indicated above.
;;;
;;; The only type of auxiliary entry that can refer to a global macro
;;; entry is a compiler-macro entry.
;;;
;;; A global macro entry can only come into existence by the use of
;;; (SETF MACRO-FUNCTION).  
;;;
;;; A global macro entry may be removed in the following situations: 
;;;
;;;  * When (SETF FDEFINITION) is used to either create a global
;;;    function entry, or to making an existing global function entry
;;;    FBOUND by storing a new definition in its storage cell. 
;;;
;;;  * As a result of calling FMAKUNBOUND on the name of the entry. 

(defclass global-macro-entry (macro-entry)
  ())

(defun make-global-macro-entry (name expander)
  (make-instance 'global-macro-entry
		 :name name
		 :definition expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOCAL-MACRO-ENTRY.

(defclass local-macro-entry (macro-entry)
  ())

(defun make-local-macro-entry (name expander)
  (make-instance 'local-macro-entry
		 :name name
		 :definition expander))

(defun add-local-macro-entry (env name expander)
  (add-to-environment env (make-local-macro-entry name expander)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-OPERATOR-ENTRY.
;;;
;;; Special operator entries are base entries.  They occur in the list
;;; contained in the SPECIAL-OPERATORS list of a global environment.
;;; A special operator entry represents a special operator, as opposed
;;; to an ordinary function or a macro.
;;;
;;; The HyperSpec makes no provision for creating or removing special
;;; operators, so we assume that all special operator entries that
;;; will ever exist are created when the global environment is
;;; created.

(defclass special-operator-entry
    (base-entry named-entry function-space)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class COMPILER-MACRO-ENTRY.
;;;
;;; The HyperSpec is very skimpy when it comes to compiler macros.  In
;;; section 3.2.2.1 it says that "A compiler macro can be defined for
;;; a name that also names a function or a macro."  This sentence can
;;; be interpreted in several ways, but we take it to mean that when a
;;; compiler macro is defined, there must already be a function or a
;;; macro with the same name.  As a consequence, we make compiler
;;; macro entries AUXILIARY, and such an entry refers either to a
;;; global function entry or to a global macro entry.
;;;
;;; Compiler macro entries are thus auxiliary entries.  They occur in
;;; the list contained in the COMPILER-MACROS slot of a global
;;; environment.  A compiler macro entry refers either to a global
;;; function entry or to a global macro entry.  Compiler macros are by
;;; definition global.  The HyperSpec makes no provision for creating
;;; local compiler macros.
;;; 
;;; A compiler macro entry is created as a result of a call to (SETF
;;; COMPILER-MACRO-FUNCTION).  If there is a global macro entry with
;;; the same name, then that global macro entry becomes the base entry
;;; for the compiler macro entry.  If not, and there is a global
;;; function entry with the same name, and that global function entry
;;; has a location where the storage cell is not +unbound+, then that
;;; global function entry becomes the base entry for the compiler
;;; macro entry.
;;; 
;;; It appears from the HyperSpec that a compiler macro can be removed
;;; by giving the argument NIL to (SETF COMPILER-MACRO-FUNCTION).  As
;;; a consequence, the entry (if it exists) is removed as a result of
;;; such a call.  A compiler macro entry is also removed as a result
;;; of calling FMAKUNBOUND on the name, and as a result of calling
;;; (SETF MACRO-FUNCTION) when base entry of the compiler macro entry
;;; is a global function entry, and as a result of calling (SETF
;;; FDEFINITION) or (SETF SYMBOL-FUNCTION) when the base entry of the
;;; compiler entry is a global macro entry.

(defclass compiler-macro-entry
    (auxiliary-entry function-space definition-entry)
  ())

(defun make-compiler-macro-entry (base-entry expander)
  (make-instance 'compiler-macro-entry
		 :base-entry base-entry
		 :definition expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-ENTRY.

(defclass block-entry
    (base-entry named-entry block-space definition-entry)
  ())
  
(defun make-block-entry (name block)
  (make-instance 'block-entry
		 :name name
		 :definition block))

(defun add-block-entry (env name block)
  (add-to-environment env (make-block-entry name block)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-TAG-ENTRY.

(defclass go-tag-entry
    (base-entry named-entry tag-space definition-entry)
  ())

(defun make-go-tag-entry (name tag)
  (make-instance 'go-tag-entry
		 :name name
		 :definition tag))

(defun add-go-tag-entry (env name tag)
  (add-to-environment env (make-go-tag-entry name tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPE-ENTRY.

(defclass type-entry
    (base-entry named-entry type-space definition-entry)
  ())

(defun make-type-entry (name expander)
  (make-instance 'type-entry
		 :name name
		 :definition expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Declaration entries.

(defclass declaration-entry (entry) ())

(defclass location-declaration-entry (declaration-entry)
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPE-DECLARATION-ENTRY.
;;;
;;; We do not have a separate declaration entry for FTYPE.

(defclass type-declaration-entry (auxiliary-entry)
  ((%type :initarg :type :reader type)))

(defun make-type-declaration-entry (location-entry type)
  (make-instance 'type-declaration-entry
		 :base-entry location-entry
		 :type type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INLINE-OR-NOTINLINE-DECLARATION-ENTRY.
;;;
;;; This class is not meant to be directly instantiated.  It just
;;; serves as a base class for the two classes
;;; INLINE-DECLARATION-ENTRY and NOTINLINE-DECLARATION-ENTRY. 

(defclass inline-or-notinline-declaration-entry (auxiliary-entry)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class INLINE-DECLARATION-ENTRY.

(defclass inline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defun make-inline-declaration-entry (location-entry)
  (make-instance 'inline-declaration-entry
		 :location (location location-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NOTINLINE-DECLARATION-ENTRY.

(defclass notinline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defun make-notinline-declaration-entry (location-entry)
  (make-instance 'notinline-declaration-entry
		 :location (location location-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DYNAMIC-EXTENT-DECLARATION-ENTRY.

(defclass dynamic-extent-declaration-entry (auxliary-entry)
  ())

(defun make-dynamic-extent-declaration-entry (location-entry)
  (make-instance 'dynamic-extent-declaration-entry
		 :location (location location-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IGNORE-DECLARATION-ENTRY.

(defclass ignore-declaration-entry (auxiliary-entry)
  ())

(defun make-ignore-declaration-entry (location-entry)
  (make-instance 'ignore-declaration-entry
		 :location (location location-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IGNORABLE-DECLARATION-ENTRY.

(defclass ignorable-declaration-entry (auxiliary-entry)
  ())

(defun make-ignorable-declaration-entry (location-entry)
  (make-instance 'ignorable-declaration-entry
		 :location (location location-entry)))

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

(defun make-optimize-declaration-entry (quality &optional (value 3))
  (make-instance 'optimize-declaration-entry
		 :quality quality
		 :value value))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DECLARATION-DECLARATION-ENTRY.

(defclass declaration-declaration-entry (base-entry named-entry)
  ((%name :initarg :name :reader name)))

(defun make-declaration-declaration-entry (name)
  (make-instance 'declaration-declaration-entry
		 :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Info classes.
;;;
;;; We return an instance of an info class as a result of a query. 

(defclass info () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Definition info classes.

(defclass definition-info (info)
  ((%name :initarg :name :reader name)
   (%definition :initarg :definition :reader definition)))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-VARIABLE-INFO.

(defclass constant-variable-info (definition-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MACRO-INFO.

(defclass macro-info (definition-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SYMBOL-MACRO-INFO.
;;;
;;; The HyperSpec says that it is allowed to declare the type of a
;;; symbol macro.  Such a declaration is "equivalent to wrapping a THE
;;; expression around the expansion of that symbol, although the
;;; symbol's macro expansion is not actually affected."

(defclass symbol-macro-info (info)
  ((%type :initarg :type :reader type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-INFO.

(defclass block-info (definition-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TAG-INFO.

(defclass tag-info (definition-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location info classes.
;;;
;;; These classes represent information about a location.

(defclass location-info (info)
  ((%location :initarg :location :reader location)
   (%type :initarg :type :reader type)
   (%ignore-info :initarg :ignore-info :reader ignore-info)
   (%dynamic-extent-p :initarg :dynamic-extent-p :reader dynamic-extent-p)))

(defclass function-info-mixin ()
  ((%inline-info :initarg :inline-info :reader inline-info)
   (%lambda-list :initarg :lambda-list :reader lambda-list)
   (%ast :initarg :ast :reader ast)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-LOCATION-INFO.

(defclass special-location-info (location-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-LOCATION-INFO.

(defclass lexical-location-info (location-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-FUNCTION-LOCATION-INFO.

(defclass lexical-function-location-info
    (lexical-location-info function-info-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-LOCATION-INFO.

(defclass global-location-info (location-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-FUNCTION-LOCATION-INFO.

(defclass global-function-location-info
    (global-location-info function-info-mixin)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro expansion.

(defun macro-function (symbol &optional env)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'macro-entry)
			       (eq (name entry) symbol)))
			(append env (macros *global-environment*)))))
    (if (null entry)
	nil
	(definition entry))))

(cl:defparameter *macroexpand-hook*
  (lambda (macro-function macro-form environment)
    (funcall macro-function macro-form environment)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MACROEXPAND-1.
;;;
;;; This function returns two values, the expanded form and a boolean
;;; indicating whether the form was expanded or not.  I can't think of
;;; a reason for the existence of the second return value, because if
;;; the form is a macro form, then the expander is called and the
;;; second value is true.  If the form is not a macro form, then the
;;; initial form is return and the second value is false.  If the
;;; expander should ever return the same form, then MACROEXPAND-1 will
;;; be called again (at least if it was called by MACROEXPAND) and we
;;; would have an infinite computation.  Though I suppose that if the
;;; expander did some non-functional stuff like consulting global
;;; variables, then, it might be possible. 

(defun macroexpand-1 (form &optional env)
  (let ((expander nil))
    (cond
      ((and (consp form) (symbolp (car form)))
       (setf expander (macro-function (car form) env)))
      ((symbolp form)
       (let ((entry (find-if (lambda (entry)
			       (and (typep entry 'symbol-macro-entry)
				    (eq (name entry) form)))
			     (append env
				     (symbol-macros *global-environment*)))))
	 (if (null entry)
	     nil
	     (setf expander (definition entry)))))
      (t nil))
    (if (null expander)
	(values form nil)
	(values (funcall (coerce *macroexpand-hook* 'function)
			 expander
			 form
			 env)
		t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MACROEXPAND.
;;;
;;; The HyperSpec says that this function repeatedly calls
;;; MACROEXPAND-1 until the second return value is false, so that is
;;; what we do too.
;;;
;;; However, that is not what the compiler should do.  The reason is
;;; that MACROEXPAND-1 might very well return another macro form that
;;; in addition has a compiler macro associated with it.  In that
;;; case, it would be inappropriate (though technically not "wrong")
;;; for the compiler to call MACROEXPAND-1 again.  Instead, it should
;;; apply the compiler macro first and then expand again.  In fact,
;;; the application of the compiler macro function might return
;;; something other than a macro form. It could also be the case that
;;; what MACROEXPAND-1 returns is NOT a macro form, but it is a
;;; function-call form with a compiler macro associated with it, and
;;; the compiler macro might return a macro form.
;;;
;;; For reasons mentioned in the previous paragraph, the compiler does
;;; not call macroexpand at all, but instead a function called
;;; FULLY-EXPAND-FORM defined below.

(defun macroexpand (form &optional environment)
  (multiple-value-bind (expansion expanded-p)
      (macroexpand-1 form environment)
    (if expanded-p
	(loop while (multiple-value-bind (new-expansion expanded-p)
			(macroexpand-1 expansion environment)
		      (setf expansion new-expansion)
		      expanded-p)
	      finally (return (values expansion t)))
	(values form nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Compiler macros.

;;; Since a compiler macro entry exists only if the name is fbound,
;;; this means that if the compiler macro entry exists, it refers
;;; either to a global macro entry, or to a global function entry
;;; which does not have +unbound+ in its storage cell. 
;;;
;;; I am not sure what the optional environment argument could be.  It
;;; seems to me that it must either be a global environment, i.e. an
;;; instance of GLOBAL-ENVIRONMENT, a local environment, or NIL.  In
;;; the last two cases, unless I am wrong, the value of
;;; *global-envrionment* must be used. 
(defun compiler-macro-function (name &optional environment)
  (unless (typep environment 'global-environment)
    (setf environment *global-environment*))
  (let ((entry (find-if (lambda (entry)
			  (eq (name (base-entry entry)) name))
			(compiler-macros environment))))
    (if (null entry)
	nil
	(definition entry))))

;;; If there is a compiler macro entry, it must refer to a base entry
;;; which is either a global macro entry or to a global function entry
;;; that is bound.  This function searches for such a base entry.
;;;
;;; FIXME: for now, don't require the function entry to be bound.
(defun find-base-entry (name environment)
  (or (find-if (lambda (entry)
		 (eq (name entry) name))
	       (macros environment))
      (find-if (lambda (entry)
		 (eq (name entry) name))
;;		 (and (eq (name entry) name)
;;		      (not (eq (car (storage (location entry))) +unbound+))))
	       (functions environment))))

(defun (setf compiler-macro-function) (new-function name &optional environment)
  (unless (null environment)
    (error "Environment object must be nil."))
  (let ((base-entry (find-base-entry name *global-environment*)))
    (when (null base-entry)
      (error "A global macro or a global function must already exist."))
    (let ((c-m-entry (find-if (lambda (entry)
				(eq (base-entry entry) base-entry))
			      (compiler-macros *global-environment*))))
      ;; Remove the old entry if there was one.
      (unless (null c-m-entry)
	(setf (compiler-macros *global-environment*)
	      (delete c-m-entry (compiler-macros *global-environment*)
		      :test #'eq)))
      ;; Add a new entry unless the new function is NIL.
      (unless (null new-function)
	(push (make-compiler-macro-entry base-entry new-function)
	      (compiler-macros *global-environment*)))))
  ;; Return the new value as required by the HyperSpec.
  new-function)

;;; FIXME: we probably won't need this function, so maybe remove it.
(defun compiler-macroexpand-1 (form &optional env)
  (if (symbolp (car form))
      (let ((entry (find-if (lambda (entry)
			      (eq (name (base-entry entry)) (car form)))
			    (compiler-macros env))))
	(if (null entry)
	    form
	    (funcall (coerce *macroexpand-hook* 'function)
		     (definition entry)
		     form
		     env)))
      form))

;;; FIXME: we probably won't need this function, so maybe remove it.
(defun compiler-macroexpand (form &optional env)
  (loop for expanded-form = (compiler-macroexpand-1 form env)
	until (eq expanded-form form)
	do (setf form expanded-form))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FULLY-EXPAND-FORM.
;;;
;;; The compiler calls this function to "fully expand" a form.  While
;;; it might seem reasonable for the compiler to call MACROEXPAND,
;;; this is not always the case.  The reason is that MACROEXPAND
;;; repeatedly calls MACROEXPAND-1 until the second return value is
;;; false.  However, this is not the right thing for the compiler to
;;; do.  The reason is that it might well be the case that
;;; MACROEXPAND-1 returns a macro form that has a compiler macro
;;; associated with it.  If that is the case, then that compiler macro
;;; should be applied first.  It could also be the case that the
;;; MACROEXPAND-1 returns a form that is not a macro form, but a
;;; function call form with a compiler macro associated with it.  That
;;; compiler macro might then return a macro form.
;;;
;;; So instead of calling MACROEXPAND, the compiler calls this
;;; function.  It does the following:
;;;
;;;   * If the form is a symbol:
;;;
;;;      - If the most recent entry for symbol in the environment is a
;;;        symbol macro, then expand the form accordingly, and
;;;        iterate.
;;;
;;;      - If there is either no entry in the environment for the
;;;        symbol, or the most recent entry is not a not a symbol
;;;        macro, then we are done, and we return the original form.
;;;
;;;   * If the form is a compound form and the CAR of the compound
;;;     form is a symbol:
;;;
;;;      - If there is a local macro or a local function for the
;;;        symbol:
;;;
;;;         + If it is a local macro, then expand the form accordingly
;;;           and iterate.
;;;
;;;         + If it is a local function, we are done, and we return
;;;           the original form.
;;;
;;;      - If there is not a local macro or a local function for the
;;;        symbol, but there is a global macro for the symbol:
;;;
;;;         + If that global macro has a compiler macro associated
;;;           with it, then call the compiler-macro function to expand
;;;           the form, and iterate. 
;;;
;;;         + If that global macro does not have a compiler macro
;;;           associated with it, then expand the macro form and
;;;           iterate.
;;;
;;;      - If there is nether a local macro, a local function, nor a
;;;        global macro for the symbol, but there is a global function
;;;        for it:
;;;
;;;         + If that global function has a compiler macro associated
;;;           with it, then call the compiler-macro function to expand
;;;           the form, and iterate. 
;;;
;;;         + If that global function does not have a compiler macro
;;;           associated with it, then we are done, and we return the
;;;           original form.
;;;
;;;      - Otherwise (i.e., there is neither a local macro, a local
;;;        function, a global macro, nor a global function for the
;;;        symbol), we are done, and return the original form.
;;;
;;;   * Otherwise (i.e., the form is neither a symbol, nor a compound
;;;     form with a symbol in its CAR), we are done, and return the
;;;     original form.

(defun fully-expand-form (form environment)
  (loop
    do (cond ((symbolp form)
	      (let ((entry
		      (find-if (lambda (entry)
				 (and (typep entry 'variable-space)
				      (eq (name entry) form)))
			       (append environment
				       ;; There is no need to include
				       ;; any other lists to search,
				       ;; because we are really just
				       ;; interested in whether there
				       ;; is a symbol macro for the
				       ;; name.
				       (symbol-macros *global-environment*)))))
		(if (and (not (null entry))
			 (typep entry 'symbol-macro-entry))
		    (setf form
			  (funcall (coerce *macroexpand-hook* 'function)
				   (definition entry)
				   form
				   environment))
		    (return-from fully-expand-form form))))
	     ((and (consp form) (symbolp (car form)))
	      (let ((entry
		      (find-if (lambda (entry)
				 (and (typep entry 'function-space)
				      (eq (name entry) (car form))))
			       ;; We are only interested in local
			       ;; function or local macros, so no need
			       ;; to include the global environment.
			       environment)))
		(if (not (null entry))
		    ;; We found and entry corresponding either to a
		    ;; local macro or a local function. 
		    (if (typep entry 'local-macro-entry)
			;; It is a local macro.  Expand it and iterate. 
			(setf form
			      (funcall (coerce *macroexpand-hook* 'function)
				       (definition entry)
				       form
				       environment))
			;; It is a local function.  We are done. 
			(return-from fully-expand-form form))
		    ;; No local macro or local function found.  Check
		    ;; whether we have a global macro.
		    (let ((entry (find (car form) (macros *global-environment*)
				       :key #'name :test #'eq)))
		      (if (not (null entry))
			  ;; We found an entry corresponding to a
			  ;; global macro.  Check whether it has a
			  ;; compiler macro associated with it.
			  (let ((c-m-entry
				  (find entry
					(compiler-macros *global-environment*)
					:key #'base-entry :test #'eq)))
			    (if (not (null c-m-entry))
				;; We found a compiler macro entry.
				;; Try to expand it.
				(let ((new-form
					(funcall (coerce *macroexpand-hook*
							 'function)
						 (definition c-m-entry)
						 form
						 environment)))
				  (if (eq form new-form)
				      ;; The compiler macro declined.
				      ;; Expand the macro instead and
				      ;; iterate.
				      (setf form 
					    (funcall (coerce *macroexpand-hook*
							     'function)
						     (definition entry)
						     form
						     environment))
				      ;; The compiler macro expanded.
				      ;; Iterate with new form.
				      (setf form new-form)))
				;; No compiler macro found.  Expand the
				;; macro instead, and iterate
				(setf form 
				      (funcall (coerce *macroexpand-hook*
						       'function)
					       (definition entry)
					       form
					       environment))))
			  ;; No global macro found.  At this point, it
			  ;; is sufficient to search for a compiler
			  ;; macro, because it we find one, then it
			  ;; must be associated with a global
			  ;; function.
			  (let ((c-m-entry
				  (find-if (lambda (entry)
					     (eq (name (base-entry entry))
						 (car form)))
					   (compiler-macros
					    *global-environment*))))
			    (if (and (not (null c-m-entry))
				     (not (find (lambda (entry)
						  (and (typep entry
							      'notinline-declaration-entry)
						       (eq (base-entry entry)
							   (base-entry c-m-entry))))
						(proclamations *global-environment*))))
				;; We found a compiler macro entry.
				;; Try to expand it.
				(let ((new-form
					(funcall (coerce *macroexpand-hook*
							 'function)
						 (definition c-m-entry)
						 form
						 environment)))
				  (if (eq form new-form)
				      ;; The compiler macro declined.
				      ;; We are done.
				      (return-from fully-expand-form form)
				      ;; The compiler macro expanded.
				      ;; Iterate with new form
				      (setf form new-form)))
				;; No compiler macro found.  We are done.
				(return-from fully-expand-form form))))))))
	     (t (return-from fully-expand-form form)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Type expansion.

(defun type-function (symbol)
  (let ((entry (find symbol (types *global-environment*)
		     :key #'name :test #'eq)))
    (if (null entry)
	nil
	(definition entry))))

;;; FIXME: check if the type is already there, maybe?
(defun (setf type-function) (new-function symbol)
  (push (make-type-entry symbol new-function)
	(types *global-environment*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPEEXPAND-1.

(defun typeexpand-1 (type)
  (let* ((name (if (symbolp type) type (car type)))
	 (expander (type-function name)))
    (if (null expander)
	type
	(funcall expander
		 (if (symbolp type) (list type) type)
		 *global-environment*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPEEXPAND.

(defun typeexpand (type)
  (let ((expansion (typeexpand-1 type)))
    (if (eq expansion type)
	type
	(typeexpand expansion))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function TYPEEXPAND-ALL.

;;; FIXME: figure out what to do with the FUNCTION type specifier.
(defun typeexpand-all (type)
  (let ((expansion (typeexpand type)))
    (if (or (symbolp expansion) (null (cdr expansion)))
	(let ((name (if (symbolp expansion) expansion (car expansion))))
	  (case name
	    ((array cons integer rational
	      float short-float single-float double-float long-float)
	     `(,name * *))
	    (complex '(complex *))
	    (t
	     name)))
	(case (car expansion)
	  ((array cons integer rational
	    float short-float single-float double-float long-float)
	   (if (null (cddr expansion))
	       (append expansion '(*))
	       expansion))
	  ((and or values)
	   (cons (car expansion) (mapcar #'typeexpand-all (cdr expansion))))
	  (not
	   (cons 'not (typeexpand-all (cadr expansion))))
	  (t
	   expansion)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Querying the environment.

(defun find-in-namespace (name environment namespace)
  (find-if (lambda (entry)
	     (and (typep entry namespace)
		  (equal (name entry) name)))
	   environment))

(defun find-variable (name environment)
  (find-in-namespace name
		     (append environment
			     ;; The order here doesn't matter because,
			     ;; there can only be one entry for a
			     ;; particular name in the global
			     ;; environment.
			     (constant-variables *global-environment*)
			     (symbol-macros *global-environment*)
			     (special-variables *global-environment*))
		     'variable-space))

(defun find-function (name environment)
  (find-in-namespace name
		     (append environment
			     ;; We want to search global macros first,
			     ;; because if such an entry exists, it
			     ;; takes precedence over other entries.
			     (macros *global-environment*)
			     (functions *global-environment*)
			     (special-operators *global-environment*))
		     'function-space))

(defun find-type (entry env)
  `(and ,@(loop for e in (append env (proclamations *global-environment*))
		when (and (typep e 'type-declaration-entry)
			  (eq (base-entry e) entry))
		  collect (type e))))

(defun find-inline-info (entry env)
  (loop for e in (append env (proclamations *global-environment*))
	do (when (and (typep e 'inline-or-notinline-declaration-entry)
		      (eq (location e) entry))
	     (return (if (typep e 'inline-declaration-entry)
			 :inline
			 :notinline)))))

(defun find-ignore-info (entry env)
  (cond ((loop for e in (append env (proclamations *global-environment*))
	       when (and (typep e 'ignore-declaration-entry)
			 (eq (location e) entry))
		 return t)
	 :ignore)
	((loop for e in (append env (proclamations *global-environment*))
	       when (and (typep e 'ignorable-declaration-entry)
			 (eq (location e) entry))
		 return t)
	 :ignorable)
	(t nil)))

(defun find-dynamic-extent-info (entry env)
  (loop for e in (append env (proclamations *global-environment*))
	when (and (typep e 'dynamic-extent-declaration-entry)
		  (eq (location e) entry))
	  return t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function VARIABLE-INFO.
;;;
;;; This function is called by the compiler whenever there is a symbol
;;; in a normal value position (as opposed to in a function position).
;;; It is similar to the function with the same name, defined in
;;; CLtL2, except that we return a class instance containing all the
;;; information, rather than multiple values.

(defun variable-info (name env &optional create-if-does-not-exist)
  (let ((entry (find-variable name env)))
    (cond ((null entry)
	   (if create-if-does-not-exist
	       (progn (warn "Undefined variable: ~a" name)
		      (setf entry (make-special-variable-entry name))
		      (push entry (special-variables *global-environment*))
		      (make-instance 'special-variable-entry
			:name name
			:location (location entry)
			:type t
			:ignore-info nil
			:dynamic-extent-p nil))
	       nil))
	  ((typep entry 'constant-variable-entry)
	   (make-instance 'constant-variable-info
			  :name (name entry)
			  :definition (definition entry)))
	  ((typep entry 'symbol-macro-entry)
	   (make-instance 'symbol-macro-info
			  :name (name entry)
			  :definition (definition entry)
			  :type (find-type entry env)))
	  (t
	   (let ((type (find-type entry env))
		 (ignore-info (find-ignore-info entry env))
		 (dynamic-extent-p (find-dynamic-extent-info entry env)))
	     (make-instance (if (typep entry 'special-variable-entry)
				'special-location-info
				'lexical-location-info)
			    :location (location entry)
			    :type type
			    :ignore-info ignore-info
			    :dynamic-extent-p dynamic-extent-p))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FUNCTION-INFO.
;;;
;;; This function is called by the compiler whenever there is a symbol
;;; in a function position of a compound expression.  It is similar to
;;; the function with the same name, defined in CLtL2, except that we
;;; return a class instance containing all the information, rather
;;; than multiple values.  

(defun function-info (name env &optional create-if-does-not-exist)
  (let ((entry (find-function name env)))
    (cond ((null entry)
	   (if create-if-does-not-exist
	       (progn (warn "Undefined function: ~a" name)
		      (setf entry (make-global-function-entry name))
		      (push entry (functions *global-environment*))
		      (make-instance 'global-function-location-info
			:location (location entry)
			:type t
			:inline-info nil
			:ignore-info nil
			:dynamic-extent-p nil))
	       nil))
	  ((typep entry 'macro-entry)
	   (make-instance 'macro-info
			  :name (name entry)
			  :definition (definition entry)))
	  (t
	   (let ((type (find-type entry env))
		 (inline-info (find-inline-info entry env))
		 (ignore-info (find-ignore-info entry env))
		 (dynamic-extent-p (find-dynamic-extent-info entry env)))
	     (make-instance (if (typep entry 'global-function-entry)
				'global-function-location-info
				'lexical-function-location-info)
			    :location (location entry)
			    :type type
			    :inline-info inline-info
			    :ast (ast entry)
			    :lambda-list (lambda-list entry)
			    :ignore-info ignore-info
			    :dynamic-extent-p dynamic-extent-p))))))

(defun block-info (name env)
  (let ((entry (find-in-namespace name env 'block-space)))
    (if (null entry)
	nil
	(make-instance 'block-info
		       :name (name entry)
		       :definition (definition entry)))))

(defun tag-info (name env)
  (let ((entry (find-in-namespace name env 'tag-space)))
    (if (null entry)
	nil
	(make-instance 'tag-info
		       :name (name entry)
		       :definition (definition entry)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Create an environment entry from a canonicalized declaration
;;; specifier.

(defun make-entry-from-declaration
    (canonicalized-declaration-specifier environment)
  (destructuring-bind (head . rest) canonicalized-declaration-specifier
    (case head
      (declaration
       (make-declaration-declaration-entry (car rest)))
      (dynamic-extent
       (let ((entry (if (consp (car rest))
			(find-function (cadr (car rest)) environment)
			(find-variable (car rest) environment))))
	 (make-dynamic-extent-declaration-entry entry)))
      (ftype
       (let ((entry (find-function (cadr rest) environment)))
	 (make-type-declaration-entry entry (car rest))))
      (ignorable
       (let ((entry (if (consp (car rest))
			  (find-function (cadr (car rest)) environment)
			  (find-variable (car rest) environment))))
	 (make-ignorable-declaration-entry entry)))
      (ignore
       (let ((entry (if (consp (car rest))
			(find-function (cadr (car rest)) environment)
			(find-variable (car rest) environment))))
	 (make-ignore-declaration-entry entry)))
      (inline
       (let ((entry (find-function (car rest) environment)))
	 (make-inline-declaration-entry entry)))
      (notinline
       (let ((entry (find-function (car rest) environment)))
	 (make-notinline-declaration-entry entry)))
      (optimize
       (make-optimize-declaration-entry
	(car (car rest)) (cadr (car rest))))
      (special
       ;; FIXME: is this right?
       (make-special-variable-entry (car rest)))
      (type
       (let ((entry (find-variable (cadr rest) environment)))
	 (make-type-declaration-entry entry (car rest)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FBOUNDP.
;;;
;;; According to the HyperSpec, this function should return any true
;;; value of the name is fbound in the global environment.  From the
;;; glossary, we learn that "fbound" means that the name has a
;;; definition as either a function, a macro, or a special operator in
;;; the global environment.
;;;
;;; We could return something more useful than T, but since conforming
;;; code can not count on anything else, we might as well just return
;;; T.

(defun function-name-p (object)
  (or (symbolp object)
      (and (consp object)
	   (eq (car object) 'setf)
	   (consp (cdr object))
	   (symbolp (cadr object))
	   (null (cddr object)))))

(defun fboundp (function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  (let ((entry (find function-name
		     (append (macros *global-environment*)
			     (functions *global-environment*)
			     (special-operators *global-environment*))
		     :key #'name
		     :test #'equal)))
    (typecase entry
      (global-function-entry
       (not (eq (car (storage (location entry))) +unbound+)))
      (global-macro-entry
       t)
      (special-operator-entry
       t)
      (t
       nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FMAKUNBOUND.
;;;
;;; The description of this function in the HyperSpec say: "Removes
;;; the function or macro definition, if any, of name in the global
;;; environment.", and it adds that the consequences are undefined it
;;; the name is a special operator.
;;;
;;; For a special operator we do nothing.
;;;
;;; Recall that we may simultaneously have a global function entry and
;;; a global macro for the name.  Furthermore, one of those entries
;;; may have a compiler macro auxiliar entry referring to it. 
;;;
;;; Either way, we remove the compiler macro entry.
;;;
;;; If there is a base entry and that entry is a macro entry, we
;;; remove it too.
;;;
;;; If there is a base entry and that entry is a global function
;;; entry, we just mark it as +unbound+, but we remove any auxiliary
;;; entry that refers to it in PROCLAMATIONS.  This means that if
;;; anyone uses FMAKUNBOUND with the intention of later giving the
;;; function a new definition, then they must again proclaim its type,
;;; inline, etc.

(defun fmakunbound (function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  ;; Remove any compiler macro entry that refers to a base entry with 
  ;; this name.
  (setf (compiler-macros *global-environment*)
	(remove-if (lambda (entry)
		     (equal (name (base-entry entry)) function-name))
		   (compiler-macros *global-environment*)))
  ;; See if there is a global macro entry with the right name.
  (let ((macro-entry (find function-name (macros *global-environment*)
			   :key #'name :test #'equal)))
    (unless (null macro-entry)
      ;; We found such an entry.  Remove it. 
      (setf (macros *global-environment*)
	    (delete macro-entry (macros *global-environment*) :test #'eq))))
  ;; Next, see if there is a global function entry.
  (let ((function-entry (find function-name (functions *global-environment*)
			      :key #'name :test #'equal)))
    (unless (null function-entry)
      ;; We found such an entry.  Make sure it is unbound.
      (setf (car (storage (location function-entry))) +unbound+)
      ;; Remove any proclamations that refer to this entry
      (setf (proclamations *global-environment*)
	    (remove-if (lambda (entry)
			 (and (typep entry 'auxiliary-entry)
			      (eq (base-entry entry) function-entry)))
		       (proclamations *global-environment*)))))
  ;; Return the function name, as required by the HyperSpec.
  function-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FDEFINITION.
;;;
;;; The HyperSpec has some important things to say about this
;;; function.
;;;
;;; For one thing, it says that "An error of type UNDEFINED-FUNCTION
;;; is signaled [...] if FUNCTION-NAME is not fbound".
;;;
;;; Furthermore, it says that the return value "... may be a function
;;; or may be an object representing a special form or macro.  The
;;; value returned by fdefinition when fboundp returns true but the
;;; function-name denotes a macro or special form is not well-defined,
;;; but fdefinition does not signal an error."  In other words, we
;;; must decide what to return in the case of a macro or a special
;;; operator.  We decide that for a macro, it returns its expander
;;; function, and for a special operator, it returns the name. 
;;;
;;; Recall that we may simultaneously have a global function entry and
;;; a global macro for the name.  If that is the case, then the global
;;; macro entry is the one that is valid.  

(defun fdefinition (function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  ;; First see if there is a global macro entry with the right name.
  (let ((macro-entry
	  ;; We can use EQ to test the name because names
	  ;; of macros may only be symbols. 
	  (find function-name (macros *global-environment*)
		:key #'name :test #'eq)))
    (if (not (null macro-entry))
	;; We found a global macro entry with the right name.
	;; Return the expansion function associated with it.
	(definition macro-entry)
	;; If we did not find a global macro entry, see if there might
	;; be a global function entry with the right name.
	(let ((function-entry
		(find function-name (functions *global-environment*)
		      :key #'name :test #'equal)))
	  (if (not (null function-entry))
	      ;; We found a global function entry with the right name.
	      ;; In this case, there can not also be a special
	      ;; operator entry for the same name.
	      (let ((value (car (storage (location function-entry)))))
		(if (eq value  +unbound+)
		    (error 'undefined-function :name function-name)
		    value))
	      ;; If we did not find a global function entry, see if
	      ;; there might be a special operator entry.
	      (let ((specop-entry
		      (find function-name
			    (special-operators *global-environment*)
			    :key #'name :test #'eq)))
		(if (null specop-entry)
		    (error 'undefined-function :name function-name)
		    function-name)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF FDEFINITION).
;;;
;;; They HyperSpec says that this function can be used "to replace a
;;; global function definition when the function-name's function
;;; definition does not represent a special form.  [it] requires a
;;; function as the new value."
;;;
;;; We take this to mean: If we find an existing
;;; special-operator-entry for the name, then we signal an error.  If
;;; we find an existing global-macro-entry for the name, we replace it
;;; with a global-function entry.  If we find an existing
;;; global-function entry, we replace the definition.  If no existing
;;; entry is found, we create one.
;;;
;;; In the case of an existing global-macro-entry, we must remove it.
;;; If in addition, it has a compiler macro entry referring to it, we
;;; must remove that compiler macro entry as well.

(defun (setf fdefinition) (new-definition function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  (unless (functionp new-definition)
    (error 'type-error :datum new-definition :expected-type 'function))
  ;; First see whether there is a special operator entry for the name.
  (let ((specop-entry
	  (find function-name (special-operators *global-environment*)
		:key #'name :test #'eq)))
    (if (not (null specop-entry))
	;; We found a special operator entry.  In this situation we
	;; signal an error.
	(error "can't replace a special operator")
	(progn
	  ;; If there was no special operator entry, then check whether
	  ;; there might be a global macro entry. 
	  (let ((macro-entry
		  (find function-name (macros *global-environment*)
			:key #'name :test #'eq)))
	    (unless (null macro-entry)
	      ;; We found a global macro entry.  We must remove it.
	      (setf (macros *global-environment*)
		    (delete macro-entry
			    (macros *global-environment*)
			    :test #'eq))
	      ;; There might be a compiler macro entry that refers to
	      ;; the global macro entry we just removed, because
	      ;; compiler macro entries are auxiliary entries.  If so we
	      ;; remove that one too.
	      (setf (compiler-macros *global-environment*)
		    (delete macro-entry (compiler-macros *global-environment*)
			    :key #'base-entry :test #'eq))))
	  ;; When we come here, we know that there is no special
	  ;; operator entry with the name we are defining, and if
	  ;; there was a global macro entry for it, then that entry
	  ;; has been removed.  Next, we check whether there is an
	  ;; existing global function entry.
	  (let ((function-entry
		  (find function-name (functions *global-environment*)
			:key #'name :test #'equal)))
	    (when (null function-entry)
	      ;; No function entry found.  Create one.
	      (setf function-entry (make-global-function-entry function-name))
	      (push function-entry (functions *global-environment*)))
	    ;; Now, we have a global function entry for the name,
	    ;; whether it already existed, or we just created one.
	    ;; All we need to do is assign the new defintion to the
	    ;; storage cell of the entry.
	    (setf (car (storage function-entry)) new-definition)))))
  ;; The HyperSpec says that any SETF function must return the new
  ;; value that was assigned.
  new-definition)
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SYMBOL-FUNCTION.
;;;
;;; According to the HyperSpec, SYMBOL-FUNCTION is just like
;;; FDEFINITION, except that it only accepts a symbol as its argument.
;;; I am guessing that SYMBOL-FUNCTION existed before (SETF <mumble>)
;;; were legal function names, and that FDEFINITION was introduced to
;;; make such names possible.  In fact, on the SYMBOL-FUNCTION page,
;;; the HyperSpec says: (symbol-function symbol) == (fdefinition symbol)
;;; It suffices thus to check that the argument is a symbol, and then 
;;; to call FDEFINITION to do the work. 

(defun symbol-function (symbol)
  (unless (symbolp symbol)
    (error 'type-error :expected-type 'symbol :datum symbol))
  (fdefinition symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF SYMBOL-FUNCTION).
;;;
;;; According to the HyperSpec, (SETF SYMBOL-FUNCTION) is just like
;;; (SETF FDEFINITION), except that it only accepts a symbol as its
;;; argument.  It suffices thus to check that the argument is a
;;; symbol, and then to call (SETF FDEFINITION) to do the work.

(defun (setf symbol-function) (new-definition symbol)
  (unless (symbolp symbol)
    (error 'type-error :expected-type 'symbol :datum symbol))
  (setf (fdefinition symbol) new-definition))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SPECIAL-OPERATOR-P.
;;;
;;; We could return something more useful than T, but since conforming
;;; code can not count on anything else, we might as well just return
;;; T.

(defun special-operator-p (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (let ((entry (find symbol (special-operators *global-environment*)
		     :key #'name :test #'eq)))
    (not (null entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-FUNCTION-CELL.
;;;
;;; This function is used when a a FASL file is loaded.  When the
;;; source file was compiled, there was a reference to a named global
;;; function in it.  As part of loading the FASL file, a CODE OBJECT
;;; must be built, and that code object contains a LINKAGE VECTOR,
;;; which contains all the external references of the file that was
;;; compiled.  External references to named global functions result in
;;; an entry in the linkage vector containg the CONS cell that is the
;;; value of the STORAGE slot of the global function entry.  
;;;
;;; This function finds that CONS cell and returns it.  Normally, the
;;; FASL file should be loaded into an environment that contains the
;;; global function entry, because if it did not already exist, it was
;;; created as part of the compilation.  However, we must put
;;; SOMETHING in the linkage vector even if the file happens to be
;;; loaded into an environment that does not have the global function
;;; entry that we want.  Otherwise the system will crash when an
;;; attempt is made to execute the code we loaded.  For that reason,
;;; we create the global function entry if it so happens that it does
;;; not exist.

(defun find-function-cell (function-name)
  (let ((function-entry
	  (find function-name (functions *global-environment*)
		:key #'name :test #'equal)))
    (when (null function-entry)
      ;; No function entry found.  Create one.
      (setf function-entry (make-global-function-entry function-name))
      (push function-entry (functions *global-environment*)))
    (storage (location function-entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF MACRO-FUNCTION).
;;;
;;; The HyperSpec says that the consequences are undefined if a
;;; non-nil environment is given.  We define those consequences to be
;;; that an error is signaled.
;;;
;;; The HyperSpec further says that "Performing this operation causes
;;; symbol to have only that macro definition as its global function
;;; definition; any previous definition, whether as a macro or as a
;;; function, is lost."
;;;
;;; The HyperSpec also says that it is possible for a symbol to be
;;; defined both as a special operator and as a macro.  For that
;;; reason, we want to find out whether there is an entry that is
;;; either a global function entry or a global macro entry, but we
;;; want to ignore the possibility of there being a special operator
;;; entry.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun (setf macro-function) (new-function symbol &optional environment)
    (unless (null environment)
      (error 'type-error :datum environment :expected-type 'null))
    (unless (symbolp symbol)
      (error 'type-error :datum symbol :expected-type 'symbol))
    (unless (functionp new-function)
      (error 'type-error :datum new-function 'function))
    ;; First check whether there is a global function entry with
    ;; this name.
    (let ((function-entry
	    (find symbol (functions *global-environment*)
		  :key #'name :test #'eq)))
      (if (not (null function-entry))
	  (progn
	    ;; Make it unbound.
	    (setf (car (storage (location function-entry))) +unbound+)
	    ;; Remove any compiler macro that might be present referring
	    ;; to this entry.
	    (setf (compiler-macros *global-environment*)
		  (remove function-entry (compiler-macros *global-environment*)
			  :key #'base-entry :test #'eq))
	    ;; Remove any proclamations referring to this entry.
	    (setf (proclamations *global-environment*)
		  (remove-if (lambda (entry)
			       (and (typep entry 'auxiliary-entry)
				    (eq (base-entry entry) function-entry)))
			     (proclamations *global-environment*))))
	  ;; No function entry exists.  check whether there is a
	  ;; global macro entry with this name.
	  (let ((macro-entry
		  (find symbol (macros *global-environment*)
			:key #'name :test #'eq)))
	    (if (not (null macro-entry))
		;; Then just replace the old definition.  This
		;; way, we preserve any compiler macro that refers
		;; to this entry.
		(setf (definition macro-entry) new-function)
		;; Otherwise, we must create a new entry
		(push (make-instance 'global-macro-entry
			:name symbol
			:definition new-function)
		      (macros *global-environment*))))))
    ;; Return the new value as required by the HyperSpec.
    new-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function BOUNDP.
;;;
;;; According to the HyperSpec, this function should return any true
;;; value if the name is bound in the global environment and false
;;; otherwise.  We return T when the symbol is bound.  
;;;
;;; The HyperSpec does not say whether the name of a constant variable
;;; is considered to be bound.  We think it is reasonable to consider
;;; it bound in this case.  They HyperSpec also does not say whether
;;; the name of a global symbol macro is considered to be bound.
;;; Again, we think it is reasonable to consider this to be the case,
;;; if for nothing else, then for symmetry with fboundp.
;;;
;;; The symbol is bound as a special variable if it is both the case
;;; that a special variable entry exists for it AND the storage cell
;;; of that entry does not contain +unbound+.

(defun boundp (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (not (null (or (find symbol (constant-variables *global-environment*)
		       :key #'name :test #'eq)
		 (find symbol (symbol-macros *global-environment*)
		       :key #'name :test #'eq)
		 (find-if (lambda (entry)
			    (and (eq (name entry) symbol)
				 (not (eq (car (storage (location entry)))
					  +unbound+))))
			  (special-variables *global-environment*))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function MAKUNBOUND.
;;;
;;; Since we consider a name bound if it names a constant variable, or
;;; if it names a global symbol macro, we must decide what to do in
;;; those cases.  It would be embarassing for someone to call
;;; MAKUNBOUND successfully and then have BOUNDP return true.  What we
;;; do is to remove the symbol macro if any, and signal an error if an
;;; attempt is made to make a constant variable unbound.

(defun makunbound (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  ;; Check whether the symbol has a definition as a constant variable.
  (let ((constant-variable-entry
	  (find symbol (constant-variables *global-environment*)
		:key #'name :test #'eq)))
    (if (not (null constant-variable-entry))
	(error "Attemp to make a constant variable unbound")
	;; Check whether the symbol has a definition as a global
	;; symbol macro.
	(let ((macro-entry
		(find symbol (symbol-macros *global-environment*)
		      :key #'name :test #'eq)))
	  (if (not (null macro-entry))
	      (progn
		;; Remove the symbol macro entry.
		(setf (symbol-macros *global-environment*)
		      (delete macro-entry (symbol-macros *global-environment*)
			      :test #'eq))
		;; The symbol macro might have a type proclamation
		;; associated with it.  Remove that too.
		(setf (proclamations *global-environment*)
		      (delete-if (lambda (entry)
				   (and (typep entry 'auxiliary-entry)
					(eq (base-entry entry) macro-entry)))
				 (proclamations *global-environment*))))
	      ;; Check whether the symbol has a definition as a
	      ;; special variable.
	      (let ((variable-entry
		      (find symbol (special-variables *global-environment*)
			    :key #'name :test #'eq)))
		(unless (null variable-entry)
		  ;; Set the storage cell to +unbound+
		  (setf (car (storage (location variable-entry))) +unbound+)
		  ;; The variable might have a various proclamation
		  ;; associated with it.  Remove those too.
		  (setf (proclamations *global-environment*)
			(delete-if (lambda (entry)
				     (and (typep entry 'auxiliary-entry)
					  (eq (base-entry entry) macro-entry)))
				   (proclamations *global-environment*)))))))))
  ;; Return the symbol as required by the HyperSpec
  symbol)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function SYMBOL-VALUE.
;;;
;;; The HyperSpec specifically allows for SYMBOL-VALUE to be used on
;;; constant variables.  

(defun symbol-value (symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  ;; Handle keyword symbols specially here.
  (if (keywordp symbol)
      symbol
      ;; Next check whether the symbol has a defintion as a constant
      ;; variable. 
      (let ((constant-variable-entry
	      (find symbol (constant-variables *global-environment*)
		    :key #'name :test #'eq)))
	(if (not (null constant-variable-entry))
	    (definition constant-variable-entry)
	    ;; Check whether the symbol has a definition as a special
	    ;; variable, and check whether it is bound. 
	    (let ((special-variable-entry
		    (find symbol (special-variables *global-environment*)
			  :key #'name :test #'eq)))
	      (if (not (null special-variable-entry))
		  (let ((val (car (storage (location special-variable-entry)))))
		    (if (eq val +unbound+)
			(error 'unbound-variable :name symbol)
			val))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary function ENSURE-DEFINED-VARIABLE.
;;;
;;; This function checks whether there is an entry for a name as a
;;; special variable in the global environment, and if not creates
;;; such an entry.  If the entry exists, but is not marked as DEFINED,
;;; then this function marks it as such.
;;;
;;; If the name is already that of a constant variable, then an error
;;; is signaled.
;;;
;;; If there is an entry for the name as a global macro then an error
;;; is signaled.

(defun ensure-defined-variable (name)
  (when (constantp name)
    (error "Attempt to redefine a constant variable."))
  (unless (null (find name (symbol-macros *global-environment*)
		      :key #'name :test #'eq))
    (error "Attempt to redefine a global symbol macro as a variable."))
  (let ((entry (find name (special-variables *global-environment*)
		     :key #'name :test #'eq)))
    (if (null entry)
	(push (make-special-variable-entry name t)
	      (special-variables *global-environment*))
	(setf (defined-p entry) t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function (SETF SYMBOL-VALUE).
;;;
;;; Signal an error if an attempt is made to call this function with
;;; the name of a constant variable.
;;;
;;; The Hyperspec does not indicate what should be done if this
;;; function is called and the name already has a definition as a
;;; global symbol macro.  However, it does say that an error is
;;; signaled in the opposite situation, i.e., if an attempt is made to
;;; define a symbol macro with a name of an existing special variable.
;;; For that reason, we think it is reasonable to signal an error in
;;; this case too.

(defun (setf symbol-value) (new-value symbol)
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  ;; Handle keyword symbols specially here.
  (when (keywordp symbol)
    (error "attempt to change the value of a keyword."))
  ;; Next check whether the symbol has a defintion as a constant
  ;; variable. 
  (when (constantp symbol)
    (error "attempt to change the value of a constant variable"))
  ;; Calling this function implicitly DEFINES the variable.
  (ensure-defined-variable symbol)
  ;; Find out everything about the variable. 
  (let ((info (variable-info symbol nil)))
    (unless (typep new-value (type info))
      (error 'type-error
	     :datum new-value
	     :expected-type (type info)))
    (setf (car (storage (location info)))
	  new-value))
  ;; Return the new value as the HyperSpec requires.
  new-value)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function FIND-VALUE-CELL.
;;;
;;; This function is used when a a FASL file is loaded.  This function
;;; is used when the source file that was compiled contained a
;;; reference to a special variable or to a free undefined variable
;;; (which is taken to be a special variable that will ultimately be
;;; defined).  As part of loading the FASL file, a CODE OBJECT must be
;;; built, and that code object contains a LINKAGE VECTOR, which
;;; contains all the external references of the file that was
;;; compiled.  External references to special variables result in an
;;; entry in the linkage vector containg the CONS cell that is the
;;; value of the STORAGE slot of the special variable entry.  This
;;; value cell contains the global value of the variable, or +unbound+
;;; if the variable does not have a global value.
;;;
;;; This function finds that CONS cell and returns it.  Normally, the
;;; FASL file should be loaded into an environment that contains the
;;; special variable entry, because if it did not already exist, it
;;; was created as part of the compilation.  However, we must put
;;; SOMETHING in the linkage vector even if the file happens to be
;;; loaded into an environment that does not have the special variable
;;; entry that we want.  Otherwise the system will crash when an
;;; attempt is made to execute the code we loaded.  For that reason,
;;; we create the special variable entry if it so happens that it does
;;; not exist.

(defun find-value-cell (name)
  (let ((special-variable-entry
	  (find name (special-variables *global-environment*)
		:key #'name :test #'eq)))
    (when (null special-variable-entry)
      ;; No function entry found.  Create one.
      (setf special-variable-entry
	    (make-special-variable-entry name))
      (push special-variable-entry
	    (special-variables *global-environment*)))
    (storage (location special-variable-entry))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function CONSTANTP.

(defun constantp (form &optional environment)
  (declare (ignore environment))
  (or (and (not (symbolp form))
	   (not (consp form)))
      (keywordp form)
      (not (null (find form (constant-variables *global-environment*)
		       :key #'name :test #'eq)))
      (and (consp form)
	   (eq (car form) 'quote))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function PROCLAIM.

(defun proclaim-declaration (name)
  (unless (find-if (lambda (entry)
		     (and (typep entry 'declaration-declaration-entry)
			  (eq (name entry) name)))
		   (proclamations *global-environment*))
    (push (make-instance 'declaration-declaration-entry
	    :name name)
	  (proclamations *global-environment*))))

(defun proclaim-ftype (name type)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'global-function-entry)
			       (eq (name entry) name)))
			(functions *global-environment*))))
    (when (null entry)
      (setf entry (make-global-function-entry name))
      (push entry (functions *global-environment*)))
    (let ((existing-declaration
	    (find-if (lambda (decl)
		       (and (typep decl 'type-declaration-entry)
			    (eq (base-entry decl) entry)))
		     (proclamations *global-environment*))))
      (cond ((null existing-declaration)
	     (push (make-type-declaration-entry entry type)
		   (proclamations *global-environment*)))
	    ((equal (type existing-declaration) type)
	     nil)
	    (t
	     ;; make that an error for now
	     (error "function already has a type proclamation"))))))

(defun proclaim-special (name)
  (pushnew (make-special-variable-entry name)
	   (special-variables *global-environment*)
	   :key #'name
	   :test #'eq))

(defun proclaim (declaration-specifier)
  (case (car declaration-specifier)
    (declaration
     (mapc #'proclaim-declaration
	   (cdr declaration-specifier)))
    (ftype
     (mapc (lambda (name) (proclaim-ftype name (cadr declaration-specifier)))
	   (cddr declaration-specifier)))
    (special
     (mapc #'proclaim-special
	   (cdr declaration-specifier)))
    ;; FIXME: handle more proclamations
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The expansion of the macro IN-PACKAGE results in a call to this
;;; function.

(defun in-package-function (string-designator)
  (declare ((or character symbol string) string-designator))
  (setq *package* (find-package string-designator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The expansion of the macro DEFCONSTANT results in a call to this
;;; function.

(defun defconstant-function (name initial-value)
  (unless (null (find name (special-variables *global-environment*)
		      :key #'name :test #'eq))
    (error "attempt to redefine a special variable as a constant variable."))
  (unless (null (find name (symbol-macros *global-environment*)
		      :key #'name :test #'eq))
    (error "attempt to redefine a global symbol macro as a constant variable."))
  (let ((existing-entry (find name (constant-variables *global-environment*)
			      :key #'name :test #'eq)))
    (cond ((null existing-entry)
	   (push (make-constant-variable-entry name initial-value)
		 (constant-variables *global-environment*)))
	  ((not (eql initial-value (definition existing-entry)))
	   (error "attempt to redefine a constant variable"))
	  (t
	   nil)))
  ;; Return the name as the HyperSpec requires
  name)


