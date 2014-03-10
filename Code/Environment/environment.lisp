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
  ((%storage :initarg :storage :initform (list +funbound+) :reader storage)))

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
;;; +funbound+.
;;;
;;; A global function entry can come into existence in several ways:
;;; 
;;;  * Using (SETF FDEFINITION) on a name that is not the name of a
;;;    special operator.  A LOCATION for the entry is created, and the
;;;    storage cell will be set to the new definition.
;;;
;;;  * Proclaiming FTYPE, INLINE, NOTINLINE or DYNAMIC-EXTENT with
;;;    FUNCTION using the name.  Again, A LOCATION for the entry is
;;;    created, but the storage cell will be set to +funbound+.  The
;;;    appropriate auxiliary entry is created and will refer to the
;;;    base entry.
;;;
;;;  * When the compiler sees a compound form with the CAR containing
;;;    a symbol that is not associated with an entry in the FUNCTION
;;;    namespace.  In this case, a LOCATION for the entry is created
;;;    and the storage cell of the entry is initialized to +funbound+.
;;;    A warning is also signaled, indicating that the function is
;;;    undefined.
;;;
;;; A global function entry is never removed for reasons mentioned
;;; above.

(defclass global-function-entry (function-entry)
  ())

(defun make-global-function-entry (name &optional
					  (lambda-list :none)
					  ast
					  parameters)
  (declare (cl:type function-name name))
  (make-instance 'global-function-entry
		 :name name
		 :lambda-list lambda-list
		 :ast ast
		 :parameters parameters
		 :location (make-global-location name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ENSURE-GLOBAL-FUNCTION-ENTRY.
;;;
;;; Given a name and an optional lambda expression, make sure there is
;;; a global function entry in the global environment with that name.
;;; If there is no such entry then create one and return it.  If there
;;; already is an entry with that name, then if a lambda list is
;;; given, then make that lambda list the new lambda list of the
;;; existing entry.  If the function to be defined has an INLINE
;;; proclamation in effect, then create the abstract syntax tree for
;;; the function and store it in the function entry.  Return either
;;; the newly created entry or the existing entry.

(defun ensure-global-function-entry
    (name lambda-list ast parameters)
  (declare (cl:type function-name name))
  (let ((entry (find name (functions *global-environment*)
		     :test #'equal :key #'name)))
    (if (null entry)
	(let ((new-entry (make-global-function-entry
			  name lambda-list ast parameters)))
	  (push new-entry (functions *global-environment*))
	  new-entry)
	(progn (setf (lambda-list entry) lambda-list)
	       (setf (ast entry) ast)
	       (setf (parameters entry) parameters)
	       entry))))

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
;;; has a location where the storage cell is not +funbound+, then that
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

(defun make-inline-declaration-entry (base-entry)
  (make-instance 'inline-declaration-entry
		 :base-entry base-entry))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class NOTINLINE-DECLARATION-ENTRY.

(defclass notinline-declaration-entry (inline-or-notinline-declaration-entry)
  ())

(defun make-notinline-declaration-entry (base-entry)
  (make-instance 'notinline-declaration-entry
		 :base-entry base-entry))

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
  ((%lambda-list :initarg :lambda-list :reader lambda-list)
   ;; This slot contains either NIL (meaning that no inline
   ;; information has been established for this function), INLINE
   ;; (which means that the function has been declared INLINE in this
   ;; context), or NOTINLINE (which means that the function has been
   ;; declared NOTINLINE in this context).
   (%inline-info :initarg :inline-info :reader inline-info)
   ;; When this slot contains NIL, there is no available
   ;; AST, so the function can not be inlined.  
   (%ast :initarg :ast :reader ast)
   ;; When the AST slot contains an AST (and not NIL), then this slot
   ;; contains a list of lexical locations corresponding to the
   ;; parameters of the function.
   (%parameters :initarg :parameters :reader parameters)))

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
		      (eq (base-entry e) entry))
	     (return (if (typep e 'inline-declaration-entry)
			 :inline
			 :notinline)))))

(defun find-ignore-info (entry env)
  (cond ((loop for e in (append env (proclamations *global-environment*))
	       when (and (typep e 'ignore-declaration-entry)
			 (eq (base-entry e) entry))
		 return t)
	 :ignore)
	((loop for e in (append env (proclamations *global-environment*))
	       when (and (typep e 'ignorable-declaration-entry)
			 (eq (base-entry e) entry))
		 return t)
	 :ignorable)
	(t nil)))

(defun find-dynamic-extent-info (entry env)
  (loop for e in (append env (proclamations *global-environment*))
	when (and (typep e 'dynamic-extent-declaration-entry)
		  (eq (base-entry e) entry))
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
		      (make-instance 'special-location-info
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
			:ast nil
			:parameters nil
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
			    :lambda-list (lambda-list entry)
			    :ast (ast entry)
			    :parameters (parameters entry)
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


