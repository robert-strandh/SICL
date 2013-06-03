(in-package #:sicl-compiler-environment)

;;;; An environment contains ENTRIES of various kinds.  An entry can
;;;; be a BASE ENTRY.  This is the kind of entry used for variables,
;;;; functions, and macros, but also for autonomous declarations such
;;;; as OPTIMIZE.  Other entries are AUXILIARY entries, in that they
;;;; provide additioal information about other entries.  This type of
;;;; entry is used for declarations of type, inline, and dynamic
;;;; extent.  The class of a BASE ENTRY determines for which namespace
;;;; it is relevant.  
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

(defvar +unbound+ (list nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Global environment.
;;;
;;; The FUNCTION namespace contains three types of base entries:
;;; GLOBAL-FUNCTION-ENTRY, GLOBAL-MACRO-ENTRY, and
;;; SPECIAL-OPERATOR-ENTRY.  All special operator entries are global,
;;; because there is no mechanism for defining local special
;;; operators.  The FUNCTION name space also contains auxiliary
;;; entries of type COMPILER-MACRO-ENTRY. 

(defclass global-environment ()
  (;; The package namespace.  A list of packages.
   (%packages :initform '() :accessor packages)
   ;; The class namespace.  A list of classes.
   (%classes :initform '() :accessor classes)
   ;; The type namespace.
   (%types :initform '() :accessor types)
   ;; The variable namespace.  It contains entries for
   ;; symbol macros, constant variables, and special variables. 
   (%variables :initform '() :accessor variables)
   ;; The function namespace.  It contains entries for macros,
   ;; functions, compiler macros, and special operators.
   (%functions :initform '() :accessor functions)
   (%proclamations :initform '() :accessor proclamations)))

(defparameter *global-environment*
  (make-instance 'global-environment))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Locations. 
;;;
;;; Locations are the values of slots of entries representing places
;;; that need to be accessed at runtime.  This is the case for
;;; variables and functions.  A location can be a global location, a
;;; lexical location, or a special location.

(defclass location ()
  ((%name :initarg :name :reader name)))

;;; A global location is a location that has global storage associated
;;; with it, and that storage is allocated directly in the global
;;; environment.  This is the case for globally defined functions.
(defclass global-location (location)
  ((%storage :initform (list +unbound+) :reader storage)))

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
(defclass declaration-space () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The nature of the entry.

;;; For entries that have a complete definition in the environment.
;;; This is the case for macros, symbol macros, constant variables,
;;; blocks, and go tags.
(defclass definition-entry ()
  ((%definition :initarg :definition :reader definition)))

;;; This type of entry has some kind of location associated with it.
;;; This is the case for special variables, lexical variables, and
;;; functions.
(defclass location-entry ()
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-VARIABLE-ENTRY.
;;;
;;; A constant variable entry belongs to the variable namespace.  It
;;; does not require any storage to be accessed at runtime becuase its
;;; value is propagated at compile time.

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
;;; variable with that name exists, and if so, creates a special
;;; binding.  An entry in stage 1 does not trigger that behavior in
;;; the compiler, so any new binding of the varible is considered to
;;; be a lexical binding.
;;;
;;; Stage 2 happens when a DEFVAR or DEFPARAMETER form has been
;;; evaluated that refers to the variable.  The main difference with
;;; stage 1, is that in stage 2, when the compiler processes a binding
;;; of a variable with this name, then the binding is always a special
;;; binding.

(defclass special-variable-entry
    (base-entry named-entry variable-space location-entry)
  ((%defined-p :initform nil :initarg :defined-p :accessor defined-p)))

(defun make-special-variable-entry (name)
  (make-instance 'special-variable-entry
		 :name name
		 :location (make-special-location name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-VARIABLE-ENTRY.
;;;

(defclass lexical-variable-entry
    (base-entry named-entry varible-space location-entry)
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
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-FUNCTION-ENTRY.
;;;
;;; Global function entries are base entries.  They occur in the
;;; FUNCTION namespace of a global environment.  A global function
;;; entry represents a globally defined ordinary function, as opposed
;;; to a macro or a special operator.
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
;;; The reason for not removing it is that other functions may exist that
;;; use the name N as a function, so the entry can not be removed.
;;; For this reason, it is possible that there simultaneously exist a
;;; global macro entry and a global function entry for the same name
;;; N.  However, in that case, the storage cell of the location of the
;;; global function entry always contains +unbound+.
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
;;; Global macro entries are base entries.  They occur in the FUNCTION
;;; namespace of a global environment.  A global macro entry
;;; represents a globally defined macro, as opposed to an ordinary
;;; function or a special operator.
;;;
;;; There can simultaneously be a global macro entry and either a
;;; special operator entry or a global function entry (but not both)
;;; with the same name.  The HyperSpec specifically allows for a macro
;;; and a special operator for the same name to exist.  A global
;;; function entry can exist at the same time as a global macro entry
;;; as a result of the global macro entry being created using (SETF
;;; MACRO-FUNCTION) but the global function entry can not be removed,
;;; because its LOCATION is not NIL.  
;;;
;;; A global macro entry can not have any auxiliary entries associated
;;; with it.
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
;;; Special operator entries are base entries.  They occur in the
;;; FUNCTION namespace of a global environment.  A special operator
;;; entry represents a special operator, as opposed to an ordinary
;;; function or a macro.
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
;;; the FUNCTION namespace of a global environment.  A compiler macro
;;; entry refers either to a global function entry or to a global
;;; macro entry.  Compiler macros are by definition global.  The
;;; HyperSpec makes no provision for creating local compiler macros.
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
		 :location (location location-entry)
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
   (%inline-info :initarg :inline-info :reader inline-info)
   (%ignore-info :initarg :ignore-info :reader ignore-info)
   (%dynamic-extent-p :initarg :dynamic-extent-p :reader dynamic-extent-p)))

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
;;; Class GLOBAL-LOCATION-INFO.

(defclass global-location-info (location-info)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro expansion.

(defun macro-function (symbol &optional env)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'macro-entry)
			       (eq (name entry) symbol)))
			(append env (functions *global-environment*)))))
    (if (null entry)
	nil
	(definition entry))))

(defparameter *macroexpand-hook*
  (lambda (macro-function macro-form environment)
    (funcall macro-function macro-form environment)))

(defun macroexpand-1 (form &optional env)
  (let ((expander nil))
    (cond ((and (consp form) (symbolp (car form)))
	   (setf expander (macro-function (car form) env)))
	  ((symbolp form)
	   (let ((entry (find-if (lambda (entry)
				   (and (typep entry 'symbol-macro-entry)
					(eq (name entry) form)))
				 (append env (variables *global-environment*)))))
	     (if (null entry)
		 nil
		 (setf expander (definition entry)))))
	  (t nil))
    (if expander
	(values (funcall (coerce *macroexpand-hook* 'function)
			 expander
			 form
			 env)
		t)
	(values form nil))))

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

;;; If there is a compiler macro entry, it must refer to a base entry
;;; which is either a global macro entry or to a global function entry
;;; that is bound.  This function searches for such a base entry.
(defun find-base-entry (name environment)
  (or (find-if (lambda (entry)
		 (and (eq (name entry) name)
		      (typep entry 'global-macro-entry)))
	       (functions environment))
      (find-if (lambda (entry)
		 (and (eq (name entry) name)
		      (typep entry 'global-function-entry)
		      (not (null (location entry)))
		      (not (eq (car (storage (location entry))) +unbound+))))
	       (functions environment))))

;;; I am not sure what the optional environment argument could be.  It
;;; seems to me that it must either be a global environment, i.e. an
;;; instance of GLOBAL-ENVIRONMENT, a local environment, or NIL.  In
;;; the last two cases, unless I am wrong, the value of
;;; *global-envrionment* must be used. 
(defun compiler-macro-function (name &optional environment)
  (unless (typep environment 'global-environment)
    (setf environment *global-environment*))
  (let* ((base-entry (find-base-entry name environment))
	 (c-m-entry (find-if (lambda (entry)
			       (and (eq (location entry) base-entry)
				    (typep entry 'compiler-macro-entry)))
			     (functions environment))))
    (if (null c-m-entry)
	nil
	(definition c-m-entry))))

(defun (setf compiler-macro-function) (new-function name &optional environment)
  (unless (null environment)
    (error "Environment object must be nil."))
  (let ((base-entry (find-base-entry name environment)))
    (when (null base-entry)
      (error "A global macro or a global function must already exist."))
    (let ((c-m-entry (find-if (lambda (entry)
				(and (eq (location entry) base-entry)
				     (typep entry 'compiler-macro-entry)))
			      (functions *global-environment*))))
      ;; Remove the old entry if there was one.
      (unless (null c-m-entry)
	(setf (functions *global-environment*)
	      (delete c-m-entry (functions *global-environment*)
		      :test #'eq)))
      ;; Add a new entry unless the new function is NIL.
      (unless (null new-function)
	(push (make-compiler-macro-entry name new-function)
	      (functions *global-environment*)))))
  new-function)

(defun compiler-macroexpand-1 (form &optional env)
  (if (symbolp (car form))
      (let* ((base-entry (find-function (car form) env))
	     (c-m-entry (find-if (lambda (entry)
				   (and (eq (location entry) base-entry)
					(typep entry 'compiler-macro-entry)))
				 (functions *global-environment*))))
	(if (null c-m-entry)
	    form
	    (funcall (coerce *macroexpand-hook* 'function)
		     (definition c-m-entry)
		     form
		     env)))
      form))

(defun compiler-macroexpand (form &optional env)
  (loop for expanded-form = (compiler-macroexpand-1 form env)
	until (eq expanded-form form)
	do (setf form expanded-form))
  form)

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
		     (append environment (variables *global-environment*))
		     'variable-space))

(defun find-function (name environment)
  (find-in-namespace name
		     (append environment (functions *global-environment*))
		     'function-space))

(defun find-type (entry env)
  `(and ,@(loop for e in (append env (proclamations *global-environment*))
		when (and (typep e 'type-declaration-entry)
			  (eq (location e) entry))
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

(defun variable-info (name env)
  (let ((entry (find-variable name env)))
    (cond ((null entry)
	   nil)
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
		 (inline-info (find-inline-info entry env))
		 (ignore-info (find-ignore-info entry env))
		 (dynamic-extent-p (find-dynamic-extent-info entry env)))
	     (make-instance (if (typep entry 'special-variable-entry)
				'special-location-info
				'lexical-location-info)
			    :location (location entry)
			    :type type
			    :inline-info inline-info
			    :ignore-info ignore-info
			    :dynamic-extent-p dynamic-extent-p))))))

(defun function-info (name env)
  (let ((entry (find-function name env)))
    (cond ((null entry)
	   (warn "Undefined function: ~a" name)
	   (setf entry (make-global-function-entry name))
	   (push entry (functions *global-environment*))
	   (make-instance 'global-location-info
	     :location (location entry)
	     :type nil
	     :inline-info nil
	     :ignore-info nil
	     :dynamic-extent-p nil))
	  ((typep entry 'local-macro-entry)
	   (make-instance 'macro-info
			  :name (name entry)
			  :definition (definition entry)))
	  (t
	   (let ((type (find-type entry env))
		 (inline-info (find-inline-info entry env))
		 (ignore-info (find-ignore-info entry env))
		 (dynamic-extent-p (find-dynamic-extent-info entry env)))
	     (make-instance (if (typep entry 'global-function-entry)
				'global-location-info
				'lexical-location-info)
			    :location (location entry)
			    :type type
			    :inline-info inline-info
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
		     (functions *global-environment*)
		     :key #'name
		     :test #'equal)))
    (typecase entry
      (global-function-entry
       (and (not (null (location entry)))
	    (not (eq (car (storage (location entry))) +unbound+))))
      (global-macro-entry
       (definition entry))
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
;;; We could remove the entry altogether, but we have chosen not to do
;;; that.  The reason is that we may have other entries that refer to
;;; it, such as entries that have to do with whether a function should
;;; be inlined or not.  We want to preserve the invariant that such
;;; entries should always refer to the "base" entry.  To preserve that
;;; property, we mark the base entry as not being bound, and we check
;;; this in all relevant functions.  By doing it this way, we can also
;;; give a meaning to a proclamation of an ftype for an unbound
;;; function.  We simply create a base entry and mark it as unbound. 

(defun fmakunbound (function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  ;; First see if there is a global macro entry with teh right name.
  (let ((macro-entry (find-if (lambda (entry)
				(and (typep entry 'global-macro-entry)
				     (equal (name entry) function-name)))
			      (functions *global-environment*))))
    (unless (null macro-entry)
      ;; We found such an entry.  Remove it. 
      (setf (functions *global-environment*)
	    (delete macro-entry (functions *global-environment*) :test #'eq))
      ;; If there is a compiler-macro entry referring to that entry,
      ;; then remove the compiler-macro entry as well. 
      (setf (functions *global-environment*)
	    (delete-if (lambda (entry)
			 (and (typep entry 'compiler-macro-entry)
			      (eq (base-entry entry) macro-entry)))
		       (functions *global-environment*)))))
  ;; Next, see if there is a global function entry.
  (let ((function-entry (find-if (lambda (entry)
				   (and (typep entry 'global-function-entry)
					(equal (name entry) function-name)))
				 (functions *global-environment*))))
    (unless (null function-entry)
      ;; We found such an entry.  Make sure it is unbound.
      (unless (null (location function-entry))
	(setf (car (storage (location function-entry))) +unbound+))))
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
	  (find-if (lambda (entry)
		     (and (typep entry 'global-macro-entry)
			  (equal (name entry) function-name)))
		   (functions *global-environment*))))
    (if (not (null macro-entry))
	;; We found a global macro entry with the right name.
	;; Return the expansion function associated with it.
	(definition macro-entry)
	;; If we did not find a global macro entry, see if there might
	;; be a global function entry with the right name.
	(let ((function-entry
		(find-if (lambda (entry)
			   (and (typep entry 'global-function-entry)
				(equal (name entry) function-name)))
			 (functions *global-environment*))))
	  (if (not (null function-entry))
	      ;; We found a global function entry with the right name.
	      ;; In this case, there can not also be a special
	      ;; operator entry for the same name.
	      (if (or (null (location function-entry))
		      (eq (car (storage (location function-entry))) +unbound+))
		  (error 'undefined-function :name function-name)
		  (car (storage (location function-entry))))
	      ;; If we did not find a global function entry, see if
	      ;; there might be a special operator entry.
	      (let ((specop-entry
		      (find-if (lambda (entry)
				 (and (typep entry 'special-operator-entry)
				      (equal (name entry) function-name)))
			       (functions *global-environment*))))
		(if (null specop-entry)
		    (error 'undefined-function :name function-name)
		    t)))))))

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
  (let ((entry (find function-name
		     (functions *global-environment*)
		     :key #'name
		     :test #'equal)))
    (cond ((null entry)
	   (setf entry (make-instance 'global-function-entry
			 :name function-name
			 :bound t))
	   (push entry (functions *global-environment*)))
	  ((typep entry 'special-operator-entry)
	   (error "can't replace a special operator"))
	  ((typep entry 'global-macro-entry)
	   (setf (functions *global-environment*)
		 (remove entry (functions *global-environment*)))
	   (setf entry (make-instance 'global-function-entry
			 :name function-name
			 :bound t))
	   (push entry (functions *global-environment*))))
    (setf (car (storage entry)) new-definition))
  new-definition)
  
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
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'special-operator-entry)
			       (eq (name entry) symbol)))
			(functions *global-environment*))))
    (not (null entry))))

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

(defun (setf macro-function) (new-function symbol &optional environment)
  (unless (null environment)
    (error 'type-error :datum environment :expected-type 'null))
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (unless (functionp new-function)
    (error 'type-error :datum new-function 'function))
  (setf (functions *global-environment*)
	(remove-if (lambda (entry)
		     (and (or (typep entry 'global-function-entry)
			      (typep entry 'global-macro-entry))
			  (eq (name entry) symbol)))
		   (functions *global-environment*)))
  (push (make-instance 'global-macro-entry
	  :name symbol
	  :definition new-function
	  :bound t)
	(functions *global-environment*)))

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
			    (eq (location decl) (location entry))))
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
	   (variables *global-environment*)
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

