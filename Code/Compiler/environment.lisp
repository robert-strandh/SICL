(in-package #:sicl-compiler-environment)

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

(defclass dummy-entry (entry)
  ())

(defclass named-entry ()
  ((%name :initarg :name :reader name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Namespaces.

(defclass function-space () ())
(defclass variable-space () ())
(defclass block-space () ())
(defclass tag-space () ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The nature of the entry.

;;; For entries that have a complete definition in 
;;; the environment.
(defclass definition-entry (entry named-entry)
  ((%definition :initarg :definition :reader definition)))

;;; For entries representing places that need
;;; to be accessed at runtime. 
(defclass location ()
  ((%name :initarg :name :reader name)))

;;; Every use of the environment must get the same
;;; place, so the storage is allocated here. 
(defclass global-location (location)
  ((%storage :initform (list nil) :reader storage)))

(defun make-global-location (name)
  (make-instance 'global-location :name name))

;;; For special locations, the name is the symbol which must be used
;;; at runtime to access the value.
(defclass special-location (location)
  ())

(defun make-special-location (name)
  (make-instance 'special-location :name name))

;;; The name of a lexical location is just used to display it.
(defclass lexical-location (location)
  ())

(defun make-lexical-location (name)
  (make-instance 'lexical-location :name name))

(defclass location-entry (entry named-entry)
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Constant variable entry.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class CONSTANT-VARIABLE-ENTRY.
;;;
;;; A constant variable entry belongs to the variable namespace.  It
;;; does not require any storage to be accessed at runtime becuase its
;;; value is propagated at compile time.

(defclass constant-variable-entry (variable-space definition-entry)
  ())

(defun make-constant-variable-entry (name definition)
  (make-instance 'constant-variable-entry
		 :name name
		 :definition definition))

(defun add-constant-variable-entry (env name definition)
  (add-to-environment env (make-constant-variable-entry name definition)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Location entries.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-VARIABLE-ENTRY.
;;;

(defclass special-variable-entry (variable-space location-entry)
  ())
  
(defun make-special-variable-entry (name)
  (make-instance 'special-variable-entry
		 :name name
		 :location (make-special-location name)))

(defun add-special-variable-entry (env name)
  (add-to-environment env (make-special-variable-entry name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-VARIABLE-ENTRY.
;;;

(defclass lexical-variable-entry (variable-space location-entry)
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

(defclass symbol-macro-entry (variable-space definition-entry)
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

(defclass function-entry (function-space location-entry)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-FUNCTION-ENTRY.

(defclass global-function-entry (function-entry)
  ((%bound :initform nil :initarg :bound :accessor bound)))

(defun make-global-function-entry (name)
  (make-instance 'global-function-entry
		 :name name
		 :location (make-global-location name)))

(defun add-global-function-entry (env name)
  (add-to-environment env (make-global-function-entry name)))

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

(defclass macro-entry (function-space definition-entry)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-MACRO-ENTRY.

(defclass global-macro-entry (macro-entry)
  ((%bound :initform nil :initarg :bound :accessor bound)))

(defun make-global-macro-entry (name expander)
  (make-instance 'global-macro-entry
		 :name name
		 :definition expander))

(defun add-global-macro-entry (env name expander)
  (add-to-environment env (make-global-macro-entry name expander)))

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

(defclass special-operator-entry (named-entry function-space)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-ENTRY.

(defclass block-entry (block-space definition-entry)
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

(defclass go-tag-entry (tag-space definition-entry)
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

(defclass declaration-entry (environment-entry) ())

(defclass location-declaration-entry (declaration-entry)
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPE-DECLARATION-ENTRY.
;;;
;;; We do not have a separate declaration entry for FTYPE.

(defclass type-declaration-entry (location-declaration-entry)
  ((%type :initarg :type :reader type)))

(defun make-type-declaration-entry (location-entry type)
  (make-instance 'type-declaration-entry
		 :location (location location-entry)
		 :type type))

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

(defclass dynamic-extent-declaration-entry (location-declaration-entry)
  ())

(defun make-dynamic-extent-declaration-entry (location-entry)
  (make-instance 'dynamic-extent-declaration-entry
		 :location (location location-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IGNORE-DECLARATION-ENTRY.

(defclass ignore-declaration-entry (location-declaration-entry)
  ())

(defun make-ignore-declaration-entry (location-entry)
  (make-instance 'ignore-declaration-entry
		 :location (location location-entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class IGNORABLE-DECLARATION-ENTRY.

(defclass ignorable-declaration-entry (location-declaration-entry)
  ())

(defun make-ignorable-declaration-entry (location-entry)
  (make-instance 'ignorable-declaration-entry
		 :location (location location-entry)))

(defclass autonomous-declaration-entry (declaration-entry)
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class OPTIMIZE-DECLARATION-ENTRY.

(defclass optimize-declaration-entry (autonomous-declaration-entry)
  ((%quality :initarg :quality)
   (%value :initarg :value)))

(defun make-optimize-declaration-entry (quality &optional (value 3))
  (make-instance 'optimize-declaration-entry
		 :quality quality
		 :value value))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class DECLARATION-DECLARATION-ENTRY.

(defclass declaration-declaration-entry (autonomous-declaration-entry)
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
;;; Querying the environment.

(defun find-in-namespace (name environment namespace)
  (find-if (lambda (entry)
	     (and (typep entry namespace)
		  (equal (name entry) name)))
	   environment))

(defun find-type (entry env)
  `(and ,@(loop for e in env
		when (and (typep e 'type-declaration-entry)
			  (eq (location e) entry))
		  collect (type e))))

(defun find-inline-info (entry env)
  (loop for e in env
	do (when (and (typep e 'inline-or-notinline-declaration-entry)
		      (eq (location e) entry))
	     (return (if (typep e 'inline-declaration-entry)
			 :inline
			 :notinline)))))

(defun find-ignore-info (entry env)
  (cond ((loop for e in env
	       when (and (typep e 'ignore-declaration-entry)
			 (eq (location e) entry))
		 return t)
	 :ignore)
	((loop for e in env
	       when (and (typep e 'ignorable-declaration-entry)
			 (eq (location e) entry))
		 return t)
	 :ignorable)
	(t nil)))

(defun find-dynamic-extent-info (entry env)
  (loop for e in env
	when (and (typep e 'dynamic-extent-declaration-entry)
		  (eq (location e) entry))
	  return t))

(defun variable-info (name env)
  (let ((entry (find-in-namespace name env 'variable-space)))
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
  (let ((entry (find-in-namespace name env 'function-space)))
    (cond ((null entry)
	   nil)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The global environment.
;;;
;;; The first entry of the global environment is always a dummy entry,
;;; so that it is possible to side-effect the global environment by
;;; modifying the CDR of it.

(defparameter *global-environment*
  (list (make-instance 'dummy-entry)))

(defun add-to-global-environment (entry)
  (push entry (cdr *global-environment*)))

(defun find-variable (name environment)
  (find-in-namespace name environment 'variable-space))

(defun find-function (name environment)
  (find-in-namespace name environment 'function-space))

(defun find-block (name environment)
  (find-in-namespace name environment 'block-space))

(defun find-go-tag (name environment)
  (find-in-namespace name environment 'tag-space))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Macro expansion.

(defun macro-function (symbol &optional environment)
  (when (null environment)
    (setf environment *global-environment*))
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'macro-entry)
			       (eq (name entry) symbol)))
			environment)))
    (if (null entry)
	nil
	(definition entry))))

(defparameter *macroexpand-hook*
  (lambda (macro-function macro-form environment)
    (funcall macro-function macro-form environment)))

(defun macroexpand-1 (form &optional environment)
  (when (null environment)
    (setf environment *global-environment*))
  (let ((expander nil))
    (cond ((and (consp form) (symbolp (car form)))
	   (setf expander (macro-function (car form) environment)))
	  ((symbolp form)
	   (let ((entry (find-if (lambda (entry)
				   (and (typep entry 'symbol-macro-entry)
					(eq (name entry) form)))
				 environment)))
	     (if (null entry)
		 nil
		 (setf expander (definition entry)))))
	  (t nil))
    (if expander
	(values (funcall (coerce *macroexpand-hook* 'function)
			 expander
			 form
			 environment)
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
;;; Global environment.
;;;
;;; The FUNCTION namespace contains three types of entries:
;;; GLOBAL-FUNCTION-ENTRY, GLOBAL-MACRO-ENTRY, and
;;; SPECIAL-OPERATOR-ENTRY.  All special operator entries are global,
;;; because there is no mechanism for defining local special
;;; operators.
 
(defclass package ()
  ((%name :initarg :name :accessor name)
   (%nicknames :initarg :nicknames :initform '() :accessor nicknames)))

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
   ;; The function namespace.  It contains entries for
   ;; macros and functions.
   ;; FIXME: what about compiler macros?
   (%functions :initform '() :accessor functions)
   (%proclamations :initform '() :accessor proclamaitions)))

(defun function-name-p (object)
  (or (symbolp function-name)
      (and (consp function-name)
	   (eq (car function-name) 'setf)
	   (consp (cdr function-name))
	   (symbolp (cadr function-name))
	   (null (cddr function-name)))))

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

(defun fboundp (function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  (let ((entry (find function-name
		     (functions *global-environment*)
		     :key #'name
		     :test #'equal)
	       (functions *global-environment*)))
    (typecase entry
      ((or global-function-entry global-macro-entry)
       (bound entry))
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
  (let ((entry (find function-name
		     (functions *global-environment*)
		     :key #'name
		     :test #'equal)
	       (functions *global-environment*)))
    (unless (or (null entry)
		(typep entry 'special-operator-entry))
      (setf (bound entry) nil))))

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

(defun fdefinition (function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  (let ((entry (find function-name
		     (functions *global-environment*)
		     :key #'name
		     :test #'equal)
	       (functions *global-environment*)))
    (typecase entry
      (global-function-entry
       (if (bound entry)
	   (car (storage entry))
	   (error 'undefined-function :name function-name)))
      (global-macro-entry
       (if (bound entry)
	   (definition entry)
	   (error 'undefined-function :name function-name)))
      (special-operator-entry
       function-name)
      (t
       (error 'undefined-function :name function-name)))))

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
;;; In the case of an existing global-macro-entry, it is safe to
;;; remove it, because there can be no declarations, so no auxiliary
;;; entries referring to it.

(defun (setf fdefinition) (new-definition function-name)
  (unless (function-name-p function-name)
    (error "not a function name ~s" function-name))
  (unless (functionp new-definition)
    (error 'type-error :datum new-definition :expected-type 'function))
  (let ((entry (find function-name
		     (functions *global-environment*)
		     :key #'name
		     :test #'equal)
	       (functions *global-environment*)))
    (cond ((null entry)
	   (push (make-instance 'global-function-entry
		   :name function-name
		   :bound t)
		 (functions *global-environment*)))
	  ((typep entry 'special-operator-entry)
	   (error "can't replace a special operator"))
	  ((typep entry 'global-macro-entry)
	   (setf (functions *global-environment*)
		 (remove entry (functions *global-environment*)))
	   (push (make-instance 'global-function-entry
		   :name function-name
		   :bound t)
		 (functions *global-environment*)))
	  (t
	   (setf (car (storage entry)) new-definition))))
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
			       (eq (name entry) symbol))))
	       (functions *global-environment*)))
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
    (error 'type-error :datum environment :expected-type null))
  (unless (symbolp symbol)
    (error 'type-error :datum symbol :expected-type 'symbol))
  (unless (functionp new-function)
    (error 'type-error :datum new-function 'function))
  (setf (functions *global-environment*)
	(remove-if (lambda (entry)
		     (and (or (typep entry 'global-function-entry)
			      (typep entry 'global-macro-entry))
			  (eq (name entry))))
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
		   *global-environment*)
    (add-to-global-environment
     (make-instance 'declaration-declaration-entry
		    :name name))))

(defun proclaim-ftype (name type)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry 'global-function-entry)
			       (eq (name entry) name)))
			*global-environment*)))
    (when (null entry)
      (error "no function by that name"))
    (let ((existing-declaration
	    (find-if (lambda (decl)
		       (and (typep decl 'type-declaration-entry)
			    (eq (location decl) (location entry))))
		     *global-environment*)))
      (cond ((null existing-declaration)
	     (add-to-global-environment
	      (make-type-declaration-entry entry type)))
	    ((equal (type existing-declaration) type)
	     nil)
	    (t
	     ;; make that an error for now
	     (error "function already has a type proclamation"))))))

(defun proclaim (declaration-specifier)
  (case (car declaration-specifier)
    (declaration
     (mapc #'proclaim-declaration
	   (cdr declaration-specifier)))
    (ftype
     (mapc (lambda (name) (proclaim-ftype name (cadr declaration-specifier)))
	   (cddr declaration-specifier)))
    ;; FIXME: handle more proclamations
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Put some stuff in the global environment.

(add-to-global-environment
 (make-constant-variable-entry 'pi pi))

(add-to-global-environment
 (make-special-variable-entry '*read-base*))

(add-to-global-environment
 (make-global-function-entry 'car))

(add-to-global-environment
 (make-global-function-entry 'error))

(add-to-global-environment
 (make-global-function-entry '(setf fdefinition)))

(add-to-global-environment
 (make-global-function-entry 'funcall))

(add-to-global-environment
 (make-local-macro-entry
  'when
  (lambda (form env)
    (declare (ignore env))
    `(if ,(cadr form)
	 (progn ,(cddr form))
	 nil))))

