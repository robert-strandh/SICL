(defpackage #:sicl-compiler-environment
  (:nicknames #:sicl-env)
  (:use #:common-lisp)
  (:shadow #:type
	   #:proclaim
	   #:macroexpand-1
	   #:macroexpand
	   #:macro-function
	   #:*macroexpand-hook*
	   )
  (:export
   #:constant-variable-entry #:make-constant-variable-entry
   #:special-variable-entry #:make-special-variable-entry
   #:lexical-variable-entry #:make-lexical-variable-entry
   #:symbol-macro-entry #:make-symbol-macro-entry
   #:global-function-entry #:make-global-function-entry
   #:local-function-entry #:make-local-function-entry
   #:macro-entry #:make-macro-entry
   #:block-entry #:make-block-entry
   #:go-tag-entry #:make-go-tag-entry
   #:type-declaration-entry #:make-type-declaration-entry
   #:ftype-declaration-entry #:make-ftype-declaration-entry
   #:inline-declaration-entry #:make-inline-declaration-entry
   #:notinline-declaration-entry #:make-notinline-declaration-entry
   #:dynamic-extent-declaration-entry #:make-dynamic-extent-declaration-entry
   #:ignore-decalration-entry #:make-ignore-decalration-entry
   #:ignorable-declaration-entry #:make-ignorable-declaration-entry
   #:optimize-declaration-entry #:make-optimize-declaration-entry
   #:declaration-declaration-entry #:make-declaration-declaration-entry
   #:definition
   #:location #:lexical-location
   #:storage
   #:name
   #:type
   #:*global-environment*
   #:add-to-global-environment
   #:make-entry-from-declaration
   #:find-variable
   #:find-function
   #:find-block
   #:find-go-tag
   #:macroexpand-1
   #:macroexpand
   #:find-type
   #:find-ftype
   #:find-fype  ; remove later
   #:augment-environment
   #:augment-environment-with-declarations
   ))

(in-package #:sicl-compiler-environment)

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
(defclass location () ())

;;; Every use of the environment must get the same
;;; place, so the storage is allocated here. 
(defclass global-location (location)
  ((%storage :initform (list nil) :reader storage)))

;;; For special locations, we give the symbol which
;;; must then be used at runtime to access the value. 
(defclass special-location (location)
  ((%name :initarg :name :reader name)))

;;; For a lexical location, we do not determine the
;;; place for it at all.  This is done by the register 
;;; allocator and other backend functions.
(defclass lexical-location (location)
  ())

(defclass location-entry (entry named-entry)
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The different types of entries. 

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class SPECIAL-VARIABLE-ENTRY.
;;;

(defclass special-variable-entry (variable-space location-entry)
  ())
  
(defun make-special-variable-entry (name)
  (make-instance 'special-variable-entry
		 :name name
		 :location (make-instance 'special-location
					  :name name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LEXICAL-VARIABLE-ENTRY.
;;;

(defclass lexical-variable-entry (variable-space location-entry)
  ())

(defun make-lexical-variable-entry (name)
  (make-instance 'lexical-variable-entry
		 :name name
		 :location (make-instance 'lexical-location)))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GLOBAL-FUNCTION-ENTRY.

(defclass global-function-entry (function-space location-entry)
  ())

(defun make-global-function-entry (name)
  (make-instance 'global-function-entry
		 :name name
		 :location (make-instance 'global-location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class LOCAL-FUNCTION-ENTRY.

(defclass local-function-entry (function-space location-entry)
  ())

(defun make-local-function-entry (name)
  (make-instance 'local-function-entry
		 :name name
		 :location (make-instance 'lexical-location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class MACRO-ENTRY.

(defclass macro-entry (function-space definition-entry)
  ())

(defun make-macro-entry (name expander)
  (make-instance 'macro-entry
		 :name name
		 :definition expander))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class BLOCK-ENTRY.

(defclass block-entry (block-space definition-entry)
  ())
  
(defun make-block-entry (name block)
  (make-instance 'block-entry
		 :name name
		 :definition block))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class GO-TAG-ENTRY.

(defclass go-tag-entry (tag-space definition-entry)
  ())

(defun make-go-tag-entry (name tag)
  (make-instance 'go-tag-entry
		 :name name
		 :definition tag))

(defclass declaration-entry (environment-entry) ())

(defclass location-declaration-entry (declaration-entry)
  ((%location :initarg :location :reader location)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class TYPE-DECLARATION-ENTRY.

(defclass type-declaration-entry (location-declaration-entry)
  ((%type :initarg :type :reader type)))

(defun make-type-declaration-entry (location-entry type)
  (make-instance 'type-declaration-entry
		 :location (location location-entry)
		 :type type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class FTYPE-DECLARATION-ENTRY.

(defclass ftype-declaration-entry (location-declaration-entry)
  ((%type :initarg :type :reader type)))

(defun make-ftype-declaration-entry (location-entry type)
  (make-instance 'ftype-declaration-entry
		 :location (location location-entry)
		 :type type))

(defclass inline-or-notinline-declaration-entry (location-declaration-entry)
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

(defparameter *global-environment*
  (list (make-instance 'dummy-entry)))

(defun add-to-global-environment (entry)
  (push entry (cdr *global-environment*)))

(defun find-in-namespace (name environment namespace)
  (let ((entry (find-if (lambda (entry)
			  (and (typep entry namespace)
			       (equal (name entry) name)))
			environment)))
    (when (null entry)
      ;; FIXME: do this better
      (error "no such name ~s" name))
    entry))
  
(defun find-variable (name environment)
  (find-in-namespace name environment 'variable-space))

(defun find-function (name environment)
  (find-in-namespace name environment 'function-space))

(defun find-block (name environment)
  (find-in-namespace name environment 'block-space))

(defun find-go-tag (name environment)
  (find-in-namespace name environment 'go-tag-space))

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
	 (make-ftype-declaration-entry entry (car rest))))
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
	      (make-ftype-declaration-entry entry type)))
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
;;; Find the type declarations associated with an entry.

(defun find-type (location environment)
  `(and ,@(loop for entry in environment
		when (and (typep entry 'type-declaration-entry)
			  (eq (location entry) location))
		  collect (type entry))))

(defun find-ftype (location environment)
  `(and ,@(loop for entry in environment
		when (and (typep entry 'ftype-declaration-entry)
			  (eq (location entry) location))
		  collect (type entry))))

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
;;; Put some stuff in the global environment.

(add-to-global-environment
 (make-constant-variable-entry 'pi pi))

(add-to-global-environment
 (make-special-variable-entry '*read-base*))

(add-to-global-environment
 (make-global-function-entry 'car))

(add-to-global-environment
 (make-global-function-entry '(setf fdefinition)))

(add-to-global-environment
 (make-global-function-entry 'funcall))

(add-to-global-environment
 (make-macro-entry 'when
		   (lambda (form env)
		     (declare (ignore env))
		      `(if ,(cadr form)
			   (progn ,(cddr form))
			   nil))))

