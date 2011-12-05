(in-package :sicl-clos)

;;; This condition is used to mix into other conditions that
;;; will report the construct (function, macro, etc) in which 
;;; the condition was signaled. 
(define-condition name-mixin ()
  ((%name :initarg :name :reader name)))

(define-condition no-such-class-name (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition must-be-class-or-nil (type-error name-mixin)
  ()
  (:default-initargs :type '(or class null)))

(define-condition superclass-list-must-be-proper-list
    (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition class-name-must-be-non-nil-symbol
    (type-error name-mixin)
  ()
  (:default-initargs :type '(and symbol (not null))))

(define-condition malformed-slots-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-class-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-documentation-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-metaclass-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-default-initargs-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility functions

;;; FIXME: check for circular lists
(defun proper-list-p (object)
  (or (null object)
      (and (consp object)
	   (null (cdr (last object))))))

(defclass slot-definition ()
  ((name :initarg :name :accessor slot-definition-name)
   (initargs :initarg :initargs :accessor slot-definition-initargs)
   (initform :initarg :initform :accessor slot-definition-initform)
   (initfunction :initarg :initfunction :accessor slot-definition-initfunction)
   (readers :initarg :readers :accessor slot-definition-readers)
   (writers :initarg :writers :accessor slot-definition-writers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions find-class and (setf find-class)
;;;
;;; For now, ignore the environment parameter to FIND-CLASS, and just
;;; use a global variable to hold a hash table with class definitions.

(defparameter *classes* (make-hash-table :test #'eq))

(defun find-class (name &optional (errorp t) environment)
  (declare (ignore environment))
  (let ((result (gethash name *classes*)))
    (if (null result)
	(if errorp
	    (error 'no-such-class-name
		   :name 'find-class
		   :datum name)
	    nil)
	result)))

(defun (setf find-class) (new-class name &optional errorp environment)
  (declare (ignore errorp environment))
  (if (null new-class)
      (remhash name *classes*)
      (if (not (typep new-class 'class))
	  (error 'must-be-class-or-nil
		 :name '(setf find-class)
		 :datum new-class)
	  (setf (gethash name *classes*) new-class))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function ensure-class.  

(defun ensure-class (name &rest arguments)
  (if (not (null (find-class name nil)))
      ;; FIXME: handle redefinitions here later
      (assert nil () "can't handle redefined classes yet")
      (let ((class (apply #'make-instance
			  :name name
			  arguments)))
	(setf (find-class name) class)
	;; Make it explicit that we return the
	;; newly created class.
	class)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to canonicalize certain parts of the defclass macro

(defun canonicalize-direct-superclass (class-name)
  (unless (and (symbolp class-name)
	       (not (null class-name)))
    (error 'class-name-must-be-non-nil-symbol
	   :datum class-name))
  `(find-class ',class-name))

(defun canonicalize-direct-superclasses (direct-superclasses)
  (unless (proper-listp direct-superclasses)
    (error 'superclass-list-must-be-proper-list
	   :name 'defclass
	   :datum direct-superclasses))
  `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses)))

;;; Make sure each class options is well formed, and check that a
;;; class option appears at most once.  Return a list of class
;;; options, including the corresponding keyword argument, to be
;;; spliced into the call to ENSURE-CLASS.
(defun canonicalize-defclass-options (options)
  (let (default-initargs-option documentation-option metaclass-option)
    (loop for option in options
	  do (unless (consp option)
	       (error 'malformed-class-option
		      :name 'defclass
		      :datum option))
	  do (unless (member (car option)
			     '(:default-initargs :documentation :metaclass))
	       (error 'unknown-class-option
		      :name 'defclass
		      :datum (car option)))
	  do (ecase (car option)
	       (:default-initargs
		   (unless (null default-initargs-option)
		     (error 'default-initarg-option-once
			    :name 'defclass
			    :datum option))
		   (setf default-initargs-option option))
	       (:documentation
		  (unless (and (consp (cdr option))
			       (null (cddr option)))
		    (error 'malformed-documentation-option
			   :name 'defclass
			   :datum option))
		  (unless (null documentation-option)
		    (error 'documentation-option-once
			   :name 'defclass
			   :datum option))
		  (setf documentation-option option))
	       (:metaclass
		  (unless (and (consp (cdr option))
			       (null (cddr option)))
		    (error 'malformed-metaclass-option
			   :name 'defclass
			   :datum option))
		  (unless (null metaclass-option)
		    (error 'metaclass-option-once
			   :name 'defclass
			   :datum option))
		  (setf metaclass-option option))))
    `(,@(unless (null default-initargs-option)
	  `(:default-initargs ,(cdr default-initargs-option)))
      ,@documentation-option
      ,@metaclass-option)))

;;; The DEFCLASS macro.  The AMOP is inconsistent with respect to the
;;; CLHS.  For instance, it requires the arguments to ENSURE-CLASS to
;;; appear in the same order as they appear in the DEFCLASS form, but
;;; that should not matter since they are not evaluated.  Furthermore,
;;; the AMOP talks about additional calss options, but no such
;;; additional options are permitted according to the CLHS.  We follow
;;; the CLHS.

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  (when (or (not (listp direct-slots))
	    (not (null (last direct-slots 0))))
    (error 'malformed-slots-list
	   :name 'defclass
	   :datum direct-slots))
  `(ensure-class ,name
		 :direct-superclasses ,direct-superclasses
		 :direct-slots ,(mapcar #'canonicalize-slot-spec direct-slots)
		 ,@(canonicalize-defclass-options options)))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Provide an :after method on initialize-instance to be used
;;; when class meta-objects are instantiated.  As the AMOP says,
;;; we have to provide appropriate default values for the superclasses
;;; and we have to link the superclasses to this new class meta-object.
;;; We also have to create slot meta-objects from the slot property
;;; list provided to initialize-instance.

(defmethod initialize-instance :after ((class standard-class)
				       &key
				       direct-superclasses
				       direct-slots)
  

  (let ((superclasses (or direct-superclasses
			  (list (find-class 'standard-object)))))
    (setf (class-direct-superclasses class) superclasses)
    ;; Make sure this class meta-object is a direct subclass of
    ;; all its direct superclasses. 
    (loop for superclass in superclasses
	  do (push class (class-direct-subclasses superclass))))
  (let ((slots (loop for slot-proerties in direct-slots
		     collect (apply #'make-direct-slot-definition
				    slot-properties))))
    (setf (class-direct-slots slots))
    (loop for slot in slots
	  do (loop for readers in (slot-definition-readers slot)
		   do (add-reader-method class
			                 reader
					 (slot-definition-name slot)))
	     (loop for writers in (slot-definition-writers slot)
		   do (add-writer-method class
			                 writer
					 (slot-definition-name slot)))))
  (finalize-inheritance class))
			 

;;; (defun canonicalize-slot-spec (slot-spec)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We follow the AMOP and define functions to allocate
;;; class instances and slot storage.  For now, these
;;; functions create other Common Lisp objects such as 
;;; structures and arrays.  In a real implementation, 
;;; these functions might use lower-level mechanisms
;;; to allocate these entities.

(defstruct standard-instance class slots)

(defun allocate-standard-instance (class slot-storage)
  (make-standard-instance :class class
			  :slots slot-storage))

(defun allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

(defun slot-contents (slot-storage location)
  (aref slot-storage location))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Allocate an instance

(deparameter *secret-unbound-value* (list "slot unbound"))

(defun allocate-instance (class)
  (allocate-standard-instance
   class
   (allocate-slot-storage (length (class-slots class))
			  *secret-unbound-value*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Builtin classes



