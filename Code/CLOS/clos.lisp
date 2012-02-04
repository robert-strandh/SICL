(in-package :sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conditions

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

(define-condition malformed-slot-spec (type-error name-mixin)
  ()
  (:default-initargs :type '(or symbol list)))

(define-condition illegal-slot-name (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition slot-options-must-be-even (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition slot-option-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition multiple-initform-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-documentation-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-allocation-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition multiple-type-options-not-permitted (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition slot-documentation-option-must-be-string (type-error name-mixin)
  ()
  (:default-initargs :type 'string))

(define-condition class-option-must-be-non-empty-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition class-option-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

;;; FIXME: This doesn't feel like a type-error
(define-condition duplicate-class-option-not-allowed (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition malformed-documentation-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition malformed-metaclass-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition default-initargs-must-be-proper-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition default-initargs-must-have-even-length (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition default-initarg-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

(define-condition malformed-default-initargs-option (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The contents of this file should be read sequentially, because the
;;; bootstrapping issues make it necessary to create functionality
;;; step by step.  FIXME: elaborate...

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Utility functions

;;; FIXME: check for circular lists
(defun proper-list-p (object)
  (or (null object)
      (and (consp object)
	   (null (cdr (last object))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions find-class and (setf find-class)
;;;
;;; For now, ignore the environment parameter to FIND-CLASS, and just
;;; use a global variable to hold a hash table with class definitions.

;;; Classes use symbols as names, so we create an EQ hash table to map
;;; class names to classes.  
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

;;; This function should check the type of its argument, but
;;; we don't have class types working yet at this point.  
(defun (setf find-class) (new-class name &optional errorp environment)
  (declare (ignore errorp environment))
  (if (null new-class)
      (remhash name *classes*)
      (setf (gethash name *classes*) new-class)))

;;; What we really want to do is this:
;; (defun (setf find-class) (new-class name &optional errorp environment)
;;   (declare (ignore errorp environment))
;;   (if (null new-class)
;;       (remhash name *classes*)
;;       (if (not (typep new-class 'class))
;; 	  (error 'must-be-class-or-nil
;; 		 :name '(setf find-class)
;; 		 :datum new-class)
;; 	  (setf (gethash name *classes*) new-class))))

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

(defparameter *secret-unbound-value* (list "slot unbound"))

(defun allocate-instance (class)
  (allocate-standard-instance
   class
   (allocate-slot-storage (length (class-slots class))
			  *secret-unbound-value*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Allocating direct slot definitions
;;;
;;; To create any class metaobject whatsoever, we need to fill its
;;; DIRECT-SLOTS slot with instances of
;;; STANDARD-DIRECT-SLOT-DEFININTION.  But
;;; STANDARD-DIRECT-SLOT-DEFININTION is itself a class metaobject so
;;; we have a circular dependency.  We fix this problem by allocating
;;; direct slot definitions "manually", i.e., without the help of
;;; MAKE-INSTANCE and the machinery that comes with it.

(defparameter *class-standard-direct-slot-definition*
  (let ((class (allocate-standard-instance nil nil)))
    (setf (find-class 'standard-direct-slot-definition) class)
    class))
    
;;; REMEMBER: Fill in standard-direct-slot-definition. +

;;; We cheat by creating a list of descriptions (name and one initarg)
;;; of the direct slots that we know we would en up with if we had
;;; used the normal machinery to create the class. 

(defparameter *standard-direct-slot-definition-slots*
  '(;; Inherited from standard-object.  This information is what
    ;; allows us to update instances when a class changes. 
    (%version-information :version-information)
    (%name :name)
    (%allocation :allocation)
    (%type :type)
    (%initargs :initargs)
    (%initform :initform)
    (%initfunction :initfunction)
    (%readers :readers)
    (%writers :writers)))

(defun make-direct-slot-definition (&rest initargs
				    &key &allow-other-keys)
  (let* ((slot-descriptions *standard-direct-slot-definition-slots*)
	 (slot-storage (allocate-slot-storage (length slot-descriptions)
					      *secret-unbound-value*)))
    (flet ((set-slot-value (initarg value)
	     (let ((position (position initarg slot-descriptions :key #'cadr)))
	       (setf (aref slot-storage position) value))))
      ;; Give default value for :version-information.  Fill in later.
      (set-slot-value :version-information nil)
      ;; Give default values for :type and :allocation
      (set-slot-value :type t)
      (set-slot-value :allocation :instance)
      (loop for (initarg value) on initargs by #'cddr
	    do (set-slot-value initarg value)))
    ;; Check that no slot is unbound.
    (loop for i from 0 below (length slot-descriptions)
	  do (assert (not (null (slot-contents slot-storage i)))))
    ;; Allocate and return the final object.
    (allocate-standard-instance *class-direct-slot-definition*
				slot-storage)))

;;; REMEMBER: Change make-direct-slot-definition to use make-instance. +
;;; REMEMBER: Fill in version information of slot-definition-instances. +

(defun slot-of-standard-direct-slot-definition (instance slot-name)
  (slot-contents (standard-instance-slots standard-direct-slot-definition)
		 (position slot-name
			   *standard-direct-slot-definition-slots*
			   :key #'car)))

(defun slot-definition-name (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%name))

;;; REMEMBER: Change slot-defintion-name to be a reader function. +

(defun slot-definition-allocation (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%allocation))

;;; REMEMBER: Change slot-defintion-allocation to be a reader function. +

(defun slot-definition-type (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%type))

;;; REMEMBER: Change slot-defintion-type to be a reader function. +

(defun slot-definition-initargs (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%initargs))

;;; REMEMBER: Change slot-defintion-initargs to be a reader function. +

(defun slot-definition-initform (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%initform))

;;; REMEMBER: Change slot-defintion-initform to be a reader function. +

(defun slot-definition-initfunction (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%initfunction))

;;; REMEMBER: Change slot-defintion-initfunction to be a reader function. +

(defun slot-definition-readers (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%readers))

;;; REMEMBER: Change slot-defintion-readers to be a reader function. +

(defun slot-definition-writers (slot-definition)
  (slot-of-standard-direct-slot-definition slot-definition '%writers))

;;; REMEMBER: Change slot-defintion-writers to be a reader function. +

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions ensure-class-using-class and ensure-class.  

;;; Since we don't have generic functions yet, we define
;;; ensure-class-using-class as an ordinary function.  Furthermore, we
;;; only implement its functionality for the case when the class is
;;; NIL.

(defun method-ensure-class-using-class-null (name &rest keys)
  (let ((class (apply #'make-instance 'standard-class keys)))
    (setf (find-class name) class)))

;; (defgeneric ensure-class-using-class (class name
;; 				      &key
;; 				      direct-default-initargs
;; 				      direct-slots
;; 				      direct-superclasses
;; 				      ;; name ???
;; 				      metaclass
;; 				      &allow-other-keys))

;; (defmethod ensure-class-using-class ((class class) name
;; 				     &key
;; 				     metaclass
;; 				     direct-superclasses
;; 				     &allow-other-keys)
;;   nil)

;; (defmethod ensure-class-using-class ((class forward-referenced-class) name
;; 				     &key
;; 				     metaclass
;; 				     direct-superclasses
;; 				     &allow-other-keys)
;;   nil)

;; (defmethod ensure-class-using-class ((class null) name
;; 				     &key
;; 				     metaclass
;; 				     direct-superclasses
;; 				     &allow-other-keys)
;;   nil)

(defun ensure-class (name &rest arguments &key &allow-other-keys)
  (unless (symbolp name)
    (error 'class-name-must-be-non-nil-symbol
	   :name 'ensure-class
	   :datum name))
  (apply #'ensure-class-using-class
	 (find-class name nil)
	 name
	 arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions to canonicalize certain parts of the defclass macro

(defun canonicalize-direct-superclass-name (class-name)
  (unless (and (symbolp class-name)
	       (not (null class-name)))
    (error 'class-name-must-be-non-nil-symbol
	   :name 'defclass
	   :datum class-name))
  class-name)

(defun canonicalize-direct-superclass-names (direct-superclass-names)
  (unless (proper-list-p direct-superclass-names)
    (error 'superclass-list-must-be-proper-list
	   :name 'defclass
	   :datum direct-superclasses))
  (loop for name in direct-superclass-names
        collect (canonicalize-direct-superclass-name name)))

(defun canonicalize-direct-slot-spec (direct-slot-spec)
  ;; A direct-slot-spec can be a symbol which is then the
  ;; name of the slot.
  (if (symbolp direct-slot-spec)
      `(:name ',direct-slot-spec)
      (progn
	;; If the direct-slot-spec is not a symbol, it must
	;; be a non-empty proper list.
	(unless (and (proper-list-p direct-slot-spec)
		     (consp direct-slot-spec))
	  (error 'malformed-slot-spec
		 :name 'defclass
		 :datum direct-slot-spec))
	;; In that case, the first element must be the name
	;; of the slot, which must be a symbol.
	(unless (symbolp (car direct-slot-spec))
	  (error 'illegal-slot-name
		 :name 'defclass
		 :datum (car direct-slot-spec)))
	;; The slot options must be a list of even length
	;; where every other element is the name of a slot
	;; option and every other element is the value of
	;; the slot option.
	(unless (evenp (length (cdr direct-slot-spec)))
	  (error 'slot-options-must-be-even
		 :name 'defclass
		 :datum direct-slot-spec))
	(let ((ht (make-hash-table :test #'eq)))
	  (loop for (name value) on (cdr direct-slot-spec) by #'cddr
	        do (unless (symbolp name)
		     (error 'slot-option-name-must-be-symbol
			    :name 'defclass
			    :datum name))
	           (push value (gethash name ht '())))
	  (let ((result `(:name ',(car direct-slot-spec))))
	    (flet ((add (name value)
		     (setf result (append result (list name value)))))
	      ;; Check and process :initform option.
	      (multiple-value-bind (value flag)
		  (gethash :initform ht)
		(when flag
		  (unless (= (length value) 1)
		    (error 'multiple-initform-options-not-permitted
			   :datum direct-slot-spec))
		  (add :initform (car value))
		  (add :initfunction `(lambda () ,(car value)))
		  (remhash :initform ht)))
	      ;; Process :initarg option.
	      (multiple-value-bind (value flag)
		  (gethash :initarg ht)
		(when flag
		  (add :initargs (reverse value))
		  (remhash :initarg ht)))
	      ;; Turn :accessor into :reader and :writer
	      (multiple-value-bind (value flag)
		  (gethash :accessor ht)
		(when flag
		  (loop for accessor in value
		        do (push accessor (gethash :reader ht '()))
		           (push `(setf ,accessor) (gethash :writer ht '())))
		  (remhash :accessor ht)))
	      ;; Process :reader option.
	      (multiple-value-bind (value flag)
		  (gethash :reader ht)
		(when flag
		  (add :readers (reverse value))
		  (remhash :reader ht)))
	      ;; Process :writer option.
	      (multiple-value-bind (value flag)
		  (gethash :writer ht)
		(when flag
		  (add :writers (reverse value))
		  (remhash :writer ht)))
	      ;; Check and process :documentation option.
	      (multiple-value-bind (value flag)
		  (gethash :documentation ht)
		(when flag
		  (unless (= (length value) 1)
		    (error 'multiple-documentation-options-not-permitted
			   :datum direct-slot-spec))
		  (unless (stringp (car value))
		    (error 'slot-documentation-option-must-be-string
			   :datum (car value)))
		  (add :documentation (car value))
		  (remhash :documentation ht)))
	      ;; Check and process :allocation option.
	      (multiple-value-bind (value flag)
		  (gethash :allocation ht)
		(when flag
		  (unless (= (length value) 1)
		    (error 'multiple-allocation-options-not-permitted
			   :datum direct-slot-spec))
		  (add :allocation (car value))
		  (remhash :allocation ht)))
	      ;; Check and process :type option.
	      (multiple-value-bind (value flag)
		  (gethash :type ht)
		(when flag
		  (unless (= (length value) 1)
		    (error 'multiple-type-options-not-permitted
			   :datum direct-slot-spec))
		  (add :type (car value))
		  (remhash :type ht)))
	      ;; Add remaining options without checking.
	      (maphash (lambda (name value)
			 (add name (reverse value)))
		       ht))
	    result)))))

(defun canonicalize-direct-slot-specs (direct-slot-specs)
  (when (not (proper-list-p direct-slot-specs))
    (error 'malformed-slots-list
	   :name 'defclass
	   :datum direct-slot-specs))
  (loop for spec in direct-slot-specs
        collect (canonicalize-direct-slot-spec spec)))

;;; Make sure each class options is well formed, and check that a
;;; class option appears at most once.  Return a list of class
;;; options, including the corresponding keyword argument, to be
;;; spliced into the call to ENSURE-CLASS.
(defun canonicalize-defclass-options (options)
  ;; Check that each option is a non-empty list
  (let ((potential-malformed-option (member-if-not #'consp options)))
    (unless (null potential-malformed-option)
      (error 'class-option-must-be-non-empty-list
	     :name 'defclass
	     :datum (car potential-malformed-option))))
  ;; Check that the name of each option is a symbol
  (let ((potential-malformed-option (member-if-not #'symbolp options :key #'car)))
    (unless (null potential-malformed-option)
      (error 'class-option-name-must-be-symbol
	     :name 'defclass
	     :datum (car potential-malformed-option))))
  ;; Check that there are no duplicate option names
  (let ((reduced-options (remove-duplicates options :key #'car :test #'eq)))
    (when (< (length reduced-options) (length options))
      (loop for option in reduced-options
	    do (when (> (count (car option) options :key #'car :test #'eq) 1)
		 (error 'duplicate-class-option-not-allowed
			:name 'defclass
			:datum (car option))))))
  (let ((result '()))
    (loop for option in options
          do (case (car option)
	       (:default-initargs
		(unless (proper-list-p (cdr option))
		  (error 'default-initargs-must-be-proper-list
			 :datum option))
		(unless (evenp (length (cdr option)))
		  (error 'default-initargs-must-have-even-length
			 :datum option))
		(let ((canonicalized-initargs '()))
		  (loop for (name value) on (cdr option) by #'cddr
		        do (unless (symbolp name)
			     (error 'default-initarg-name-must-be-symbol
				    :datum name))
		        do (setf canonicalized-initargs
				 (append canonicalized-initargs
					 `((,name ,value (lambda () ,value))))))
		  (setf result
			(append result `(:default-initargs ,canonicalized-initargs)))))
	       (:documentation
		(unless (null (cddr option))
		  (error 'malformed-documentation-option
			 :name 'defclass
			 :datum option))
		(setf result
		      (append result `(:documentation ,(cadr option)))))
	       (:metaclass
		  (unless (null (cddr option))
		    (error 'malformed-metaclass-option
			   :name 'defclass
			   :datum option))
		(setf result
		      (append result `(:metaclass ,(cadr option)))))
	       (t 
		(setf result
		      (append result `(,(car option) ,(cdr option)))))))
    result))

;;; The DEFCLASS macro.  The AMOP is inconsistent with respect to the
;;; CLHS.  For instance, it requires the arguments to ENSURE-CLASS to
;;; appear in the same order as they appear in the DEFCLASS form, but
;;; that should not matter since they are not evaluated.  Furthermore,
;;; the AMOP talks about additional calss options, but no such
;;; additional options are permitted according to the CLHS.  We follow
;;; the CLHS.

(defmacro defclass (name
		    superclass-names
		    slot-specifiers
		    &rest options)
  `(ensure-class ',name
		 :direct-superclasses 
		 ,(canonicalize-direct-superclass-names superclass-names)
		 :direct-slots
		 ,(canonicalize-direct-slot-specs slot-specifiers)
		 ,@(canonicalize-defclass-options options)))
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Provide an :after method on initialize-instance to be used
;;; when class meta-objects are instantiated.  As the AMOP says,
;;; we have to provide appropriate default values for the superclasses
;;; and we have to link the superclasses to this new class meta-object.
;;; We also have to create slot meta-objects from the slot property
;;; list provided to initialize-instance.

;;; We implement this functionality as a function at first, because
;;; we don't have any generic functions to begin with.  Later, 
;;; we have the real :after method call this function to accomplish
;;; its work. 
(defun initialize-instance-after-standard-class
    (class direct-superclasses direct-slots)
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
    (setf (class-direct-slots slots))))
    

;;; Now we add the :after method on initialize-instance for
;;; standard-class.  It just calls the function we defined earlier.
;; (defmethod initialize-instance :after ((class standard-class)
;; 				       &key
;; 				       direct-superclasses
;; 				       direct-slots)
;;   (initialize-instance-after-standard-class
;;    class direct-superclasses direct-slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The Metaobject hierarchy.

;;; The Metaobject hierarchy according to the AMOP table 5.1.
;;;
;;; Metaobject class                   Direct superclasses
;;; ----------------                   -------------------
;;; standard-object                    (t)
;;; funcallable-standard-object        (standard-object function)
;;; metaobject                         (standard-object)
;;; generic-function                   (metaobject
;;;                                     funcallable-standard-object)
;;; standard-generic-function          (generic-function)
;;; method                             (metaobject)
;;; standard-method                    (method)
;;; standard-accesssor-method          (standard-method)
;;; standard-reader-method             (standard-accesssor-method)
;;; standard-writer-method             (standard-accesssor-method)
;;; method-combination                 (metaobject)
;;; slot-definition                    (metaobject)
;;; direct-slot-definition             (slot-definition)
;;; effective-slot-definition          (slot-definition)
;;; standard-slot-definition           (slot-definition)
;;; standard-direct-slot-definition    (standard-slot-definition 
;;;                                     direct-slot-definition)
;;; standard-effective-slot-definition (standard-slot-definition 
;;;                                     effective-slot-definition)
;;; specializer                        (metaobject)
;;; eql-specializer                    (specializer)
;;; class                              (specializer)
;;; built-in-class                     (class)
;;; forward-reference-class            (class)
;;; standard-class                     (class)
;;; funcallable-standard-class         (class)
;;; 
;;; The class t is an instance of built-in-class.
;;;
;;; The classes generic-function and standard-generic-function
;;; are instances of funcallable-standard-class.
;;;
;;; All other other classes are instances of standard-class.

;;; This fact creates a circular dependency in that standard-class
;;; is an instance of standard-class, so it must exist before it is
;;; created.  We break this dependency by bulding the instance of
;;; standard-class "manually", i.e. without the use of defclass. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Metaobject slot-definition

(defclass slot-definition ()
  ((name :initarg :name :accessor slot-definition-name)
   (initargs :initarg :initargs :accessor slot-definition-initargs)
   (initform :initarg :initform :accessor slot-definition-initform)
   (initfunction :initarg :initfunction :accessor slot-definition-initfunction)
   (readers :initarg :readers :accessor slot-definition-readers)
   (writers :initarg :writers :accessor slot-definition-writers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Builtin classes



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Bootstrapping issues.

;;; While it is perfectly possible to figure out the structure of each
;;; class metaobject in advance, and to create all those class
;;; metaobjects "manually", it would be convenient to use as much as
;;; possible of the machinery as early as possible in the
;;; bootstrapping process.
;;;
;;; One problem is that standard-class has a bunch of superclasses
;;; that are nearly all instances of standard-class.  It would appear
;;; then, that all the classes in that inheritance chain would need to
;;; exist before standard-class is created, so that all those classed
;;; need to be created "manually".  However, in order to create an
;;; instance of a class, all information about that class need not be
;;; known.  
;;;
;;; So, what exactly is needed in order to create an instance of a
;;; class?  Well, creating an instance of a class using DEFCLASS,
;;; generates a call to some generic functions, so these generic
;;; functions must exist, and the dispatch mechanism must function.
;;; We will discuss this issue below.  
;;;
;;; The DEFCLASS macro generates a call to ENSURE-CLASS which
;;; essentially only calls ENSURE-CLASS-USING-CLASS.  When a new class
;;; is created, ENSURE-CLASS-USING-CLASS calls MAKE-INSTANCE, which
;;; creates a standard-instance and then calls INITIALIZE-INSTANCE to
;;; fill in the slots.  INITIALIZE-INSTANCE calls SHARED-INITIALIZE to
;;; do most of the job.  SHARED-INITIALIZE uses CLASS-OF to find the
;;; class of the instance it is initializing, and then calls
;;; CLASS-SLOTS on that class.  So obviously, CLASS-SLOTS must exist.
;;; It is an accessor of STANDARD-CLASS.  The method on CLASS-SLOTS
;;; specialized for STANDARD-CLASS should directly access the slot
;;; vector using the position of the slot, so no other mechanism is
;;; needed with respect to STANDARD-CLASS, but obviously, that method
;;; must be created beforehand somehow.  
;;;
;;; The return value of CLASS-SLOTS is a list of instances of the
;;; class EFFECTIVE-SLOT-DEFINITION.  So this class must exist and we
;;; must be able to create instance of it.  After SHARED-INITIALIZE
;;; has found the slots of the class metaobject, it uses accessors
;;; SLOT-DEFINITION-NAME, SLOT-DEFINITION-INITARGS, and
;;; SLOT-DEFINITION-INITFUNCTION, so these must exist as well.
;;;
;;; Finally, SHARED-INITIALIZE uses SLOT-BOUNDP (SETF SLOT-VALUE).
;;; These both use CLASS-SLOTS on the class of the instance to find
;;; the appropriate index of the slot vector of the instance. 
;;; 
;;; 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating the hierarchy of class metaobjects.

;;; Start by creating every class as a standard instance with
;;; no slot storage and no metaclass. 

;; (defmacro make-class (name)
;;   `(defparameter ,name (allocate-standard-instance nil nil)))

;; (make-class *class-t*)
;; (make-class *class-standard-object*)
;; (make-class *class-funcallable-standard-object*)
;; (make-class *class-metaobject*)
;; (make-class *class-generic-function*)
;; (make-class *class-standard-generic-function*)
;; (make-class *class-method*)
;; (make-class *class-standard-method*)
;; (make-class *class-standard-accessor-method*)
;; (make-class *class-standard-reader-method*)
;; (make-class *class-standard-writer-method*)
;; (make-class *class-method-combination*)
;; (make-class *class-slot-definition*)
;; (make-class *class-direct-slot-definition*)
;; (make-class *class-effective-slot-definition*)
;; (make-class *class-standard-slot-definition*)
;; (make-class *class-standard-direct-slot-definition*)
;; (make-class *class-standard-effective-slot-definition*)
;; (make-class *class-specializer*)
;; (make-class *class-eql-specializer*)
;; (make-class *class-class*)
;; (make-class *class-built-in-class*)
;; (make-class *class-forward-reference-class*)
;; (make-class *class-standard-class*)
;; (make-class *class-funcallable-standard-class*)

;; ;;; Make a list of all standard-classes in top-down breadth-first
;; ;;; order.

;; (defparameter *standard-classes*
;;   (list *class-standard-object*
;; 	*class-funcallable-standard-object*
;; 	*class-metaobject*
;; 	*class-method*
;; 	*class-standard-method*
;; 	*class-standard-accessor-method*
;; 	*class-standard-reader-method*
;; 	*class-standard-writer-method*
;; 	*class-method-combination*
;; 	*class-slot-definition*
;; 	*class-direct-slot-definition*
;; 	*class-effective-slot-definition*
;; 	*class-standard-slot-definition*
;; 	*class-standard-direct-slot-definition*
;; 	*class-standard-effective-slot-definition*
;; 	*class-specializer*
;; 	*class-eql-specializer*
;; 	*class-class*
;; 	*class-built-in-class*
;; 	*class-forward-reference-class*
;; 	*class-standard-class*
;; 	*class-funcallable-standard-class*))

;;; Make a list of all funcallable-standard-classes in top-down
;;; breadth-first order.

;; (defparameter *funcallable-standard-classes*
;;   (list *class-generic-function* *class-standard-generic-function*))

;;; Now that every class metaobject is allocated, we can fill in the
;;; metaclass of each class metaobject. 

;; (defmacro set-class (class metaclass)
;;   `(setf (standard-instance class ,class) ,metaclass))

;; (loop for class in *standard-classes*
;;       do (set-class class *class-standard-class*))

;; (loop for class in *funcallable-standard-classes*
;;       do (set-class class *class-funcallable-standard-class*))

;; (set-class *class-t* *class-built-in-class*)

;;; Before a class metaobject can be used to create instance, it has
;;; to have a list of effective slot definitions, but those are
;;; themeselves instances that must be created, and so we have a
;;; circular dependency.  We fix this problem by starting to allocate
;;; effective slot definitions "manually", i.e. without the help of
;;; MAKE-INSTANCE and the machinery that comes with it. 

;;; We cheat by creating a list of descriptions (name and one initarg)
;;; of the effective slots that we know we would en up with if we had
;;; used the normal machinery to create the class. 

;; (defparameter *descriptions-of-slots-of-effective-slot-definition*
;;   '(;; Inherited from standard-object.  This information is what
;;     ;; allows us to update instances when a class changes. 
;;     (%version-information :version-information)
;;     (%name :name)
;;     (%allocation :allocation)
;;     (%type :type)
;;     (%location :location)
;;     (%initargs :initargs)
;;     (%initform :initform)
;;     (%initfunction :initfunction)
;;     (%readers :readers)
;;     (%writers :writers)))

;; (defun make-effective-slot-definition (&rest initargs
;; 				       &key &allow-other-keys)
;;   (let* ((slot-descriptions *descriptions-of-slots-of-effective-slot-definition*)
;; 	 (slot-storage (make-array (length slot-descriptions))))
;;     (flet ((set-slot-value (initarg value)
;; 	     (let ((position (position initarg slot-descriptions :key #'cadr)))
;; 	       (setf (aref slot-storage position) value))))
;;       ;; Give default values for :type and :allocation
;;       (set-slot-value :type t)
;;       (set-slot-value :allocation :instance)
;;       (loop for (initarg value) on initargs by #'cddr
;; 	    do (set-slot-value initarg value)))
;;     (allocate-standard-instance *class-effective-slot-definition*
;; 				slot-storage)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions initialize-instance, reinitialize-instance, shared-initialize

;; (defgeneric shared-initialize (instance
;; 			       slot-names
;; 			       &rest initargs
;; 			       &key &allow-other-keys))

;;; The spec requires the method on initialize-instance and
;;; reinitialize-instance specialized for standard-object to call
;;; shared-initialize as shown below.  The slot-name argument is a
;;; list of slot-names to be initialized, or t which means all slots.
;;; A slot is initialized from the initargs if one of its initargs is
;;; in that list.  Otherwise it is initialized by evaluating the
;;; corresponding intitform if and only if it is one of the slots to
;;; be initialized (according to the value of slot-names), and if it
;;; does not already have a value.  In that case, if no initform
;;; exists, the slot is left unbound.

;; (defmethod shared-initialize ((instance standard-object)
;; 			      slot-names
;; 			      &rest initargs)
;;   (loop for slot in (class-slots (class-of instance))
;;         for slot-name = (slot-definition-name slot)
;;         do (multiple-value-bind (key value foundp)
;; 	       ;; Find the first key/value pair in initargs where the
;; 	       ;; key is one of the initargs of the slot. 
;; 	       (get-properties initargs (slot-definition-initargs slot))
;; 	     (declare (ignore key))
;; 	     (if foundp
;; 		 ;; Found an explicit initarg in initargs.  Initialize
;; 		 ;; the slot from its value
;; 		 (setf (slot-value instance slot-name) value)
;; 		 ;; No explicit initarg found.  
;; 		 (when (and (not (slot-boundp instance slot-name))
;; 			    (not (null (slot-definition-initfunction slot)))
;; 			    (or (eq slot-names t)
;; 				(member slot-name slot-names)))
;; 		   ;; Evaluate the initform by executing the initfunction. 
;; 		   (setf (slot-value instance slot-name)
;; 			 (funcall (slot-definition-initfunction slot)))))))
;;   instance)

;; (defgeneric initialize-instance (instance
;; 				 &rest initargs
;; 				 &key &allow-other-keys))

;;; The method on initialize-instance specialized for standard-object
;;; is especially important to bootstrapping, because all important
;;; metaobject classes are subclasses of standard-object.  So if
;;; make-instance is used, directly or indirectly (using ensure-class)
;;; to create a metaobject class, then this method must already be in
;;; place.

;; (defmethod initialize-instance ((instance standard-object)
;; 				&rest initargs
;; 				&key &allow-other-keys)
;;   ;; Call shared-initialize with a slot-list of t, meaning all slots,
;;   ;; i.e., for every slot that is not explicitly initialized and which
;;   ;; is unbound, evaluate its initform if it has one. 
;;   (apply #'shared-initialize instance t initargs))

;; (defgeneric reinitialize-instance (instance
;; 				   &rest initargs
;; 				   &key &allow-other-keys))

;; (defmethod reinitialize-instance ((instance standard-object)
;; 				  &rest initargs
;; 				  &key &allow-other-keys)
;;   ;; Call shared-initialize with a slot-list of (), meaning no slot,
;;   ;; i.e., only assign values to slots that have explicit
;;   ;; initialization arguments in initargs. 
;;   (apply #'shared-initialize instance () initargs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the class precedence list

;;; For a given class, return a list containing the class and all its
;;; direct and indirect superclasses without any duplicate entries,
;;; but in no particular order.  We must be careful here, in that we
;;; might have to handle an incorrect inheritance graph, that might
;;; contain circular dependencies.
;; (defun find-all-superclasses (class)
;;   (let ((result '()))
;;     (labels ((find-all-supers (class)
;; 	       (unless (member class all-supers :test #'eq)
;; 		 (push class all-supers)
;; 		 (loop for super in (class-direct-superclasses class)
;; 		       do (find-all-supers super)))))
;;       (find-all-supers class))
;;     result))

;;; For a given class, compute a precedence relation.  
;;; This relation is represented as a list of cons cells, 
;;; where the class in the CAR of the cell takes precedence over
;;; the class in the CDR of the cell. For a class C with a list of 
;;; direct superclasses (S1 S2 ... Sn), the relation contains 
;;; the elements (C . S1) (S1 . S2) ... (Sn-1 . Sn).
;; (defun compute-relation (class)
;;   (loop for prev = class then super
;;         for super in (class-direct-superclasses class)
;;         collect (cons prev super)))

;; (defgeneric compute-class-prececence-list (class))

;; (defmethod compute-class-prececence-list ((class class))
;;   (let* ((all-supers (find-all-superclasses class))
;; 	 (relation (loop for class in all-supers
;; 		         append (compute-relation class)))
;; 	 (reverse-result '()))
;;     (flet ((find-candidate ()
;; 	     (let ((candidates
;; 		    ;; Start by looking for all remaining classes that
;; 		    ;; depend on no other clas according to the relation.
;; 		    (loop for super in all-supers
;; 			      unless (find super relation :key #'cdr)
;; 			      collect element)))
;; 	       ;; If no such class exists, we have a circular dependence,
;; 	       ;; and so we can't compute the class precedence list.
;; 	       (when (null candidates)
;; 		 ;; FIXME: do this better
;; 		 (error "can't compute class precedence list"))
;; 	       (if (null (cdr candidates))
;; 		   ;; A unique candiate, return it.
;; 		   (car candidates)
;; 		   ;; Several candidates.  Look for the last class in the
;; 		   ;; result computed so far (first in the reversed result)
;; 		   ;; that has one of the candidates as a direct superclass.
;; 		   ;; Return that candidate.  
;; 		   ;; There can be at most one, because within a list of
;; 		   ;; superclasses, there is already a dependency, so that
;; 		   ;; two different classes in a single list of superclasses
;; 		   ;; can not both be candidates.
;; 		   ;; There must be at least one, because ... 
;; 		   ;; (FIXME: prove it!)
;; 		   (loop for element in reverse-result
;; 		         do (loop for candidate in candidates
;; 			          do (when (member candidate
;; 						   (class-direct-superclasses
;; 						    element))
;; 				       (return-from find-candidate
;; 					 candidate))))))))
;;       (loop until (null all-supers)
;; 	    do (let ((candidate (find-candidate)))
;; 		 (push candidate reverse-result)
;; 		 (setf all-supers (remove candidate all-supers))
;; 		 (setf relation (remove candidate relation :key #'car)))))
;;     reverse-result))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a class.

;;; For a given class, return the class to be used for
;;; direct slot definitions of that class.
;; (defgeneric direct-slot-definition-class (class &rest initargs))

;; (defmethod direct-slot-definition-class ((class standard-class)
;; 					 &rest initargs)
;;   (declare (ignore initargs))
;;   (return (find-class 'standard-direct-slot-definition)))
  
;; (defmethod direct-slot-definition-class ((class funcallable-standard-class)
;; 					 &rest initargs)
;;   (declare (ignore initargs))
;;   (return (find-class 'standard-direct-slot-definition)))
  
;;; For a given class, return the class to be used for
;;; effective slot definitions of that class.
;; (defgeneric effective-slot-definition-class (class &rest initargs))

;; (defmethod effective-slot-definition-class ((class standard-class)
;; 					    &rest initargs)
;;   (declare (ignore initargs))
;;   (return (find-class 'standard-effective-slot-definition)))
  
;; (defmethod effective-slot-definition-class ((class funcallable-standard-class)
;; 					    &rest initargs)
;;   (declare (ignore initargs))
;;   (return (find-class 'standard-effective-slot-definition)))
  
;; (defgeneric compute-effective-slot-definition
;;     (class name direct-slot-definitions))

;;; Implement the behavior of compute-effective-slot-definition
;;; for standard-class and funcallable-standard-class.
;; (defun compute-effective-slot-definition-aux (class direct-slot-definitions)
;;   (let (allocation initargs initform initfunction type)
;;     (setf allocation
;; 	  (slot-definition-allocation (first direct-slot-definitions)))
;;     (setf initargs
;; 	  (reduce #'union
;; 		  (mapcar #'slot-definition-initargs direct-slot-definitions)))
;;     (let ((first-init (find-if-not #'null direct-slot-definitions
;; 				   :key #'slot-definition-initfunction)))
;;       (unless (null first-init)
;; 	(setf initform (slot-definition-initform first-init)
;; 	      initfunction (slot-definition-initfunction first-init))))
;;     (setf type
;; 	  `(and ,@mapcar #'slot-definition-type direct-slot-definitions))
;;     (if (null initfunction)
;; 	(make-instance class
;; 		       :allocation allocation
;; 		       :initargs initargs
;; 		       :type type)
;; 	(make-instance class
;; 		       :allocation allocation
;; 		       :initargs initargs
;; 		       :initform initform
;; 		       :initfunction initfunction
;; 		       :type type))))

;; (defmethod compute-effective-slot-definition ((class standard-class)
;; 					      direct-slot-definitions)
;;   (compute-effective-slot-definition-aux
;;    (effective-slot-definition-class class)
;;    direct-slot-definitions))

;; (defmethod compute-effective-slot-definition ((class funcallable-standard-class)
;; 					      direct-slot-definitions)
;;   (compute-effective-slot-definition-aux
;;    (effective-slot-definition-class class)
;;    direct-slot-definitions))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions ensure-class-using-class and ensure-class.  

;; (defgeneric ensure-class-using-class (class name
;; 				      &key
;; 				      direct-default-initargs
;; 				      direct-slots
;; 				      direct-superclasses
;; 				      ;; name ???
;; 				      metaclass
;; 				      &allow-other-keys))

;; (defmethod ensure-class-using-class ((class class) name
;; 				     &key
;; 				     metaclass
;; 				     direct-superclasses
;; 				     &allow-other-keys)
;;   nil)

;; (defmethod ensure-class-using-class ((class forward-referenced-class) name
;; 				     &key
;; 				     metaclass
;; 				     direct-superclasses
;; 				     &allow-other-keys)
;;   nil)

;; (defmethod ensure-class-using-class ((class null) name
;; 				     &key
;; 				     metaclass
;; 				     direct-superclasses
;; 				     &allow-other-keys)
;;   nil)
