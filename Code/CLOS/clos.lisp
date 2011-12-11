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
  :default-initargs :type 'list)

(define-condition multiple-documentation-options-not-permitted (type-error name-mixin)
  ()
  :default-initargs :type 'list)

(define-condition multiple-allocation-options-not-permitted (type-error name-mixin)
  ()
  :default-initargs :type 'list)

(define-condition multiple-type-options-not-permitted (type-error name-mixin)
  ()
  :default-initargs :type 'list)

(define-condition slot-documentation-option-must-be-string (type-error name-mixin)
  ()
  :default-initargs :type 'string)

(define-condition class-option-must-be-non-empty-list (type-error name-mixin)
  ()
  (:default-initargs :type 'list))

(define-condition class-option-name-must-be-symbol (type-error name-mixin)
  ()
  (:default-initargs :type 'symbol))

;;; FIXME: This doesn't feel like a type-error
(define-condition duplicate-class-option-not-allowed (type-error name-mixin)
  ()
  (:default-initarg :type 'symbol))

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
;;; Functions ensure-class-using-class and ensure-class.  

(defgeneric ensure-class-using-class (class name
				      &key
				      direct-default-initargs
				      direct-slots
				      direct-superclasses
				      ;; name ???
				      metaclass
				      &allow-other-keys))

(defmethod ensure-class-using-class ((class class) name
				     &key
				     metaclass
				     direct-superclasses
				     &allow-other-keys)
  nil)

(defmethod ensure-class-using-class ((class forward-referenced-class) name
				     &key
				     metaclass
				     direct-superclasses
				     &allow-other-keys)
  nil)

(defmethod ensure-class-using-class ((class null) name
				     &key
				     metaclass
				     direct-superclasses
				     &allow-other-keys)
  nil)

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
  (unless (proper-list-p direct-superclasses)
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
		  (remhash :initform ht)))
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
  (when (not (proper-list-p direct-slots))
    (error 'malformed-slots-list
	   :name 'defclass
	   :datum direct-slots))
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

(defmacro defclass (name direct-superclasses direct-slots &rest options)
  `(ensure-class ,name
		 :direct-superclasses 
		 ,(canonicalize-direct-superclasses direct-superclasses)
		 :direct-slots
		 ,(canonicalize-direct-slots direct-slots)
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



