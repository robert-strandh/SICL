(in-package #:sicl-clos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions find-class and (setf find-class)
;;;
;;; These functions manage the database of classes. 
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

;;; This function is only used in the bootstrap code to 
;;; find the name of a class metaobject
(defun find-class-reverse (class)
  (maphash (lambda (name class-metaobject)
	     (when (eq class-metaobject class)
	       (return-from find-class-reverse name)))
	   *classes*)
  (error "no such class"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generating temporary accessors for our metaobjects. 
;;;
;;; We take our slot descriptors and generate really stupid and slow
;;; (but very safe) versions of all the readers and accessors that
;;; were mentioned in those slot descriptions.  Each reader has the
;;; following form:
;;;
;;;    (defun mumble (x)
;;;      (let ((class (standard-instance-class x))
;;;            (slots (standard-instance-slots x)))
;;;        (cond ((eq class (find-class 'class-1 nil))
;;;               (slotcontents slots s-1))
;;;              ((eq class (find-class 'class-2 nil))
;;;               (slotcontents slots s-2))
;;;              ...
;;;              (t (error ...)))))
;;;
;;; where s-i is the index in class-i of the slot that is read.
;;;
;;; Writers have the analogous structure. 
;;;
;;; This means that all our accessors are operational even though
;;; they are not generic functions of course. 
(eval-when (:compile-toplevel :load-toplevel)

  (defun collect-reader (reader)
    (let ((result '()))
      (maphash (lambda (class-name slot-descriptors)
		 (loop for s in slot-descriptors
		       for i from 0
		       do (when (or (eq (cadr (member :reader s)) reader)
				    (eq (cadr (member :accessor s)) reader))
			    (push `(,class-name ,i) result))))
	       *effective-slots*)
      result))
  
  (defun collect-writer (writer)
    (let ((result '()))
      (maphash (lambda (class-name slot-descriptors)
		 (loop for s in slot-descriptors
		       for i from 0
		       do (when (eq (cadr (member :accessor s)) writer)
			    (push `(,class-name ,i) result))))
	       *effective-slots*)
      result))
  
  (let ((readers '())
	(writers '()))
    ;; Find all :reader and :accessor slot options
    (maphash (lambda (class-name slot-descriptors)
	       (declare (ignore class-name))
	       (loop for s in slot-descriptors
		     do (let (temp)
			  (setf temp (member :reader s))
			  (when temp
			    (pushnew (cadr temp) readers))
			  (setf temp (member :accessor s))
			  (when temp
			    (pushnew (cadr temp) readers)
			    (pushnew (cadr temp) writers)))))
	     *effective-slots*)
    ;; For each one, create a function
    (flet ((make-reader-function-form (name readers)
	     `(defun ,name (x)
		(let ((class (standard-instance-class x))
		      (slots (standard-instance-slots x)))
		  (cond 
		    ,@(loop for reader in readers
			    collect
			    `((eq class (find-class ',(car reader) nil))
			      (slot-contents slots ,(cadr reader))))
		    (t (error "no reader on ~s" ',name))))))
	   (make-writer-function-form (name writers)
	     `(defun (setf ,name) (new-value x)
		(let ((class (standard-instance-class x))
		      (slots (standard-instance-slots x)))
		  (cond 
		    ,@(loop for writer in writers
			    collect
			    `((eq class (find-class ',(car writer) nil))
			      (setf (slot-contents slots ,(cadr writer))
				    new-value)))
		    (t (error "no writer on ~s" ',name)))))))
      (loop for reader in readers
	    do (eval (make-reader-function-form
		      reader (collect-reader reader))))
      (loop for writer in writers
	    do (eval (make-writer-function-form
		      writer (collect-writer writer))))))

  ) ; eval-when

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The contents of this file should be read sequentially, because the
;;; bootstrapping issues make it necessary to create functionality
;;; step by step.  FIXME: elaborate...

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
;;; We need a way to bootstrap the entire machinery because there are
;;; numerous circular dependencies.  We break some of these cycles by
;;; providing a set of functions that work directly on "slot
;;; descriptors", by which we mean the raw version of the slot
;;; information of a class as provided to the DEFCLASS macro.  We take
;;; that information, together with information about superclasses,
;;; and compute "effective slot descriptor" for each class metaobject.
;;; Of course, we have to make sure that this information is computed
;;; in a way consistent with what will eventually be done by the real
;;; machinery.  Then, we can use this information to allocate
;;; instances (because we know the length of the list of slot
;;; descriptors) and initialize instance (we have information about
;;; initargs and initforms). 
;;;
;;; The functions sd-initialize-instance and sd-make-instance are part
;;; of this bootstrapping machinery.  Using these, we can create
;;; instances of any metaobject class that can be found using
;;; FIND-CLASS.  This restriction is minor, because all that has to
;;; exist is a standard-instance representing the class metaobject
;;; associated with a name.  We can fill in the details later, as we
;;; do for STANDARD-CLASS.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Initialize an instance using slot-descriptions.

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
(defun sd-initialize-instance-after-standard-class
    (instance &key name direct-superclasses direct-slots &allow-other-keys)
  (setf (class-name instance) name)
  (let ((superclasses (or direct-superclasses
			  (list (find-class 'standard-object)))))
    (setf (class-direct-superclasses instance) superclasses))
  (let ((slots (loop for slot-properties in direct-slots
		     collect (apply #'make-direct-slot-definition
				    slot-properties))))
    (setf (class-direct-slots instance) slots)))

(defun sd-initialize-instance (instance slot-descriptions &rest initargs)
  (let ((slots (standard-instance-slots instance)))
    (loop for (initarg value) on initargs by #'cddr
	  do (loop for slot-description in slot-descriptions
		   for i from 0
		   do (when (and
			     ;; The slot has an initarg.
			     (member :initarg slot-description)
			     ;; It is the right one.
			     (eq (cadr (member :initarg slot-description))
				 initarg)
			     ;; The slot is still unbound.
			     (eq (slot-contents slots i) *secret-unbound-value*))
			;; Initialize the slot from the initarg provided.
			(setf (slot-contents slots i) value))))
    ;; Initialize from initforms if any
    (loop for slot-description in slot-descriptions
	  for i from 0
	  do (when (and
		    ;; The slot has an initform.
		    (member :initform slot-description)
		    ;; The slot is still unbound.
		    (eq (slot-contents slots i) *secret-unbound-value*))
	       ;; Initialize the slot from the initform.
	       (setf (slot-contents slots i)
		     (eval (cadr (member :initform slot-description)))))))
  ;; Call after methods
  (when (eq (standard-instance-class instance)
	    (find-class 'standard-class))
    (apply #'sd-initialize-instance-after-standard-class
	   instance initargs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Make an instance using slot-descriptions

(defun sd-make-instance (class slot-descriptions &rest initargs)
  (let* ((slot-storage (allocate-slot-storage (length slot-descriptions)
					      *secret-unbound-value*))
	 (instance (allocate-standard-instance class slot-storage)))
    (apply #'sd-initialize-instance instance slot-descriptions initargs)
    instance))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessing slots using the slot definitions above.

(defun slot-value-using-slots (instance slot-name slots)
  (let ((pos (position slot-name slots :key #'car)))
    (slot-contents (standard-instance-slots instance) pos)))

(defun (setf slot-value-using-slots) (new-value instance slot-name slots)
  (let ((pos (position slot-name slots :key #'car)))
    (setf (slot-contents (standard-instance-slots instance) pos) new-value)))

(defun (setf slot-value-using-slot-initarg) (new-value instance initarg slots)
  (let ((pos (position initarg slots
		       :key (lambda (slot) (cadr (member :initarg slot))))))
    (unless (null pos)
      (setf (slot-contents (standard-instance-slots instance) pos) new-value))))

;;; For each unbound slot, evaluate its initform to obtain a value to
;;; initialize it with.  We could probably avois using EVAL here
;;; because, all of the initforms are constant forms anyway, but we do
;;; not want to take the risk.
(defun initialize-unbound-slots (instance slots)
  (loop for slot in slots
	do (when (eq (slot-value-using-slots instance (car slot) slots)
		     *secret-unbound-value*)
	     (setf (slot-value-using-slots instance (car slot) slots)
		   (eval (cadr (member :initform slot)))))))

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
    
;;; REMEMBER: Patch standard-direct-slot-definition. +

(defun make-direct-slot-definition (&rest initargs
				    &key &allow-other-keys)
  (apply #'make-instance
	 'standard-direct-slot-definition
	 initargs))

;;; REMEMBER: Patch version information of slot-definition-instances. +

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class T, so that we can allocate the class STANDARD-INSTANCE.

(setf (find-class 't) (allocate-standard-instance nil nil))

;;; REMEMBER: Patch class T. +

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class standard-class.

(defparameter *class-standard-class*
  (let ((class (allocate-standard-instance nil nil)))
    ;; STANDARD-CLASS is an instance of itself. 
    (setf (standard-instance-class class) class)
    (setf (find-class 'standard-class) class)
    class))
    
;;; REMEMBER: Patch class STANDARD-CLASS. +

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Function make-instance.  
;;;
;;; For now, we only need MAKE-INSTANCE to make instances of
;;; STANDARD-CLASS, which is fortunate because we don't have the
;;; mechanisms in place to implement MAKE-INSTANCE correctly.  So we
;;; temporarily define it as an ordinary function, and check that it
;;; is only used to define instances of STANDARD-CLASS.

(defun make-instance (class
		      &rest
			initargs
		      &key
		      &allow-other-keys)
  (let ((class-name (if (symbolp class) class (find-class-reverse class)))
	(class-metaobject (if (symbolp class) (find-class class) class)))
    (apply #'sd-make-instance
	   class-metaobject
	   (find-effective-slots class-name)
	   initargs)))

;;; REMEMBER: Redefine make-instance as a generic function. +

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Functions ensure-class-using-class and ensure-class.  

(defun subclass-of-class-p (class)
  (and (not (eq class (find-class 't nil)))
       (or (eq class *class-standard-class*)
	   (let ((class-class (find-class 'class)))
	     (or (eq class class-class)
		 (some #'subclass-of-class-p
		       (class-direct-superclasses class)))))))

(defun classp (object)
  (and (standard-instance-p object)
       (subclass-of-class-p (standard-instance-class object))))

;;; Since we don't have generic functions yet, we define
;;; ensure-class-using-class as an ordinary function.  Furthermore, we
;;; only implement its functionality for the case when the class is
;;; NIL.

(defun process-direct-superclasses (direct-superclasses)
  (unless (proper-list-p direct-superclasses)
    (error "list of superclasses must be a proper list"))
  (loop for class-or-name in direct-superclasses
	collect (cond  ((classp class-or-name)
			class-or-name)
		       ((symbolp class-or-name)
			(let ((class (find-class class-or-name nil)))
			  (if (null class)
			      (setf (find-class class-or-name)
				    (make-instance 'forward-reference-class
						   :name class-or-name))
			      class)))
		       (t
			(error "~s must be a class or a class name"
			       class-or-name)))))

;;; This is the final behavior of the method on
;;; ENSURE-CLASS-USING-CLASS specialized to NULL.  Therefore, that
;;; method, once it exists, should call this function.
(defun ensure-class-using-class-null (name
				      &rest
					keys
				      &key
					direct-superclasses
					(metaclass 'standard-class)
				      &allow-other-keys)
  (setf metaclass
	(let ((class-metaobject (if (symbolp metaclass)
				    (find-class metaclass)
				    metaclass)))
	  (unless (and (classp class-metaobject)
		       (subclass-of-class-p class-metaobject))
	    (error "metaclass must be a class metaobject class"))
	  class-metaobject))
  (setf direct-superclasses
	(process-direct-superclasses direct-superclasses))
  (let ((remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (setf (find-class name)
	  (apply #'make-instance metaclass
		 :name name
		 :direct-superclasses direct-superclasses
		 remaining-keys))))

;;; REMEMBER: Call ensure-class-using-class-null from method of e-c-u-c. +

(defun ensure-class-using-class (class name &rest keys)
  (assert (null class))
  (apply #'ensure-class-using-class-null name keys))

;;; REMEMBER: Redefine ensure-class-using-class as a generic-function. +

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

;;; This is the final definition of ensure-class.
(defun ensure-class (name &rest arguments &key &allow-other-keys)
  (unless (and (symbolp name) (not (null name)))
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

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun canonicalize-direct-superclass-name (class-name)
    (unless (and (symbolp class-name)
		 (not (null class-name)))
      (error 'class-name-must-be-non-nil-symbol
	     :name 'defclass
	     :datum class-name))
    `',class-name)

  (defun canonicalize-direct-superclass-names (direct-superclass-names)
    (unless (proper-list-p direct-superclass-names)
      (error 'superclass-list-must-be-proper-list
	     :name 'defclass
	     :datum direct-superclass-names))
    `(list ,@(loop for name in direct-superclass-names
		   collect (canonicalize-direct-superclass-name name))))

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
		    (add :initform `',(car value))
		    (add :initfunction `(lambda () ,(car value)))
		    (remhash :initform ht)))
		;; Process :initarg option.
		(multiple-value-bind (value flag)
		    (gethash :initarg ht)
		  (when flag
		    (add :initargs `',(reverse value))
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
		    (add :readers `',(reverse value))
		    (remhash :reader ht)))
		;; Process :writer option.
		(multiple-value-bind (value flag)
		    (gethash :writer ht)
		  (when flag
		    (add :writers `',(reverse value))
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
	      `(list ,@result))))))

  (defun canonicalize-direct-slot-specs (direct-slot-specs)
    (when (not (proper-list-p direct-slot-specs))
      (error 'malformed-slots-list
	     :name 'defclass
	     :datum direct-slot-specs))
    `(list ,@(loop for spec in direct-slot-specs
		   collect (canonicalize-direct-slot-spec spec))))

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
			(append result `(:metaclass ',(cadr option)))))
		 (t 
		  (setf result
			(append result `(,(car option) ,(cdr option)))))))
      result))

  ) ; eval-when

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
;;; Defining standard classes.
;;; 
;;; We now have everything needed to use DEFCLASS to create instances
;;; of STANDARD-CLASS.

(defclass standard-object (t)
  #.(find-direct-slots 'standard-object))

(defclass metaobject (standard-object)
  #.(find-direct-slots 'metaobject))

(defclass method (metaobject)
  #.(find-direct-slots 'method))

(defclass standard-method (method)
  #.(find-direct-slots 'standard-method))

(defclass standard-accessor-method (standard-method)
  #.(find-direct-slots 'standard-accessor-method))

(defclass standard-reader-method (standard-accessor-method)
  #.(find-direct-slots 'standard-reader-method))

(defclass standard-writer-method (standard-accessor-method)
  #.(find-direct-slots 'standard-writer-method))

(defclass method-combination (metaobject)
  #.(find-direct-slots 'method-combination))

(defclass slot-definition (metaobject)
  #.(find-direct-slots 'slot-definition))

(defclass direct-slot-definition (slot-definition)
  #.(find-direct-slots 'direct-slot-definition))

(defclass effective-slot-definition (slot-definition)
  #.(find-direct-slots 'effective-slot-definition))

(defclass standard-slot-definition (slot-definition)
  #.(find-direct-slots 'standard-slot-definition))

;;; We can not use DEFCLASS to create the class named
;;; STANDARD-DIRECT-SLOT-DEFINITION, because we already created it in
;;; order to make it posible to create instances of it early on.  But
;;; when we created it, we did so without filling in neither its class
;;; nor its slots, so it is just a bare standard-instance at the
;;; moment.
;;;
;;; We fix the situatio by using DEFCLASS to create a fake class that
;;; looks the same, but with a different name and then we transfer the
;;; class and slots from the fake class to the bare standard-instance.

(defclass fake (standard-slot-definition direct-slot-definition)
  #.(find-direct-slots 'standard-direct-slot-definition))

(let ((to-patch (find-class 'standard-direct-slot-definition))
      (fake (find-class 'fake)))
  (setf (standard-instance-class to-patch)
	(standard-instance-class fake))
  (setf (standard-instance-slots to-patch)
	(standard-instance-slots fake))
  (setf (class-name fake) 'standard-direct-slot-definition)
  (setf (find-class 'fake) nil))

;;; REMEMBER: Patch standard-direct-slot-definition. -

(defclass standard-effective-slot-definition
    (standard-slot-definition effective-slot-definition)
  #.(find-direct-slots 'standard-effective-slot-definition))

(defclass specializer (metaobject)
  #.(find-direct-slots 'specializer))

(defclass eql-specializer (specializer)
  #.(find-direct-slots 'eql-specializer))

(defclass class (specializer)
  #.(find-direct-slots 'class))

(defclass built-in-class (class)
  #.(find-direct-slots 'built-in-class))

(defclass forward-reference-class (class)
  #.(find-direct-slots 'forward-reference-class))

;;; We can not use DEFCLASS to create the class named STANDARD-CLASS,
;;; because we already created it in order to make it posible to
;;; create instances of it early on.  But when we created it, we did
;;; so without filling in its slots (we did fill in its class,
;;; though).  Now we need to patch the slots of it.
;;;
;;; We fix the situatio by using DEFCLASS to create a fake class that
;;; looks the same, but with a different name and then we transfer the
;;; slots from the fake class to the bare standard-instance.

(defclass fake (class)
  #.(find-direct-slots 'standard-class))

(let ((to-patch (find-class 'standard-class))
      (fake (find-class 'fake)))
  (setf (standard-instance-slots to-patch)
	(standard-instance-slots fake))
  (setf (class-name fake) 'standard-class)
  (setf (find-class 'fake) nil))

;;; REMEMBER: Patch STANDARD-CLASS. -

(defclass funcallable-standard-class (class)
  #.(find-direct-slots 'funcallable-standard-class))

;; (defclass function (t)
;;   ()
;;   (:metaclass built-in-class))

;; (defclass funcallable-standard-object (standard-object function)
;;   #.(find-direct-slots 'funcallable-standard-object))

;; (defclass generic-function (metaobject funcallable-standard-objet)
;;   (:metaclass funcallable-standard-class))

;; (defclass standard-generic-function (generic-function)
;;   (:metaclass funcallable-standard-class))

;;; Now we add the :after method on initialize-instance for
;;; standard-class.  It just calls the function we defined earlier.
;; (defmethod initialize-instance :after ((class standard-class)
;; 				       &key
;; 				       direct-superclasses
;; 				       direct-slots)
;;   (initialize-instance-after-standard-class
;;    class direct-superclasses direct-slots))


;;; This fact creates a circular dependency in that standard-class
;;; is an instance of standard-class, so it must exist before it is
;;; created.  We break this dependency by bulding the instance of
;;; standard-class "manually", i.e. without the use of defclass. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; DESCRIBE and PRINT

(defun print-object (object stream)
  (cond (;; Temporary solution until we can actually print class T.
	 (eq object (find-class t))
	 (format stream
		 "<SICL-BUILTIN-CLASS SICL-T>"))
	((classp object)
	 (format stream
		 "<SICL-~s SICL-~s>"
		 (class-name (standard-instance-class object))
		 (class-name object)))
	((standard-instance-p object)
	 (format stream
		 "<SICL-~s>"
		 (class-name (standard-instance-class object))))
	(t
	 (cl:print-object object stream))))

(cl:defmethod cl:print-object ((object standard-instance) stream)
  (print-object object stream))

(defun describe-object (object stream)
  (cond ((eq (standard-instance-class object)
	     (find-class 'standard-class))
	 (format stream
		 "It is the standard class named ~s"
		 (class-name object)))
	((eq (standard-instance-class object)
	     *class-standard-direct-slot-definition*)
	 (format stream
		 "It is a standard-direct-slot-definition~@
                  of a slot named ~s.~@
                  This slot has allocation ~s,~@
                  type ~s,~@
                  initargs ~s,~@
                  initform ~s,~@
                  readers ~s,~@
                  writers ~s.~%"
		 (slot-definition-name object)
		 (slot-definition-allocation object)
		 (slot-definition-type object)
		 (slot-definition-initargs object)
		 (slot-definition-initform object)
		 (slot-definition-readers object)
		 (slot-definition-writers object)))
	(t
	 (format stream "No idea"))))
	 

(cl:defmethod cl:describe-object ((object standard-instance) stream)
  (describe-object object stream))

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
(defun find-all-superclasses (class)
  (let ((result '()))
    (labels ((find-all-supers (class)
	       (unless (member class result :test #'eq)
		 (push class result)
		 (loop for super in (class-direct-superclasses class)
		       do (find-all-supers super)))))
      (find-all-supers class))
    result))

;;; For a given class, compute a precedence relation.  
;;; This relation is represented as a list of cons cells, 
;;; where the class in the CAR of the cell takes precedence over
;;; the class in the CDR of the cell. For a class C with a list of 
;;; direct superclasses (S1 S2 ... Sn), the relation contains 
;;; the elements (C . S1) (S1 . S2) ... (Sn-1 . Sn).
(defun compute-relation (class)
  (loop for prev = class then super
        for super in (class-direct-superclasses class)
        collect (cons prev super)))

(defun compute-class-precedence-list-class (class)
  (let* ((all-supers (find-all-superclasses class))
	 (relation (loop for class in all-supers
		         append (compute-relation class)))
	 (reverse-result '()))
    (flet ((find-candidate ()
	     (let ((candidates
		     ;; Start by looking for all remaining classes that
		     ;; depend on no other class according to the relation.
		     (loop for super in all-supers
			   unless (find super relation :key #'cdr)
			     collect super)))
	       ;; If no such class exists, we have a circular dependence,
	       ;; and so we can't compute the class precedence list.
	       (when (null candidates)
		 ;; FIXME: do this better
		 (error "can't compute class precedence list"))
	       (if (null (cdr candidates))
		   ;; A unique candiate, return it.
		   (car candidates)
		   ;; Several candidates.  Look for the last class in the
		   ;; result computed so far (first in the reversed result)
		   ;; that has one of the candidates as a direct superclass.
		   ;; Return that candidate.  
		   ;; There can be at most one, because within a list of
		   ;; superclasses, there is already a dependency, so that
		   ;; two different classes in a single list of superclasses
		   ;; can not both be candidates.
		   ;; There must be at least one, because ... 
		   ;; (FIXME: prove it!)
		   (loop for element in reverse-result
		         do (loop for candidate in candidates
			          do (when (member candidate
						   (class-direct-superclasses
						    element))
				       (return-from find-candidate
					 candidate))))))))
      (loop until (null all-supers)
	    do (let ((candidate (find-candidate)))
		 (push candidate reverse-result)
		 (setf all-supers (remove candidate all-supers))
		 (setf relation (remove candidate relation :key #'car)))))
    (reverse reverse-result)))
  
;; (defgeneric compute-class-prececence-list (class))

;; (defmethod compute-class-prececence-list ((class class))
;;   (compute-class-precedence-list-class class))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Computing the effective slots of a class.

;;; For a given class, return the class to be used for
;;; direct slot definitions of that class.

(defun direct-slot-definition-class (class &rest initargs)
  (declare (ignore class initargs))
  (find-class 'standard-direct-slot-definition))

;;; REMEMBER: turn DIRECT-SLOT-DEFINITION-CLASS into a generic function. +

;; (defgeneric direct-slot-definition-class (class &rest initargs))

;; (defmethod direct-slot-definition-class ((class standard-class)
;; 					 &rest initargs)
;;   (declare (ignore initargs))
;;   (find-class 'standard-direct-slot-definition))
  
;; (defmethod direct-slot-definition-class ((class funcallable-standard-class)
;; 					 &rest initargs)
;;   (declare (ignore initargs))
;;   (find-class 'standard-direct-slot-definition))
  
;;; For a given class, return the class to be used for
;;; effective slot definitions of that class.

(defun effective-slot-definition-class (class &rest initargs)
  (declare (ignore class initargs))
  (find-class 'standard-effective-slot-definition))

;;; REMEMBER: turn EFFECTIVE-SLOT-DEFINITION-CLASS into a generic function. +

;; (defgeneric effective-slot-definition-class (class &rest initargs))

;; (defmethod effective-slot-definition-class ((class standard-class)
;; 					    &rest initargs)
;;   (declare (ignore initargs))
;;   (find-class 'standard-effective-slot-definition))
  
;; (defmethod effective-slot-definition-class ((class funcallable-standard-class)
;; 					    &rest initargs)
;;   (declare (ignore initargs))
;;   (find-class 'standard-effective-slot-definition))
  
;; (defgeneric compute-effective-slot-definition
;;     (class name direct-slot-definitions))

;;; Implement the behavior of compute-effective-slot-definition
;;; for standard-class and funcallable-standard-class.
(defun compute-effective-slot-definition-aux (class direct-slot-definitions)
  (let (allocation initargs initform initfunction type)
    (setf allocation
	  (slot-definition-allocation (first direct-slot-definitions)))
    (setf initargs
	  (reduce #'union
		  (mapcar #'slot-definition-initargs direct-slot-definitions)))
    (let ((first-init (find-if-not #'null direct-slot-definitions
				   :key #'slot-definition-initfunction)))
      (unless (null first-init)
	(setf initform (slot-definition-initform first-init)
	      initfunction (slot-definition-initfunction first-init))))
    (setf type
	  `(and ,@(mapcar #'slot-definition-type direct-slot-definitions)))
    (if (null initfunction)
	(make-instance class
		       :allocation allocation
		       :initargs initargs
		       :type type)
	(make-instance class
		       :allocation allocation
		       :initargs initargs
		       :initform initform
		       :initfunction initfunction
		       :type type))))

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
