(in-package #:sicl-clos-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; We need a function to determine whether some object is a class.
;;; Since classes can be unnamed, we cannot consult our database of
;;; classes.  Instead, we decide that something is a class if it is 
;;; a subclass of the class CLASS.

(defgeneric classp (object))

(defmethod classp (object)
  nil)

(defmethod classp ((object class))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; ENSURE-CLASS and ENSURE-CLASS-USING-CLASS.

;;; ENSURE-CLASS is called from code resulting from the macroexpansion
;;; of a macro call to DEFCLASS, but it can also be called directly
;;; from user code.  This means that we have to be careful about
;;; default values of initialization arguments.
;;;
;;; When DEFCLASS is used, it is intened to mean that the class be
;;; defined according to the specification in DEFCLASS, with omitted
;;; options given proper defaults.  If the class already exists, and
;;; DEFCLASS is called, the intended meaning is for the class to be
;;; completely redefined according to the new specification, again
;;; with omitted options given proper defaults.  The intended meaning
;;; is NOT to redefine only the parts of the DEFCLASS specification
;;; that were explicitly given.
;;;
;;; But when ENSURE-CLASS is used directly from user code, omitting
;;; some initialization arguments may very well mean that if the class
;;; already exists, we should use the previous values.
;;;
;;; Therefore, clearly, it is safe for DEFCLASS to pass a complete set
;;; of initialization arguments to ENSURE-CLASS, but it is safe for
;;; DEFCLASS to omit some arguments to ENSURE-CLASS only if
;;; ENSURE-CLASS defaults them to the same values that DEFCLASS
;;; requires.

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

(defgeneric ensure-class-using-class
    (class
     name
     &rest keys
     &key
       direct-default-initargs
       direct-slots
       direct-superclasses
       metaclass
       &allow-other-keys))

;;; When the class is created, it is safe to use a default value of
;;; the empty list for the :DIRECT-SUPERCLASSES initialization
;;; argument, because the AMOP says that the default is the same
;;; whether this argument is the empty list, or not given at all. 

(defmethod ensure-class-using-class
    ((class null)
     name
     &rest keys
     &key
       direct-superclasses
       (metaclass nil metaclass-p)
       &allow-other-keys)
  (setf metaclass
	(let ((class-metaobject
		(if metaclass-p
		    (if (symbolp metaclass)
			(find-class metaclass)
			metaclass)
		    (find-class 'standard-class))))
	  (unless (classp class-metaobject)
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

;;; When the class is reinitialized, it is NOT safe to use a default
;;; value of the empty list for the :DIRECT-SUPERCLASSES
;;; initialization argument.  Instead, we must distinguish the case
;;; where the :DIRECT-SUPERCLASSES initialization argument is not
;;; given, which is interpreted to mean that we keep the old value of
;;; this slot, and the case where the :DIRECT-SUPERCLASSES
;;; initialization argument is given as the empty list, which has
;;; the same meaning as for initialization. 

(defmethod ensure-class-using-class
    ((class class)
     name
     &rest keys
     &key
       (direct-superclasses nil direct-superclasses-p)
       (metaclass nil metaclass-p)
       &allow-other-keys)
  (when metaclass-p
    (cond ((symbolp metaclass)
	   (setf metaclass (find-class metaclass)))
	  ((classp metaclass)
	   nil)
	  (t
	   (error "metaclass must be a symbol or a class metaobject class")))
    (unless (eq metaclass (class-of class))
      (error "can't change metaclass during reinitialization of class")))
  (when direct-superclasses-p
    (setf direct-superclasses
	  (process-direct-superclasses direct-superclasses)))
  (let ((remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (if direct-superclasses-p
	(apply #'reinitialize-instance
	       :name name
	       :direct-superclasses direct-superclasses
	       remaining-keys)
	(apply #'reinitialize-instance
	       :name name
	       remaining-keys)))
  class)

(defmethod ensure-class-using-class
    ((class forward-reference-class)
     name
     &rest keys
     &key
       (direct-superclasses nil direct-superclasses-p)
       (metaclass nil metaclass-p)
       &allow-other-keys)
  (unless metaclass-p
    (error "metaclass must be given when ensuring a forward-reference class"))
  (cond ((symbolp metaclass)
	 (setf metaclass (find-class metaclass)))
	((classp metaclass)
	 nil)
	(t
	 (error "metaclass must be a symbol or a class metaobject class")))
  (change-class class metaclass)
  (when direct-superclasses-p
    (setf direct-superclasses
	  (process-direct-superclasses direct-superclasses)))
  (let ((remaining-keys (copy-list keys)))
    (loop while (remf remaining-keys :metaclass))
    (loop while (remf remaining-keys :direct-superclasses))
    (if direct-superclasses-p
	(apply #'reinitialize-instance class
	       :name name
	       :direct-superclasses direct-superclasses
	       remaining-keys)
	(apply #'reinitialize-instance class
	       :name name
	       remaining-keys)))
  class)

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

;;; The CLHS requires that the DIRECT-SUPERCLASSES argument to
;;; DEFCLASS be a proper list of non-NIL symbols.

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
		    (error 'malformed-default-initargs
			   :datum option))
		  (unless (evenp (length (cdr option)))
		    (error 'malformed-default-initargs
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
			  (append result `(:direct-default-initargs
					   ,canonicalized-initargs)))))
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
;;; the AMOP talks about additional class options, but no such
;;; additional options are permitted according to the CLHS.  We follow
;;; the CLHS.

;;; The AMOP says that the NAME argument to DEFCLASS becomes the first
;;; argument to ENSURE-CLASS.  Nothing particular here.
;;;
;;; The AMOP says that the DIRECT-SUPERCLASSES argument to DEFCLASS
;;; becomes the value of the :DIRECT-SUPERCLASSES argument to
;;; ENSURE-CLASS.  The CLHS requires that the DIRECT-SUPERCLASSES
;;; argument to DEFCLASS be a proper list of non-NIL symbols.


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
