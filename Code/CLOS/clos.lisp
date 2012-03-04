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
;;; SLOT-MISSING.

;;; We do not have generic functions yet ....

;;; Default method for SLOT-MISSING (specializer is class T). 
(defun s-m-primary-t (object slot-name operation)
  ;; FIXME: signal a more specific condition
  (error "The slot ~s is missing in the object ~s when ~s was attempted"
	 slot-name object operation))

(defun slot-missing (object slot-name operation)
  (s-m-primary-t object slot-name operation))

;;; REMEMBER: Redefine SLOT-MISSING as a generic function. +

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE-USING-CLASS and (SETF SLOT-VALUE-USING-CLASS).
;;;
;;; These functions are generic functions, but we don't have generic
;;; functions yet.  FIXME: say more...

(defun slot-value-using-class (class object slot)
  (let* ((slot-definitions (class-slots class))
	 ;; Is this safe?
	 (slot-position (position slot slot-definitions))
	 (slot-storage (standard-instance-slots object)))
    (assert (not (null slot-position)))
    (slot-contents slot-storage slot-position)))

(defun (setf slot-value-using-class) (new-value class object slot)
  (let* ((slot-definitions (class-slots class))
	 ;; Is this safe?
	 (slot-position (position slot slot-definitions))
	 (slot-storage (standard-instance-slots object)))
    (assert (not (null slot-position)))
    (setf (slot-contents slot-storage slot-position) new-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; SLOT-VALUE and (SETF SLOT-VALUE).
;;;
;;; The specification says that the third argument of
;;; SLOT-VALUE-USING-CLASS is an EFFECTIVE-SLOT-DEFINITION metaobject,
;;; but the Closette implementation and the rest of the AMOP book
;;; claim that it is a slot name.  We go with the definition of the
;;; specification.

(defun slot-value (object slot-name)
  (if (standard-instance-p object)
      (let* ((class (standard-instance-class object))
	     (name (find-class-reverse class))
	     (slot-descriptors (find-effective-slots name)))
	(cond ((not (null slot-descriptors))
	       (let ((slot-position (position slot-name slot-descriptors :key #'car))
		     (slot-storage (standard-instance-slots object)))
		 (when (null slot-position)
		   (slot-missing object slot-name 'slot-value))
		 (slot-contents slot-storage slot-position)))
	      (t
	       (let* ((slot-definitions (class-slots class))
		      (slot-definition (find slot-name slot-definitions
					     :key #'slot-definition-name)))
		 (when (null slot-definition)
		   (slot-missing object slot-name 'slot-value))
		 (slot-value-using-class class object slot-definition)))))
      (slot-missing object slot-name 'slot-value)))

(defun (setf slot-value) (new-value object slot-name)
  (if (standard-instance-p object)
      (let* ((class (standard-instance-class object))
	     (name (find-class-reverse class))
	     (slot-descriptors (find-effective-slots name)))
	(cond ((not (null slot-descriptors))
	       (let ((slot-position (position slot-name slot-descriptors :key #'car))
		     (slot-storage (standard-instance-slots object)))
		 (when (null slot-position)
		   (slot-missing object slot-name 'slot-value))
		 (setf (slot-contents slot-storage slot-position)
		       new-value)))
	      (t
	       (let* ((slot-definitions (class-slots class))
		      (slot-definition (find slot-name slot-definitions
					     :key #'slot-definition-name)))
		 (when (null slot-definition)
		   (slot-missing object slot-name 'slot-value))
		 (setf (slot-value-using-class class object slot-definition)
		       new-value)))))
      (slot-missing object slot-name 'slot-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for class metaobjects.

(defun class-default-initargs (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-default-initargs" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (slot-value object '%default-initargs))
	  ((eq class (find-class 'forward-reference-class))
	   (error "called CLASS-DEFAULT-INITARGS on a forward-reference-class"))
	  ((eq class (find-class 'built-in-class))
	   '())
	  (t (error "no method for ~s on class-default-initargs"
		    object)))))

(defun (setf class-default-initargs) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-default-initargs)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (setf (slot-value object '%default-initargs) new-value))
	  (t (error "no method for ~s on (setf class-default-initargs)"
		    object)))))

(defun class-direct-default-initargs (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-direct-default-initargs" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (slot-value object '%direct-default-initargs))
	  ((or (eq class (find-class 'forward-reference-class))
	       (eq class (find-class 'built-in-class)))
	   '())
	  (t (error "no method for ~s on class-direct-default-initargs"
		    object)))))

(defun class-direct-slots (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-direct-slots" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (slot-value object '%direct-slots))
	  ((or (eq class (find-class 'forward-reference-class))
	       (eq class (find-class 'built-in-class)))
	   '())
	  (t (error "no method for ~s on class-direct-slots"
		    object)))))

(defun (setf class-direct-slots) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-direct-slots)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (setf (slot-value object '%direct-slots) new-value))
	  (t (error "no method for ~s on (setf class-direct-slots)"
		    object)))))

(defun class-direct-subclasses (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-direct-subclasses" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'forward-reference-class))
	       (eq class (find-class 'built-in-class)))
	   (slot-value object '%direct-subclasses))
	  (t (error "no method for ~s on class-direct-subclasses"
		    object)))))

(defun (setf class-direct-subclasses) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-direct-subclasses)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'forward-reference-class))
	       (eq class (find-class 'built-in-class)))
	   (setf (slot-value object '%direct-subclasses) new-value))
	  (t (error "no method for ~s on (setf class-direct-subclasses)"
		    object)))))

(defun class-direct-superclasses (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-direct-superclasses" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'built-in-class)))
	   (slot-value object '%direct-superclasses))
	  ((eq class (find-class 'forward-reference-class))
	   '())
	  (t (error "no method for ~s on class-direct-superclasses"
		    object)))))

(defun (setf class-direct-superclasses) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-direct-superclasses)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (setf (slot-value object '%direct-superclasses) new-value))
	  (t (error "no method for ~s on (setf class-direct-superclasses)"
		    object)))))

(defun class-finalized-p (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-finalized-p" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (slot-value object '%finalized-p))
	  ((eq class (find-class 'forward-reference-class))
	   nil)
	  ((eq class (find-class 'built-in-class))
	   t)
	  (t (error "no method for ~s on class-finalized-p"
		    object)))))

(defun (setf class-finalized-p) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-finalized-p)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (setf (slot-value object '%finalized-p) new-value))
	  (t (error "no method for ~s on (setf class-finalized-p)"
		    object)))))

(defun class-unique-number (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-unique-number" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'built-in-class)))
	   (slot-value object '%unique-number))
	  (t (error "no method for ~s on class-unique-number"
		    object)))))

(defun (setf class-unique-number) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-unique-number)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'built-in-class)))
	   (setf (slot-value object '%unique-number) new-value))
	  (t (error "no method for ~s on (setf class-unique-number)"
		    object)))))

(defun class-name (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-name" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'forward-reference-class))
	       (eq class (find-class 'built-in-class)))
	   (slot-value object '%name))
	  (t (error "no method for ~s on class-name"
		    object)))))

(defun class-precedence-list (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-precedence-list" object))
  (let ((class (standard-instance-class object)))
    (cond (;; We need to special-case this one because the
	   ;; BUILT-IN-CLASS metaobject needs to be finalized before
	   ;; we can do a slot-value of a builtin-class, and the
	   ;; BUILT-IN-CLASS class is a subclass of T so T needs to be
	   ;; finalized before BUILT-IN-CLASS can be finalized.  This
	   ;; is a vicious cycle, so be break it here.
	   (eq object (find-class 't))
	   (list (find-class 't)))
	  ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class))
	       (eq class (find-class 'built-in-class)))
	   (slot-value object '%precedence-list))
	  ((eq class (find-class 'forward-reference-class))
	   (error "called CLASS-PRECEDENCE-LIST on a forward-reference-class"))
	  (t (error "no method for ~s on class-precedence-list"
		    object)))))

(defun (setf class-precedence-list) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-precedence-list)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (setf (slot-value object '%precedence-list) new-value))
	  (t (error "no method for ~s on (setf class-precedence-list)"
		    object)))))

(defun class-slots (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on class-slots" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (slot-value object '%effective-slots))
	  ((eq class (find-class 'forward-reference-class))
	   (error "called CLASS-SLOTS on a forward-reference-class"))
	  ((eq class (find-class 'built-in-class))
	   '())
	  (t (error "no method for ~s on class-slots"
		    object)))))

(defun (setf class-slots) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf class-slots)" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-class))
	       (eq class (find-class 'funcallable-standard-class)))
	   (setf (slot-value object '%effective-slots) new-value))
	  (t (error "no method for ~s on (setf class-slots)"
		    object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for slot-definition metaobjects.

(defun slot-definition-name (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-name" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-direct-slot-definition))
	       (eq class (find-class 'standard-effective-slot-definition)))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-name"
		  object)))))

(defun slot-definition-allocation (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-allocation" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-direct-slot-definition))
	       (eq class (find-class 'standard-effective-slot-definition)))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-allocation"
		  object)))))

(defun slot-definition-type (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-type" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-direct-slot-definition))
	       (eq class (find-class 'standard-effective-slot-definition)))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-type"
		  object)))))

(defun slot-definition-initargs (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-initargs" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-direct-slot-definition))
	       (eq class (find-class 'standard-effective-slot-definition)))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-initargs"
		  object)))))

(defun slot-definition-initform (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-initform" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-direct-slot-definition))
	       (eq class (find-class 'standard-effective-slot-definition)))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-initform"
		  object)))))

(defun slot-definition-initfunction (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-initfunction" object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-direct-slot-definition))
	       (eq class (find-class 'standard-effective-slot-definition)))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-initfunction"
		  object)))))

(defun slot-definition-readers (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-readers" object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-direct-slot-definition))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-readers"
		  object)))))

(defun slot-definition-writers (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on slot-definition-writers" object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-direct-slot-definition))
	   (slot-value object '%name))
	  (t
	   (error "no method for ~s on slot-definition-writers"
		  object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for generic-function metaobjects.

(defun generic-function-argument-precedence-order (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-argument-precedence-order"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%argument-precedence-order))
	  (t
	   (error
	    "no method for ~s on generic-function-argument-precedence-order"
	    object)))))

(defun generic-function-declarations (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-declarations"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%declarations))
	  (t
	   (error
	    "no method for ~s on generic-function-declarations"
	    object)))))

(defun generic-function-lambda-list (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-lambda-list"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%lambda-list))
	  (t
	   (error
	    "no method for ~s on generic-function-lambda-list"
	    object)))))

(defun generic-function-method-combination (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-method-combination"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%method-combination))
	  (t
	   (error
	    "no method for ~s on generic-function-method-combination"
	    object)))))

(defun generic-function-method-class (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-method-class"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%method-class))
	  (t
	   (error
	    "no method for ~s on generic-function-method-class"
	    object)))))

(defun generic-function-name (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-name"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%name))
	  (t
	   (error
	    "no method for ~s on generic-function-name"
	    object)))))

(defun (setf generic-function-name) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf generic-function-name)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (setf (slot-value object '%name) new-value))
	  (t
	   (error
	    "no method for ~s on (setf generic-function-name)"
	    object)))))

(defun generic-function-discriminating-function (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-discriminating-function"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%discriminating-function))
	  (t
	   (error
	    "no method for ~s on generic-function-discriminating-function"
	    object)))))

(defun (setf generic-function-discriminating-function) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf generic-function-discriminating-function)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (setf (slot-value object '%discriminating-function) new-value))
	  (t
	   (error
	    "no method for ~s on (setf generic-function-discriminating-function)"
	    object)))))

(defun generic-function-methods (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on generic-function-methods"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (slot-value object '%methods))
	  (t
	   (error
	    "no method for ~s on generic-function-methods"
	    object)))))

(defun (setf generic-function-methods) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf generic-function-methods)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((eq class (find-class 'standard-generic-function))
	   (setf (slot-value object '%methods) new-value))
	  (t
	   (error
	    "no method for ~s on (setf generic-function-methods)"
	    object)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accessors for method metaobjects.

(defun method-qualifiers (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf method-qualifiers)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-method))
	       (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (slot-value object '%qualifiers))
	  (t
	   (error
	    "no method for ~s on method-qualifiers"
	    object)))))
	   
(defun method-lambda-list (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf method-lambda-list)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-method))
	       (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (slot-value object '%lambda-list))
	  (t
	   (error
	    "no method for ~s on method-lambda-list"
	    object)))))
	   
(defun method-specializers (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf method-specializers)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-method))
	       (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (slot-value object '%specializers))
	  (t
	   (error
	    "no method for ~s on method-specializers"
	    object)))))
	   
(defun method-function (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf method-function)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-method))
	       (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (slot-value object '%function))
	  (t
	   (error
	    "no method for ~s on method-function"
	    object)))))
	   
(defun method-generic-function (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf method-generic-function)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-method))
	       (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (slot-value object '%generic-function))
	  (t
	   (error
	    "no method for ~s on method-generic-function"
	    object)))))
	   
(defun (setf method-generic-function) (new-value object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf method-generic-function)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-method))
	       (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (setf (slot-value object '%generic-function) new-value))
	  (t
	   (error
	    "no method for ~s on (setf method-generic-function)"
	    object)))))
	   
(defun accessor-method-slot-definition (object)
  (when (not (standard-instance-p object))
    (error "no method for ~s on (setf accessor-method-slot-definition)"
	   object))
  (let ((class (standard-instance-class object)))
    (cond ((or (eq class (find-class 'standard-reader-method))
	       (eq class (find-class 'standard-writer-method)))
	   (slot-value object '%slot-definition))
	  (t
	   (error
	    "no method for ~s on accessor-method-slot-definition"
	    object)))))

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
    (instance &key name direct-superclasses &allow-other-keys)
  (let ((superclasses (cond ((not (null direct-superclasses))
			     direct-superclasses)
			    ;; When we create the class
			    ;; STANDARD-OBJECT, the
			    ;; direct-superclasses argument is the
			    ;; empty list (it will be patched later),
			    ;; but we naturally do not want to attemp
			    ;; to include itself among its own
			    ;; superclasses.
			    ((eq name 'standard-object)
			     '())
			    (t
			     (list (find-class 'standard-object))))))
    (setf (class-direct-superclasses instance) superclasses)))

(defun sd-initialize-instance-after-standard-generic-function
    (instance &key &allow-other-keys)
  (declare (ignore instance))
  nil)

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
  (cond ((eq (standard-instance-class instance)
	     (find-class 'standard-class))
	 (apply #'sd-initialize-instance-after-standard-class
		instance initargs))
	((eq (standard-instance-class instance)
	     (find-class 'standard-generic-function nil))
	 (apply #'sd-initialize-instance-after-standard-generic-function
		instance initargs))
	(t
	 nil)))

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
;;; Allocating direct slot definitions

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
					direct-slots
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
    (loop while (remf remaining-keys :direct-slots))
    (setf (find-class name)
	  (apply #'make-instance metaclass
		 :name name
		 :direct-superclasses direct-superclasses
		 :direct-slots (mapcar (lambda (x)
					 (apply #'make-direct-slot-definition x))
				       direct-slots)
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
;;; Creating built-in classes

(defun make-built-in-class
    (name direct-superclasses precedence-list)
  (setf (find-class name)
	(make-instance
	 (find-class 'built-in-class)
	 :name name
	 :direct-superclasses (mapcar #'find-class direct-superclasses)
	 :precedence-list (mapcar #'find-class precedence-list)))
  ;; Add the class itself first on its precedence list
  (push (find-class name) (slot-value (find-class name) '%precedence-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Defining standard classes.
;;; 
;;; We now have everything needed to use DEFCLASS to create instances
;;; of STANDARD-CLASS.

(defclass standard-object ()
  #.(find-direct-slots 'standard-object))

;;; REMEMBER: Include T in superclasses of STANDARD-OBJECT. +

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
  (setf (slot-value fake '%name)
	'standard-direct-slot-definition)
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

(make-built-in-class 't '() '())

(setf (slot-value (find-class 'standard-object) '%direct-superclasses)
     (list (find-class 't)))

;;; REMEMBER: Include T in superclasses of STANDARD-OBJECT. -

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
  (setf (slot-value fake '%name)
	'standard-class)
  (setf (find-class 'fake) nil))

;;; REMEMBER: Patch STANDARD-CLASS. -

(defclass funcallable-standard-class (class)
  #.(find-direct-slots 'funcallable-standard-class))

(make-built-in-class 'function '(t) '(t))

(defclass funcallable-standard-object (standard-object function)
  #.(find-direct-slots 'funcallable-standard-object))

(defclass generic-function (metaobject funcallable-standard-object)
  #.(find-direct-slots 'generic-function)
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  #.(find-direct-slots 'standard-generic-function)
  (:metaclass funcallable-standard-class))

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
		 "<SICL-BUILT-IN-CLASS SICL-T>"))
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
  ;; Make sure all the direct superclasses are already finalized so
  ;; that we can use their precedence lists.
  (loop for super in (class-direct-superclasses class)
	do (unless (class-finalized-p super)
	    (finalize-inheritance super)))
  (let* ((all-supers (cons class
			   (remove-duplicates
			    (reduce #'append
				    (mapcar #'class-precedence-list
					    (class-direct-superclasses class))))))
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

(defun compute-class-precedence-list (class)
  (compute-class-precedence-list-class class))
  
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
(defun compute-effective-slot-definition-aux (class name direct-slot-definitions)
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
    (let ((slot-definition-class (effective-slot-definition-class class)))
      (if (null initfunction)
	  (make-instance slot-definition-class
			 :name name
			 :allocation allocation
			 :initargs initargs
			 :type type)
	  (make-instance slot-definition-class
			 :name name
			 :allocation allocation
			 :initargs initargs
			 :initform initform
			 :initfunction initfunction
			 :type type)))))

(defun compute-effective-slot-definition (class name direct-slot-definitions)
  (compute-effective-slot-definition-aux class name direct-slot-definitions))

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

(defun compute-slots (class)
  (let* ((superclasses (class-precedence-list class))
	 (direct-slots (mapcar #'class-direct-slots superclasses))
	 (concatenated (reduce #'append direct-slots))
	 (reverse-slots (reverse direct-slots))
	 (reverse-concatenated (reduce #'append reverse-slots))
	 (names (mapcar #'slot-definition-name reverse-concatenated))
	 (unique (remove-duplicates names :from-end t)))
    (loop for name in unique
	  collect (compute-effective-slot-definition
		   class
		   name
		   (remove name concatenated
			   :key #'slot-definition-name
			   :test-not #'eql)))))

(defun finalize-inheritance (class)
  (setf (class-precedence-list class) (compute-class-precedence-list class))
  (setf (class-slots class) (compute-slots class))
  ;; If the class was one of the ones we computed "by hand" in the
  ;; bootstrap stage, check that we computed the same slots in the
  ;; same order this time.
  (let ((class-name (find-class-reverse class)))
    (unless (null class-name)
      (let ((slots (find-effective-slots class-name)))
  	(unless (null slots)
  	  (assert (= (length slots) (length (class-slots class))))
  	  (mapc (lambda (x y)
  		  (unless (eql (car x) (slot-definition-name y))
  		    (error "slot names ~s and ~s do not match"
  			   (car x) (slot-definition-name y))))
  		slots
  		(class-slots class))))))
  (setf (class-finalized-p class) t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Unique class numbers.

(defparameter *next-number* 0)

(defun ensure-class-number (class)
  (or (class-unique-number class)
      (progn
	;; Number the superclasses first.
	(loop for super in (class-direct-superclasses class)
	      do (ensure-class-number super))
	(setf (class-unique-number class)
	      (prog1 *next-number* (incf *next-number*))))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests.

(defun test-structure ()
  ;; Check that the class of everything is what it is supposed to be.
  (flet ((test-class-of (a b)
	   (assert (eq (standard-instance-class (find-class a))
		       (find-class b)))))
    (test-class-of 't
		   'built-in-class)
    (test-class-of 'standard-object
		   'standard-class)
    (test-class-of 'funcallable-standard-object
		   'standard-class)
    (test-class-of 'metaobject
		   'standard-class)
    (test-class-of 'generic-function
		   'funcallable-standard-class)
    (test-class-of 'standard-generic-function
		   'funcallable-standard-class)
    (test-class-of 'method
		   'standard-class)
    (test-class-of 'standard-method
		   'standard-class)
    (test-class-of 'standard-accessor-method
		   'standard-class)
    (test-class-of 'standard-reader-method
		   'standard-class)
    (test-class-of 'standard-writer-method
		   'standard-class)
    (test-class-of 'method-combination
		   'standard-class)
    (test-class-of 'slot-definition
		   'standard-class)
    (test-class-of 'direct-slot-definition
		   'standard-class)
    (test-class-of 'effective-slot-definition
		   'standard-class)
    (test-class-of 'standard-direct-slot-definition
		   'standard-class)
    (test-class-of 'standard-effective-slot-definition
		   'standard-class)
    (test-class-of 'specializer
		   'standard-class)
    (test-class-of 'eql-specializer
		   'standard-class)
    (test-class-of 'class
		   'standard-class)
    (test-class-of 'built-in-class
		   'standard-class)
    (test-class-of 'forward-reference-class
		   'standard-class)
    (test-class-of 'standard-class
		   'standard-class)
    (test-class-of 'funcallable-standard-class
		   'standard-class))
  (flet ((test-superclasses (class supers)
	   (let ((real-supers (class-direct-superclasses (find-class class)))
		 (required-supers (mapcar #'find-class supers)))
	     (assert (equal real-supers required-supers)))))
    (test-superclasses 't
		       '())
    (test-superclasses 'standard-object
		       '(t))
    (test-superclasses 'funcallable-standard-object
		       '(standard-object function))
    (test-superclasses 'metaobject
		       '(standard-object))
    (test-superclasses 'generic-function
		       '(metaobject funcallable-standard-object))
    (test-superclasses 'standard-generic-function
		       '(generic-function))
    (test-superclasses 'method
		       '(metaobject))
    (test-superclasses 'standard-method
		       '(method))
    (test-superclasses 'standard-accessor-method
		       '(standard-method))
    (test-superclasses 'standard-reader-method
		       '(standard-accessor-method))
    (test-superclasses 'standard-writer-method
		       '(standard-accessor-method))
    (test-superclasses 'method-combination
		       '(metaobject))
    (test-superclasses 'slot-definition
		       '(metaobject))
    (test-superclasses 'direct-slot-definition
		       '(slot-definition))
    (test-superclasses 'effective-slot-definition
		       '(slot-definition))
    (test-superclasses 'standard-slot-definition
		       '(slot-definition))
    (test-superclasses 'standard-direct-slot-definition
		       '(standard-slot-definition direct-slot-definition))
    (test-superclasses 'standard-effective-slot-definition
		       '(standard-slot-definition effective-slot-definition))
    (test-superclasses 'specializer
		       '(metaobject))
    (test-superclasses 'eql-specializer
		       '(specializer))
    (test-superclasses 'class
		       '(specializer))
    (test-superclasses 'built-in-class
		       '(class))
    (test-superclasses 'forward-reference-class
		       '(class))
    (test-superclasses 'standard-class
		       '(class))
    (test-superclasses 'funcallable-standard-class
		       '(class))))
