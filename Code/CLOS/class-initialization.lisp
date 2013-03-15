(in-package :sicl-clos)

;;; Implement class initialization according to the section of the
;;; AMOP entitled "Initialization of Class Metaobjects".

;;; For :direct-default-initargs, the AMOP says that it defaults to
;;; the empty list when the class metaobject is being initialized.  We
;;; take that to implicitly imply that when the class metaobject is
;;; reinitialized as opposed to initialized, there is no default for
;;; this initialization argument, so that if it is not given, the old
;;; value is retained.  The AMOP furthermore says that this argument
;;; is a list of canonicalized default initialization arguments.  The
;;; canonicalization is accomplished directly in the DEFCLASS macro
;;; (it has to, because a closure needs to be created there to get the
;;; lexical environment right), so there is no further processing to
;;; be done after that.  It follows that we can obtain the desired
;;; effect by using an :initform in the definition of the class.

;;; For :direct-slots, the situation is more complicated.  the
;;; DEFCLASS macro provides us with the canonicalized
;;; slot-descriptions, but it can not create the
;;; DIRECT-SLOT-DEFINITION metaobjects, because the class to be used
;;; for those metaobjects is determined later.  During initialization
;;; and reinitialization, there is therefore some further processing
;;; that needs to be done.  Again, the AMOP says that it defaults to
;;; the empty list when the class metaobject is being initialized.
;;; Again, we take that to implicitly imply that when the class
;;; metaobject is reinitialized as opposed to initialized, there is no
;;; default for this initialization argument, so that if it is not
;;; given, the old value is retained.

;;; Provide an :after method on INITIALIZE-INSTANCE to be used
;;; when class meta-objects are instantiated.  As the AMOP says,
;;; we have to provide appropriate default values for the superclasses
;;; and we have to link the superclasses to this new class meta-object.
;;; We also have to create slot meta-objects from the slot property
;;; list provided to initialize-instance.

;;; The AMOP is not quite clear when it comes to restrictions on
;;; portable programs with respect to methods on INITIALIZE-INSTANCE
;;; and REINITIALIZE-INSTANCE.  It explicitly allows for portable
;;; programs to define :before and :after methods on
;;; INITIALIZE-INSTANCE, so one might think that the implementation
;;; could not define an :after method on INITIALIZE-INSTANCE because
;;; it could be overridden by a perfectly good portable program.  But
;;; then, when reading the restriction about :around methods (it says
;;; it must be EXTENDING, not OVERRIDING) it seems clear that the
;;; :before, :after, and :around methods that a portable program is
;;; allowed to define specialize for STRICT SUBCLASSES of the standard
;;; classes, so we are safe if we define an :after method in
;;; INITIALIZE-INSTANCE here. 

(defun set-direct-default-initargs (class direct-default-initargs)
  (unless (proper-list-p direct-default-initargs)
    (error "direct default initargs must be a proper list"))
  ;; FIXME: check that the elements of the list are
  ;; canonicalized default initargs.
  (setf (c-direct-default-initargs class)
	direct-default-initargs))

(defun add-as-subclass-to-superclasses (class)
  (loop for superclass in (class-direct-superclasses class)
	do (setf (c-direct-subclasses superclass)
		 (cons class (class-direct-subclasses superclass)))))

(defun create-readers-and-writers (class)
  (loop for direct-slot in (class-direct-slots class)
	for slot-name = (slot-definition-name direct-slot)
	do (loop for reader in (slot-definition-readers direct-slot)
		 do (add-reader-method class reader slot-name))
	   (loop for writer in (slot-definition-writers direct-slot)
		 do (add-writer-method class writer slot-name))))

(defun set-superclasses (class direct-superclasses default-superclass)
  (unless (proper-list-p direct-superclasses)
    (error "direct superclasses must be proper list"))
  (let ((defaulted-direct-superclasses (if (null direct-superclasses)
					   (list default-superclass)
					   direct-superclasses)))
    (loop for direct-superclass in defaulted-direct-superclasses
	  do (unless (validate-superclass class direct-superclass)
	       (error "superclass not valid for class")))
    (setf (c-direct-superclasses class)
	  defaulted-direct-superclasses)))

(defun set-direct-slots (class direct-slots)
  (unless (proper-list-p direct-slots)
    (error "direct slots must be proper list"))
  ;; FIXME: check that the elements are canonicalized slot specifications
  (setf (c-direct-slots class)
	(loop for canonicalized-slot-specification in direct-slots
	      collect (apply #'make-instance
			     (apply #'direct-slot-definition-class
				    class canonicalized-slot-specification)
			     canonicalized-slot-specification))))

(defmethod initialize-instance :after
    ((class standard-class)
     &key
       direct-default-initargs
       direct-superclasses
       direct-slots
       &allow-other-keys)
  (let ((default-superclass (find-class 'standard-class)))
    (set-superclasses class direct-superclasses default-superclass))
  (set-direct-default-initargs class direct-default-initargs)
  (add-as-subclass-to-superclasses class)
  (set-direct-slots class direct-slots)
  (create-readers-and-writers class))

(defmethod initialize-instance :after
    ((class funcallable-standard-class)
     &key
       (direct-default-initargs nil direct-default-initargs-p)
       direct-superclasses
       (direct-slots nil direct-slots-p)
       &allow-other-keys)
  (let ((default-superclass (find-class 'funcallable-standard-class)))
    (set-superclasses class direct-superclasses default-superclass))
  (when direct-default-initargs-p
    (set-direct-default-initargs class direct-default-initargs))
  (add-as-subclass-to-superclasses class)
  (when direct-slots-p
    (set-direct-slots class direct-slots))
  (create-readers-and-writers class))

;;; I don't know why this definition makes SBCL go into an infinite
;;; recursion.
;; (defmethod reinitialize-instance :after
;;     ((class standard-class)
;;      &rest args
;;      &key
;;      &allow-other-keys)
;;   (map-dependents class
;; 		  (lambda (dependent)
;; 		    (apply #'update-dependent class dependent args))))
