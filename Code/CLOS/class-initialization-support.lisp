(cl:in-package #:sicl-clos)

;;; Implement class initialization according to the section of the
;;; AMOP entitled "Initialization of Class Metaobjects".

;;; For :DIRECT-DEFAULT-INITARGS, the AMOP says that it defaults to
;;; the empty list when the class metaobject is being initialized.  We
;;; take that to implicitly imply that when the class metaobject is
;;; reinitialized as opposed to initialized, there is no default for
;;; this initialization argument, so that if it is not given, the old
;;; value is retained.  The AMOP furthermore says that this argument
;;; is a list of canonicalized default initialization arguments.
;;; While canonicalization is accomplished directly in the DEFCLASS
;;; macro (it has to, because a closure needs to be created there to
;;; get the lexical environment right), it is possible to create a
;;; class directly by calling MAKE-INSTANCE.  For that reason, we must
;;; still check that the :DIRECT-DEFAULT-INITARGS, is a canonicalized
;;; list.

;;; For :DIRECT-SLOTS, the situation is more complicated.  The
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

;;; Provide an :AFTER method on INITIALIZE-INSTANCE to be used when
;;; class metaobjects are instantiated.  As the AMOP says, we have to
;;; provide appropriate default values for the superclasses and we
;;; have to link the superclasses to this new class metaobject.  We
;;; also have to create slot metaobjects from the slot property list
;;; provided to initialize-instance.

;;; The AMOP is not quite clear when it comes to restrictions on
;;; portable programs with respect to methods on INITIALIZE-INSTANCE
;;; and REINITIALIZE-INSTANCE.  It explicitly allows for portable
;;; programs to define :BEFORE and :AFTER methods on
;;; INITIALIZE-INSTANCE, so one might think that the implementation
;;; could not define an :AFTER method on INITIALIZE-INSTANCE because
;;; it could be overridden by a perfectly good portable program.  But
;;; then, when reading the restriction about :AROUND methods (it says
;;; it must be EXTENDING, not OVERRIDING) it seems clear that the
;;; :BEFORE, :AFTER, and :AROUND methods that a portable program is
;;; allowed to define specialize for STRICT SUBCLASSES of the standard
;;; classes, so we are safe if we define an :AFTER method in
;;; INITIALIZE-INSTANCE here.

(defun check-direct-default-initargs (direct-default-initargs)
  (unless (cleavir-code-utilities:proper-list-p direct-default-initargs)
    (error 'direct-default-initargs-must-be-a-proper-list
           :initargs direct-default-initargs))
  (loop for initarg in direct-default-initargs
        do (unless (cleavir-code-utilities:proper-list-p initarg)
             (error 'direct-default-initarg-must-be-a-proper-list
                    :initarg initarg))
           (unless (= (length initarg) 3)
             (error 'direct-default-initarg-must-be-a-list-of-three-elements
                    :initarg initarg))
           (destructuring-bind (name form function) initarg
             (declare (ignore form))
             (unless (symbolp name)
               (error 'name-of-direct-default-initarg-must-be-a-symbol
                      :initarg initarg :name name))
             (unless (functionp function)
               (error 'third-element-of-direct-default-initarg-must-be-a-thunk
                      :initarg initarg :initfunction function)))))

(defun check-direct-superclasses (class direct-superclasses)
  (unless (cleavir-code-utilities:proper-list-p direct-superclasses)
    (error 'direct-superclasses-must-be-proper-list
           :superclasses direct-superclasses))
  (loop for direct-superclass in direct-superclasses
        do (unless (sicl-genv:typep direct-superclass
                                    'class
                                    (sicl-genv:global-environment))
             (error 'superclass-must-be-a-class-metaobject
                    :superclass direct-superclass))
           (unless (validate-superclass class direct-superclass)
             (error 'superclass-not-valid-for-class
                    :superclass direct-superclass))))

(defun check-and-instantiate-direct-slots (class direct-slots)
  (unless (cleavir-code-utilities:proper-list-p direct-slots)
    (error 'direct-slots-must-be-proper-list
           :direct-slots direct-slots))
  ;; FIXME: check that the elements are canonicalized slot specifications
  (loop for canonicalized-slot-specification in direct-slots
        collect (apply #'make-instance
                       (apply #'direct-slot-definition-class
                              class canonicalized-slot-specification)
                       canonicalized-slot-specification)))

(defun add-as-subclass-to-superclasses (class direct-superclasses)
  (loop for superclass in direct-superclasses
        do (add-direct-subclass superclass class)))

(defun create-readers-and-writers (class direct-slots)
  (loop for direct-slot in direct-slots
        do (loop for reader in (slot-definition-readers direct-slot)
                 do (add-reader-method class reader direct-slot))
           (loop for writer in (slot-definition-writers direct-slot)
                 do (add-writer-method class writer direct-slot))))

(defun shared-initialize-around-real-class-default
    (call-next-method
     class
     slot-names
     &rest initargs
     &key
       direct-default-initargs
       direct-superclasses
       direct-slots
     &allow-other-keys)
  (check-direct-default-initargs direct-default-initargs)
  (let ((slots (check-and-instantiate-direct-slots class direct-slots)))
    (apply call-next-method
           class
           slot-names
           :direct-superclasses direct-superclasses
           :direct-default-initargs direct-default-initargs
           :direct-slots slots
           initargs)
    (create-readers-and-writers class slots))
  class)

(defun shared-initialize-after-built-in-class-default (class)
  (setf (precedence-list class)
        (compute-class-precedence-list-assuming-superclasses-finalized class)))

(defun initialize-instance-around-real-class-default
    (call-next-method
     class
     &rest initargs
     &key direct-superclasses
     &allow-other-keys)
  (check-direct-superclasses class direct-superclasses)
  (when (null direct-superclasses)
    (setf direct-superclasses (default-superclasses class)))
  (apply call-next-method
         class
         :direct-superclasses direct-superclasses
         initargs)
  (add-as-subclass-to-superclasses class direct-superclasses)
  class)
