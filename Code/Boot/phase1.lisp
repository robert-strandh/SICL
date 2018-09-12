(cl:in-package #:sicl-boot)

;;;; The purpose of phase 1 is to create a class hierarchy that
;;;; mirrors the MOP class hierarchy.  The classes we create are all
;;;; host standard classes.  We create these classes by loading files
;;;; from the CLOS sub-directory that contain DEFCLASS forms defining
;;;; these classes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating class accessor generic functions.
;;;
;;; There are different ways in which we can accomplish this task,
;;; given the constraint that it has to be done by loading DEFGENERIC
;;; forms corresponding to the class accessor generic functions.
;;;
;;; We obviously can not use the host definition of DEFGENERIC because
;;; it might clobber any existing host definition.  In particular,
;;; this is the case for class accessor functions that have names in
;;; the COMMON-LISP package, for instance CLASS-NAME.  Since we must
;;; supply our own definition of DEFGENERIC, we are free to do what we
;;; want.
;;;
;;; The way we have chosen to do it is to provide both a specific
;;; definition of DEFGENERIC and a specific definition of
;;; ENSURE-GENERIC-FUNCTION.  The difference between this version of
;;; DEFGENERIC and the standard version of it is that this version
;;; deletes any existing function associated with the name before
;;; creating a new one.  This way, we are sure that each time we run
;;; the bootstrapping process, we have a fresh generic function.  We
;;; do NOT delete any existing function in the special version of
;;; ENSURE-GENERIC-FUNCTION, because we want to use that special
;;; version to find the existing generic function when the
;;; class-initialization protocol needs to add a reader or a writer
;;; method on the generic function.  On the other hand, we do not want
;;; to use the ordinary SICL version of ENSURE-GENERIC-FUNCTION
;;; because it requires a battery of additional functionality in the
;;; form of other generic functions.  So to keep things simple, we
;;; supply a special bootstrapping version of it.
;;;
;;; We can rely entirely on the host to execute the generic-function
;;; initialization protocol.  There is a little question about the
;;; initialization argument :METHOD-COMBINATION, but it appears that
;;; it defaults to the standard method combination, so we do not have
;;; to pass a method-combination metaobject here.

(defun define-generic-function-definers-phase1 (env1 env2)
  ;; We define a special version of ENSURE-GENERIC-FUNCTION in the
  ;; run-time environment to be used in phase 1.  This version of
  ;; ENSURE-GENERIC-FUNCTION is defined in ENV1 and operates in ENV2.
  ;; It checks whether there is already a function named FUNCTION-NAME
  ;; in ENV2.  If so that function is returned, and it is assumed to
  ;; be a generic function.  If not, an instance of the host class
  ;; STANDARD-GENERIC-FUNCTION is created and associated with
  ;; FUNCTION-NAME in ENV2.
  (setf (sicl-genv:fdefinition 'ensure-generic-function env1)
        (lambda (function-name &rest arguments)
          (let ((args (copy-list arguments)))
            (loop while (remf args :environment))
            (if (sicl-genv:fboundp function-name env2)
                (sicl-genv:fdefinition function-name env2)
                (setf (sicl-genv:fdefinition function-name env2)
                      (apply #'make-instance 'standard-generic-function
                             :name function-name
                             args))))))
  ;; Define the macro DEFGENERIC for use in phase 1.  We define it a
  ;; bit differently from its usual definition.  It is defined in the
  ;; environment ENV1.  The expansion defines a generic function in
  ;; the environment in which the form is executed.  However, before
  ;; defining it, we remove the existing generic function if it
  ;; exists.  This way, we are sure to get a fresh generic function,
  ;; as opposed to one that happened to have been imported from the
  ;; host.  We must, of course, make sure that we execute a DEFGENERIC
  ;; form for a particular generic function exactly once, but we can
  ;; do that because we completely master the boot process.
  (setf (sicl-genv:macro-function 'defgeneric env1)
        (lambda (form environment)
          (declare (ignore environment))
          ;; The DEFGENERIC form must be evaluated in ENV1, so that
          ;; the expansion is also evaluated in ENV1.  The reason for
          ;; that is that we call ENSURE-GENERIC-FUNCTION, and that
          ;; has to be the function defined in ENV1 above.
          `(progn (sicl-genv:fmakunbound ',(second form) ,env2)
                  (ensure-generic-function
                   ',(second form)
                   :lambda-list ',(third form))))))

(defun create-class-accessor-generic-functions-phase1 ()
  (let ((env1 *phase1-mop-class-env*)
        (env2 *phase1-mop-accessor-env*))
    (define-generic-function-definers-phase1 env1 env2)
    (ld "CLOS/accessor-defgenerics.lisp" env1 env1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating MOP classes.
;;;
;;; The purpose of this step of phase 1 is to create a class hierarchy
;;; corresponding to the MOP classes.  These classes will mostly be
;;; host standard classes.
;;;
;;; The main problem we need to solve is that we can not let the host
;;; be in charge of the full class-initialization protocol.  The
;;; reason for that is that, as part of the class-initialization
;;; protocol, reader and writer methods are added on named generic
;;; functions.  If we were to let the host be in charge of that part
;;; of the class-initialization protocol, it would add methods to
;;; generic functions associated with names in the global host
;;; environment, which we do not want.  To solve this problem, we
;;; create host classes using arguments that suggest that slots have
;;; no readers and no writers.  We then add those readers and writers
;;; here.  These reader and writers will call the host functions
;;; SLOT-VALUE and (SETF SLOT-VALUE) to accomplish their task.
;;;
;;; We can, however, let the host take care of executing the full
;;; slot-definition-initialization protocol.  All we need to do is to
;;; make sure that we pass a list of canonicalized slot specifications
;;; as the :DIRECT-SLOTS keyword argument to MAKE-INSTANCE when we
;;; create a class.  As mentioned before, though, we must remove the
;;; :READERS and :WRITERS entries in those canonicalized slot
;;; specifications.

;;; Take the name of a slot and define a reader function to be used in
;;; a method.  This reader function returns the value of the slot.
;;; The resulting function calls the host function SLOT-VALUE to
;;; accomplish its task.
(defun make-reader-function (slot-name)
  (compile nil
           `(lambda (args next-methods)
              (declare (ignore next-methods))
              (slot-value (car args) ',slot-name))))

;;; Create a reader method.  SLOT-NAME is the name of a slot for which
;;; a reader method should be created.  CLASS is a class metaobject
;;; and it is the specializer to be used in the method.
(defun make-reader-method (slot-name class)
  (make-instance 'standard-method
    :lambda-list '(object)
    :specializers (list class)
    :function (make-reader-function slot-name)))

;;; Add a new reader method to a generic function.  GENERIC-FUNCTION
;;; is a generic function metaobject to which the new reader method
;;; should be added.  SLOT-NAME is the name of the slot to be read by
;;; the new reader method.  CLASS is a class metaobject and it is the
;;; specializer to be used in the new method.
(defun add-reader-method (generic-function slot-name class)
  (add-method generic-function (make-reader-method slot-name class)))

;;; Add new reader methods to each generic function in a list of
;;; generic functions, each given by its name.  GENERIC-FUNCTION-NAMES
;;; is a list of names of generic functions to which new reader
;;; methods should be added.  ENV is the environment in which the name
;;; of each generic function should be mapped to a generic-function
;;; metaobject.  SLOT-NAME is the name of the slot to be read by each
;;; of the the new reader methods.  CLASS is a class metaobject and it
;;; is the specializer to be used in the new methods.
(defun add-reader-methods (generic-function-names env slot-name class)
  (loop for name in generic-function-names
        for generic-function = (sicl-genv:fdefinition name env)
        do (add-reader-method generic-function slot-name class)))

;;; Take the name of a slot and define a writer function to be used in
;;; a method.  This writer function sets the value of the slot.  The
;;; resulting function calls the host function (SETF SLOT-VALUE) to
;;; accomplish its task.
(defun make-writer-function (slot-name)
  (compile nil
           `(lambda (args next-methods)
              (declare (ignore next-methods))
              (setf (slot-value (cadr args) ',slot-name)
                    (car args)))))

;;; Create a writer method.  SLOT-NAME is the name of a slot for which
;;; a writer method should be created.  CLASS is a class metaobject
;;; and it is the specializer to be used in the method.
(defun make-writer-method (slot-name class)
  (make-instance 'standard-method
    :lambda-list '(new-value object)
    :specializers (list (find-class t) class)
    :function (make-writer-function slot-name)))

;;; Add a new writer method to a generic function.  GENERIC-FUNCTION
;;; is a generic function metaobject to which the new writer method
;;; should be added.  SLOT-NAME is the name of the slot to be written
;;; by the new writer method.  CLASS is a class metaobject and it is
;;; the specializer to be used in the new method.
(defun add-writer-method (generic-function slot-name class)
  (add-method generic-function (make-writer-method slot-name class)))

;;; Add new writer methods to each generic function in a list of
;;; generic functions, each given by its name.  GENERIC-FUNCTION-NAMES
;;; is a list of names of generic functions to which new writer
;;; methods should be added.  ENV is the environment in which the name
;;; of each generic function should be mapped to a generic-function
;;; metaobject.  SLOT-NAME is the name of the slot to be written by
;;; each of the new writer methods.  CLASS is a class metaobject and
;;; it is the specializer to be used in the new methods.
(defun add-writer-methods (generic-function-names env slot-name class)
  (loop for name in generic-function-names
        for generic-function = (sicl-genv:fdefinition name env)
        do (add-writer-method generic-function slot-name class)))

;;; Add accessor methods according to a canonicalized slot
;;; specification.  Recall that a canonicalized slot specification is
;;; a property list with alternating keywords arguments and values to
;;; be passed to MAKE-INSTANCE in order to create a
;;; DIRECT-SLOT-DEFINITION metaobject.  SLOT-SPEC is such a
;;; canonicalized slot specification.  In particular, it might contain
;;; keyword arguments :READERS and :WRITERS where the value is a list
;;; of names of generic functions to which to add reader and writer
;;; methods.  It also contains a keyword argument :NAME where the
;;; value is a symbol naming the slot.  CLASS is a class metaobject
;;; with a direct slot corresponding to SLOT-SPEC.  ENV is an
;;; environment to be used to translate names of generic functions to
;;; generic function metaobjects.
(defun add-accessor-methods (slot-spec class env)
  (let ((slot-name (getf slot-spec :name)))
    (add-reader-methods (getf slot-spec :readers) env slot-name class)
    (add-writer-methods (getf slot-spec :writers) env slot-name class)))

;;; Recall that a canonicalized slot specification is a property list
;;; with alternating keywords arguments and values to be passed to
;;; MAKE-INSTANCE in order to create a DIRECT-SLOT-DEFINITION
;;; metaobject.  SLOT-SPEC is such a canonicalized slot specification.
;;; In particular, it might contain keyword arguments :READERS and
;;; :WRITERS where the value is a list of names of generic functions
;;; to which to add reader and writer methods.  We return a copy of
;;; SLOT-SPEC in which the keyword arguments :READERS and :WRITERS
;;; have been removed.
(defun remove-readers-and-writers-from-slot-spec (slot-spec)
  (loop for (name value) on slot-spec by #'cddr
        unless (member name '(:readers :writers))
          collect name
          and collect value))

;;; Remove keyword arguments :READERS and :WRITERS from each canonical
;;; slot specification in a list of such canonical slot
;;; specifications.
(defun remove-readers-and-writers-from-slot-specs (slot-specs)
  (mapcar #'remove-readers-and-writers-from-slot-spec slot-specs))

;;; This function defines a version of ENSURE-CLASS to be used in
;;; phase 1.  The definition of ENSURE-CLASS is made in ENV1.  This
;;; version of ENSURE-CLASS defines a new class in ENV2.  It also uses
;;; ENV2 to find superclasses of the class to be defined.  ENV3 is the
;;; environment containing the generic functions to which slot reader
;;; and slot writer methods are to be added.
(defun define-ensure-class (env1 env2 env3)
  (setf (sicl-genv:fdefinition 'sicl-clos:ensure-class env1)
        (lambda (class-name
                 &key
                   direct-slots
                   ((:direct-superclasses direct-superclass-names))
                   name
                   ((:metaclass metaclass-name) 'standard-class)
                 &allow-other-keys)
          ;; We should be called only for the expansion of DEFCLASS,
          ;; and we make sure that we always pass the name of a
          ;; metaclass, and never a class metaobject.
          (assert (symbolp metaclass-name))
          (let (;; In phase 1, the metaclass is a host class, so we
                ;; look for it in the host environment.
                (metaclass (find-class metaclass-name))
                (slot-copies
                  (remove-readers-and-writers-from-slot-specs direct-slots))
                ;; The direct superclasses, on the other hand, are to
                ;; be found in the same environment as the one in
                ;; which this class wil. be defined.
                (direct-superclasses
                  (loop for name in direct-superclass-names
                        collect (sicl-genv:find-class name env2))))
            (let ((class (make-instance metaclass
                           :name (make-symbol (symbol-name name))
                           :direct-slots slot-copies
                           :direct-superclasses direct-superclasses)))
              (setf (sicl-genv:find-class class-name env2) class)
              (loop for slot-spec in direct-slots
                    do (add-accessor-methods slot-spec class env3)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Manual creation of some MOP classes.

;;; Since we do not use the host DEFCLASS macro nor the host version
;;; of ENSURE-CLASS in phase 1, our classes do not automatically have
;;; the host class named STANDARD-OBJECT as a superclass.  But being a
;;; subclass of STANDARD-OBJECT is a requirement for the host generic
;;; function INITIALIZE-INSTANCE to be able to initialize instances of
;;; a class.  We solve this problem by defining a special version of
;;; the class named T in phase 1 that in fact is the same as the host
;;; class STANDARD-OBJECT.  This way, we are sure that all our MOP
;;; classes in phase 1 are in fact subclass of the host class
;;; STANDARD-OBJECT.
(defun define-class-t-phase1 (env)
  (setf (sicl-genv:find-class 't env)
        (find-class 'standard-object)))

;;; We need a special definition of the class named FUNCTION in phase
;;; 1, because we want instances of this class to be funcallable in
;;; the host.  For that reason, we create this class as an instance of
;;; the host class FUNCALLABLE-STANDARD-CLASS.
(defun define-class-function-phase1 (env)
  (setf (sicl-genv:find-class 'function env)
        (make-instance 'closer-mop:funcallable-standard-class
          :name (make-symbol (symbol-name '#:function)))))

(defun create-exceptional-mop-classes-phase1 (env)
  (define-class-t-phase1 env)
  (define-class-function-phase1 env))

;;; This function defines class SICL-CLOS:FUNCALLABLE-STANDARD-CLASS
;;; in the host environment to be the same as the host version of that
;;; class.
(defun define-funcallable-standard-class ()
  (setf (find-class 'sicl-clos:funcallable-standard-class)
        (find-class 'closer-mop:funcallable-standard-class)))

(defun create-mop-classes-phase1 ()
  (let ((env1 *phase1-mop-class-env*)
        (env2 *phase1-mop-accessor-env*))
    (ld "CLOS/defclass-support.lisp" env1 env1)
    (ld "CLOS/defclass-defmacro.lisp" env1 env1)
    (create-exceptional-mop-classes-phase1 env1)
    (define-ensure-class env1 env1 env2)
    (define-funcallable-standard-class)
    (ld "CLOS/standard-object-defclass.lisp" env1 env1)
    (ld "CLOS/metaobject-defclass.lisp" env1 env1)
    (ld "CLOS/method-defclass.lisp" env1 env1)
    (ld "CLOS/standard-method-defclass.lisp" env1 env1)
    (ld "CLOS/standard-accessor-method-defclass.lisp" env1 env1)
    (ld "CLOS/standard-reader-method-defclass.lisp" env1 env1)
    (ld "CLOS/standard-writer-method-defclass.lisp" env1 env1)
    (ld "CLOS/slot-definition-defclass.lisp" env1 env1)
    (ld "CLOS/standard-slot-definition-defclass.lisp" env1 env1)
    (ld "CLOS/direct-slot-definition-defclass.lisp" env1 env1)
    (ld "CLOS/effective-slot-definition-defclass.lisp" env1 env1)
    (ld "CLOS/standard-direct-slot-definition-defclass.lisp" env1 env1)
    (ld "CLOS/standard-effective-slot-definition-defclass.lisp" env1 env1)
    (ld "CLOS/method-combination-defclass.lisp" env1 env1)
    (ld "CLOS/specializer-defclass.lisp" env1 env1)
    (ld "CLOS/eql-specializer-defclass.lisp" env1 env1)
    (ld "CLOS/class-unique-number-defparameter.lisp" env1 env1)
    (ld "CLOS/class-defclass.lisp" env1 env1)
    (ld "CLOS/forward-referenced-class-defclass.lisp" env1 env1)
    (ld "CLOS/real-class-defclass.lisp" env1 env1)
    (ld "CLOS/regular-class-defclass.lisp" env1 env1)
    (ld "CLOS/standard-class-defclass.lisp" env1 env1)
    (ld "CLOS/funcallable-standard-class-defclass.lisp" env1 env1)
    (ld "CLOS/built-in-class-defclass.lisp" env1 env1)
    (ld "CLOS/funcallable-standard-object-defclass.lisp" env1 env1)
    (ld "CLOS/generic-function-defclass.lisp" env1 env1)
    (ld "CLOS/standard-generic-function-defclass.lisp" env1 env1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Main entry point of phase 1.

(defun phase1 ()
  (message "Start of phase 1~%")
  (create-class-accessor-generic-functions-phase1)
  (create-mop-classes-phase1)
  (message "End of phase 1~%"))

;;  LocalWords:  accessor metaobject canonicalized metaobjects
