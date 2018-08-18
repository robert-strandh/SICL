(cl:in-package #:sicl-new-boot-phase-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Creating MOP classes.
;;;
;;; The purpose of this step of phase 1 is to create a class hierarchy
;;; corresponding to the MOP classes.  These classes will mostly be
;;; host funallable standard classes.
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

;;; Add a new reader method to a generic function.  GENERIC-FUNCTION
;;; is a generic function metaobject to which the new reader method
;;; should be added.  SLOT-NAME is the name of the slot to be read by
;;; the new reader method.  CLASS is a class metaobject and it is the
;;; specializer to be used in the new method.
(defun add-reader-method (generic-function slot-name class)
  (let ((temp (gensym)))
    (setf (fdefinition temp) generic-function)
    (setf (find-class temp) class)
    (eval `(defmethod ,temp ((object ,temp))
             (slot-value object ',slot-name)))
    (fmakunbound temp)
    (setf (find-class temp) nil)))

;;  (add-method generic-function (make-reader-method slot-name class)))

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

;;; Add a new writer method to a generic function.  GENERIC-FUNCTION
;;; is a generic function metaobject to which the new writer method
;;; should be added.  SLOT-NAME is the name of the slot to be written
;;; by the new writer method.  CLASS is a class metaobject and it is
;;; the specializer to be used in the new method.
(defun add-writer-method (generic-function slot-name class)
  (let ((temp (gensym)))
    (setf (fdefinition temp) generic-function)
    (setf (find-class temp) class)
    (eval `(defmethod ,temp (value (object ,temp))
             (setf (slot-value object ',slot-name) value)))
    (fmakunbound temp)
    (setf (find-class temp) nil)))

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
                   ((:metaclass metaclass-name)
                    'closer-mop:funcallable-standard-class)
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
;;; class FUNCALLABLE-STANDARD-OBJECT, which is itself a subclass of
;;; STANDAR-OBJECT.  This way, we are sure that all our MOP classes in
;;; phase 1 are in fact subclass of the host class STANDARD-OBJECT.
;;; We use FUNCALLABLE-STANDARD-OBJECT rather than STANDARD-OBJECT
;;; because we want some of our instances to be functions, and rather
;;; then trying to convince the host CLOS implementation to let us mix
;;; standard objects and funcallable standard objects, we just make
;;; all our classes subclasses of FUNCALLABLE-STANDARD-OBJECT.
(defun define-class-t-phase1 (env)
  (setf (sicl-genv:find-class 't env)
        (find-class 'closer-mop:funcallable-standard-object)))

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

(defun create-mop-classes-phase1 (env1 env2)
  (load-file "CLOS/defclass-defmacro.lisp" env1)
  (create-exceptional-mop-classes-phase1 env1)
  (define-ensure-class env1 env1 env2)
  (define-funcallable-standard-class)
  (load-file "CLOS/standard-object-defclass.lisp" env1)
  (load-file "CLOS/metaobject-defclass.lisp" env1)
  (load-file "CLOS/method-defclass.lisp" env1)
  (load-file "CLOS/standard-method-defclass.lisp" env1)
  (load-file "CLOS/standard-accessor-method-defclass.lisp" env1)
  (load-file "CLOS/standard-reader-method-defclass.lisp" env1)
  (load-file "CLOS/standard-writer-method-defclass.lisp" env1)
  (load-file "CLOS/slot-definition-defclass.lisp" env1)
  (load-file "CLOS/standard-slot-definition-defclass.lisp" env1)
  (load-file "CLOS/direct-slot-definition-defclass.lisp" env1)
  (load-file "CLOS/effective-slot-definition-defclass.lisp" env1)
  (load-file "CLOS/standard-direct-slot-definition-defclass.lisp" env1)
  (load-file "CLOS/standard-effective-slot-definition-defclass.lisp" env1)
  (load-file "CLOS/method-combination-defclass.lisp" env1)
  (load-file "CLOS/simple-method-combination-defclass.lisp" env1)
  (load-file "CLOS/specializer-defclass.lisp" env1)
  (load-file "CLOS/eql-specializer-defclass.lisp" env1)
  (load-file "CLOS/class-unique-number-defparameter.lisp" env1)
  (load-file "CLOS/class-defclass.lisp" env1)
  (load-file "CLOS/forward-referenced-class-defclass.lisp" env1)
  (load-file "CLOS/real-class-defclass.lisp" env1)
  (load-file "CLOS/regular-class-defclass.lisp" env1)
  (load-file "CLOS/standard-class-defclass.lisp" env1)
  (load-file "CLOS/funcallable-standard-class-defclass.lisp" env1)
  (load-file "CLOS/built-in-class-defclass.lisp" env1)
  (load-file "CLOS/funcallable-standard-object-defclass.lisp" env1)
  (load-file "CLOS/generic-function-defclass.lisp" env1)
  (load-file "CLOS/standard-generic-function-defclass.lisp" env1))
