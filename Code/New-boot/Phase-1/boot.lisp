(cl:in-package #:sicl-new-boot-phase-1)

(defgeneric my-make-instance (class &rest initargs))

(defun define-make-instance (client environment)

  (defmethod my-make-instance ((name symbol) &rest initargs)
    (let ((class (clo:find-class client environment name)))
      (apply #'make-instance class initargs)))

  (defmethod my-make-instance ((class class) &rest initargs)
    (apply #'make-instance class initargs))

  (setf (clo:fdefinition client environment 'make-instance)
        #'my-make-instance))

(defun define-ensure-class (client global-environment)
  (setf (clo:fdefinition client global-environment 'ensure-class)
        (lambda (name
                 &key
                   direct-default-initargs
                   direct-superclasses
                   direct-slots
                 &allow-other-keys)
          (let* ((new-slots
                   ;; We must remove the :READERS and :WRITERS
                   ;; properties, because otherwise, they would be
                   ;; created as host methods on host generic
                   ;; functions.
                   (loop for direct-slot in direct-slots
                         for copy = (copy-list direct-slot)
                         collect (progn (remf copy :readers)
                                        (remf copy :writers)
                                        copy)))
                 (new-superclasses
                   (loop for direct-superclass in direct-superclasses
                         collect
                         (clo:find-class
                          client global-environment direct-superclass)))
                 (result
                   (make-instance 'standard-class
                     :name name
                     :direct-default-initargs direct-default-initargs
                     :direct-slots new-slots
                     :direct-superclasses new-superclasses)))
          (setf (clo:find-class client global-environment name)
                result)))))

(defun define-typep (client global-environment)
  (setf (clo:fdefinition client global-environment 'typep)
        (lambda (object type-specifier)
          (cond ((eq type-specifier 'class)
                 (format *trace-output*
                         "Assuming ~s is a class~%" object)
                 t)
                ((eq type-specifier 'method)
                 (format *trace-output*
                         "Assuming ~s is a method~%" object)
                 t)
                ((eq type-specifier 'method-combination)
                 (format *trace-output*
                         "Assuming ~s is a method combination~%" object)
                 t)
                (t
                 (format *trace-output*
                         "Don't know whether ~s is of type ~s~%"
                         object type-specifier)
                 (break))))))

(defun boot (boot)
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment)))
    (sb:define-package-functions client global-environment)
    (setf (sb:e1 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (setf (clo:fdefinition client global-environment 'ensure-method)
          #'ensure-method)
    (setf (clo:fdefinition client global-environment 'closer-mop:method-function)
          #'closer-mop:method-function)
    (sb:import-khazern client global-environment)
    (sb:define-environment-functions client global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (define-make-instance client global-environment)
    (setf (clo:find-class client global-environment 'package)
          (find-class 'parcl-class:package))
    (define-ensure-class client global-environment)
    (sb:ensure-asdf-system
     client environment "sicl-environment-package")
    (let ((environment-symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "*ENVIRONMENT*"))
          (client-symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "*CLIENT*")))
      (clo:make-variable
       client global-environment environment-symbol global-environment)
      (clo:make-variable
       client global-environment client-symbol client))
    (sb:ensure-asdf-system
     client environment "clostrophilia-package")
    (sb:ensure-asdf-system
     client environment "sicl-clos-package")
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy")
    ;; Now, the class T is defined as a host standard class, but when
    ;; methods specialize to T, we must find the host class named T,
    ;; so we just replace the one we just loaded.
    (setf (clo:find-class client global-environment 't)
          (find-class 't))
    ;; It would be better to load the condition system here.
    (setf (clo:macro-function client global-environment 'define-condition)
          (lambda (form environment)
            (declare (ignore environment))
            (list 'defclass (second form) '() '())))
    (sb:ensure-asdf-system
     client environment "acclimation")
    ;; We need to define HANDLER-BIND becuase it is used by Ecclesia.
    ;; The way we define it is that it just expands to a PROGN of the
    ;; forms in the body, with the bindings having no effect.
    (setf (clo:macro-function client global-environment 'handler-bind)
          (lambda (form environment)
            (declare (ignore environment))
            (cons 'progn (rest (rest form)))))
    (clo:make-variable
     client global-environment 'lambda-list-keywords lambda-list-keywords)
    (sb:ensure-asdf-system
     client environment "ecclesia")
    (define-typep client global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-dependent-maintenance")
    (sb:ensure-asdf-system
     client environment "clostrophilia-generic-function-initialization")
    ;; ENSURE-GENERIC-FUNCTION is called by the class initialization
    ;; protocol in order to put reader and writer methods on the
    ;; resulting function.  However, we remove the READER and WRITER
    ;; properties of the canonicalized slot specifiers, because we do
    ;; not want a host generic function to be created.  So then, the
    ;; class initialization protocol will have no readers or writers
    ;; to create, which means that ENSURE-GENERIC-FUNCTION will not be
    ;; called.  So we define it here to signal an error.
    (setf (clo:fdefinition
           client global-environment 'ensure-generic-function)
          (lambda (name &key &allow-other-keys)
            (error "Attempts to create generic function named ~s" name)))
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-initialization")
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-initialization")
    (sb:ensure-asdf-system
     client environment "clostrophilia-slot-definition-initialization")
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-finalization")
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-combination")
    (let ((symbol
            (sb:intern-parcl-symbol
             client "CLOSTROPHILIA" "SET-FUNCALLABLE-INSTANCE-FUNCTION")))
      (setf (clo:fdefinition client global-environment symbol)
            (fdefinition 'closer-mop:set-funcallable-instance-function)))
    (sb:ensure-asdf-system
     client environment "clostrophilia-generic-function-invocation")
    (sb:ensure-asdf-system
     client environment "sicl-new-boot-phase-1-additional-classes")
    (let ((symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "FDEFINITION")))
      (setf (clo:fdefinition client global-environment symbol)
            (fdefinition 'clo:fdefinition))
      (setf (clo:fdefinition client global-environment `(setf ,symbol))
            (fdefinition '(setf clo:fdefinition))))
    (let ((symbol
            (sb:intern-parcl-symbol
             client "SICL-ENVIRONMENT" "FIND-CLASS")))
      (setf (clo:find-class client global-environment symbol)
            (fdefinition 'clo:find-class))
      (setf (clo:find-class client global-environment `(setf ,symbol))
            (fdefinition '(setf clo:find-class))))
    ;; The method on ENSURE-GENERIC-FUNCTION-USING-CLASS specialized
    ;; to NULL calls CLASS-FINAILIZED-P to determine whether it can
    ;; instantiate the class, but the GENERIC-FUNCTION class has an
    ;; initform that sets the corresponding slot to NIL, which is
    ;; normal.  However, in this phase, all classes are host classes,
    ;; so they are finalized.  For that reason, we make
    ;; finalize-inheritance return T always.
    (let ((symbol
            (sb:intern-parcl-symbol
             client "CLOSTROPHILIA" "FINALIZE-INHERITANCE")))
      (setf (clo:fdefinition client global-environment symbol)
            (constantly t)))
    ;; The method on ENSURE-CLASS-USING-CLASS specialized to
    ;; FORWARD-REFERENCED-CLASS calls CHANGE-CLASS to turn the class
    ;; into something other than a FORWARD-REFERENCED-CLASS.  But in
    ;; this phase, we do not have any instances of
    ;; FORWARD-REFERENCED-CLASS, so we define CHANGE-CLASS to signal
    ;; an error.
    (setf (clo:fdefinition client global-environment 'change-class)
          (lambda (&rest arguments)
            (error "CHANGE-CLASS called with arguments ~s" arguments)))
    (sb:ensure-asdf-system
     client environment "sicl-clos-ensure-metaobject-using-class")
    global-environment))
