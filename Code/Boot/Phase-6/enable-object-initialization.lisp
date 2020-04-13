(cl:in-package #:sicl-boot-phase-6)

(defun define-class-of (e6)
  (setf (sicl-genv:fdefinition 'class-of e6)
        (lambda (object)
          (let ((result (cond ((typep object 'sicl-boot-phase-3::header)
                               (slot-value object 'sicl-boot-phase-3::%class))
                              ((consp object)
                               (sicl-genv:find-class 'cons e6))
                              ((null object)
                               (sicl-genv:find-class 'null e6))
                              ((symbolp object)
                               (sicl-genv:find-class 'symbol e6))
                              ((integerp object)
                               (sicl-genv:find-class 'fixnum e6))
                              ((streamp object)
                               (sicl-genv:find-class 't e6))
                              (t
                               (class-of object)))))
            result))))

(defun enable-object-initialization (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    ;; The function CLASS-OF is called by SHARED-INITIALIZE in order
    ;; to get the slot-definition metaobjects.
    (define-class-of e6)
    ;; The support code for SHARED-INITIALIZE in phase 6 will need to
    ;; access various slots of class metaobjects and slot-definition
    ;; metaobjects.  Since we are initializing objects in E6, the
    ;; class metaobjects for these objects are located in E5.
    ;; Therefor, it is handy to load the support code for
    ;; SHARED-INITIALIZE into E5.  Notice, however, that we do not
    ;; want to define SHARED-INITIALIZE itself in E5 because we
    ;; already have a definition for it there (imported from the
    ;; host), and we do want to call SHARED-INITIALIZE from functions
    ;; in E6.
    ;;
    ;; GET-PROPERTIES is called by SHARED-INITIALIZE to get the
    ;; INITARGs of the slot-definition metaobject.
    (import-functions-from-host '(get-properties) e5)
    (setf (sicl-genv:special-variable 'sicl-clos:+unbound-slot-value+ e5 t)
          10000000)
    (load-fasl "CLOS/slot-bound-using-index.fasl" e5)
    (load-fasl "CLOS/standard-instance-access.fasl" e5)
    ;; We use the non-generic version of (SETF
    ;; SLOT-VALUE-USING-CLASS), because the generic function takes an
    ;; object as its second argument and the class of that object as
    ;; its first argument.  Therefore the two arguments clash.  They
    ;; can't both be bridge objects, and they can't both be ersatz
    ;; objects.
    (load-fasl "CLOS/slot-value-etc-support.fasl" e5)
    (load-fasl "CLOS/slot-value-etc-defuns.fasl" e5)
    (import-function-from-host '(setf sicl-genv:constant-variable) e5)
    (load-fasl "CLOS/instance-slots-offset-defconstant.fasl" e5)
    (load-fasl "CLOS/shared-initialize-support.fasl" e5)
    ;; Instead of loading the file containing the definition of
    ;; SHARED-INITIALIZE-DEFAULT, we define our own version here
    ;; so that we can have it call the main workhorse function
    ;; SHARED-INITIALIZE-DEFAULT-USING-CLASS-AND-SLOTS in E5.
    (setf (sicl-genv:fdefinition 'sicl-clos::shared-initialize-default e6)
          (lambda (instance slot-names &rest initargs)
            (let* ((class-of (sicl-genv:fdefinition 'class-of e6))
                   (class (funcall class-of instance))
                   (class-slots (sicl-genv:fdefinition 'sicl-clos:class-slots e5))
                   (slots (funcall class-slots class)))
              (apply (sicl-genv:fdefinition
                      'sicl-clos::shared-initialize-default-using-class-and-slots
                      e5)
                     instance slot-names class slots initargs))))
    (import-functions-from-host
     '(list append length consp error member
       cleavir-code-utilities:proper-list-p
       (setf sicl-genv:fdefinition))
     e6)
    (import-functions-from-host
     '(atom cddr (setf cdr) <)
     e5)
    (load-fasl "CLOS/shared-initialize-defgenerics.fasl" e6)
    (load-fasl "CLOS/shared-initialize-defmethods.fasl" e6)
    (load-fasl "CLOS/initialize-instance-support.fasl" e6)
    (load-fasl "CLOS/initialize-instance-defgenerics.fasl" e6)
    (load-fasl "CLOS/initialize-instance-defmethods.fasl" e6)
    (load-fasl "CLOS/make-instance-support.fasl" e5)
    (setf (sicl-genv:fdefinition 'make-instance e6)
          (lambda (class &rest initargs)
            (let ((class-metaobject
                    (if (symbolp class)
                        (sicl-genv:find-class class e5)
                        class)))
              (apply (sicl-genv:fdefinition 'sicl-clos::make-instance-default e5)
                     class-metaobject
                     (sicl-genv:fdefinition 'initialize-instance e6)
                     initargs))))))
