(cl:in-package #:sicl-boot-phase-5)

(defun define-class-of (e5)
  (setf (sicl-genv:fdefinition 'class-of e5)
        (lambda (object)
          (let ((result (cond ((typep object 'sicl-boot-phase-3::header)
                               (slot-value object 'sicl-boot-phase-3::%class))
                              ((consp object)
                               (sicl-genv:find-class 'cons e5))
                              ((symbolp object)
                               (sicl-genv:find-class 'symbol e5))
                              ((integerp object)
                               (sicl-genv:find-class 'fixnum e5))
                              ((streamp object)
                               (sicl-genv:find-class 't e5))
                              (t
                               (class-of object)))))
            result))))

(defun enable-object-initialization (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5)) boot
    ;; The function CLASS-OF is called by SHARED-INITIALIZE in order
    ;; to get the slot-definition metaobjects.
    (define-class-of e5)
    ;; The support code for SHARED-INITIALIZE in phase 5 will need to
    ;; access various slots of class metaobjects and slot-definition
    ;; metaobjects.  Since we are initializing objects in E5, the
    ;; class metaobjects for these objects are located in E4.
    ;; Therefor, it is handy to load the support code for
    ;; SHARED-INITIALIZE into E4.  Notice, however, that we do not
    ;; want to define SHARED-INITIALIZE itself in E4 because we
    ;; already have a definition for it there (imported from the
    ;; host), and we do want to call SHARED-INITIALIZE from functions
    ;; in E5.
    ;;
    ;; GET-PROPERTIES is called by SHARED-INITIALIZE to get the
    ;; INITARGs of the slot-definition metaobject.
    (import-functions-from-host '(get-properties) e4)
    (setf (sicl-genv:special-variable 'sicl-clos:+unbound-slot-value+ e4 t)
          10000000)
    (load-fasl "CLOS/slot-bound-using-index.fasl" e4)
    (load-fasl "CLOS/standard-instance-access.fasl" e4)
    ;; We use the non-generic version of (SETF
    ;; SLOT-VALUE-USING-CLASS), because the generic function takes an
    ;; object as its second argument and the class of that object as
    ;; its first argument.  Therefore the two arguments clash.  They
    ;; can't both be bridge objects, and they can't both be ersatz
    ;; objects.
    (load-fasl "CLOS/slot-value-etc-support.fasl" e4)
    (load-fasl "CLOS/slot-value-etc-defuns.fasl" e4)
    (import-function-from-host '(setf sicl-genv:constant-variable) e4)
    (load-fasl "CLOS/instance-slots-offset-defconstant.fasl" e4)
    (load-fasl "CLOS/shared-initialize-support.fasl" e4)
    ;; Instead of loading the file containing the definition of
    ;; SHARED-INITIALIZE-DEFAULT, we define our own version here
    ;; so that we can have it call the main workhorse function
    ;; SHARED-INITIALIZE-DEFAULT-USING-CLASS-AND-SLOTS in E4.
    (setf (sicl-genv:fdefinition 'sicl-clos::shared-initialize-default e5)
          (lambda (instance slot-names &rest initargs)
            (let* ((class-of (sicl-genv:fdefinition 'class-of e5))
                   (class (funcall class-of instance))
                   (class-slots (sicl-genv:fdefinition 'sicl-clos:class-slots e4))
                   (slots (funcall class-slots class)))
              (format *trace-output* "~%5Initargs: ~s~%" initargs)
              (apply (sicl-genv:fdefinition
                      'sicl-clos::shared-initialize-default-using-class-and-slots
                      e4)
                     instance slot-names class slots initargs))))
    (import-functions-from-host
     '(list append
       (setf sicl-genv:fdefinition))
     e5)
    (import-functions-from-host
     '(atom cddr (setf cdr))
     e4)
    (load-fasl "CLOS/shared-initialize-defgenerics.fasl" e5)
    (load-fasl "CLOS/shared-initialize-defmethods.fasl" e5)
    (load-fasl "CLOS/initialize-instance-support.fasl" e5)
    (load-fasl "CLOS/initialize-instance-defgenerics.fasl" e5)
    (load-fasl "CLOS/initialize-instance-defmethods.fasl" e5)
    (load-fasl "CLOS/make-instance-support.fasl" e4)
    (setf (sicl-genv:fdefinition 'make-instance e5)
          (lambda (class &rest initargs)
            (let ((class-metaobject
                    (if (symbolp class)
                        (sicl-genv:find-class class e4)
                        class)))
              (apply (sicl-genv:fdefinition 'sicl-clos::make-instance-default e4)
                     class-metaobject
                     (sicl-genv:fdefinition 'initialize-instance e5)
                     initargs))))))
