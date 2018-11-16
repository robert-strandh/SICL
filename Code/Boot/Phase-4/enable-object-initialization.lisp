(cl:in-package #:sicl-new-boot-phase-4)

(defun enable-object-initialization (boot)
  (with-accessors ((e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    ;; The function CLASS-OF is called by SHARED-INITIALIZE in order
    ;; to get the slot-definition metaobjects.
    (setf (sicl-genv:fdefinition 'class-of e4)
          (lambda (object)
            (slot-value object 'sicl-new-boot-phase-2::%class)))
    ;; The support code for SHARED-INITIALIZE in phase 4 will need to
    ;; access various slots of class metaobjects and slot-definition
    ;; metaobjects.  Since we are initializing objects in E4, the
    ;; class metaobjects for these objects are located in E3.
    ;; Therefor, it is handy to load the support code for
    ;; SHARED-INITIALIZE into E3.  Notice, however, that we do not
    ;; want to define SHARED-INITIALIZE itself in E3 because we
    ;; already have a definition for it there (imported from the
    ;; host), and we do want to call SHARED-INITIALIZE from functions
    ;; in E4.
    ;;
    ;; GET-PROPERTIES is called by SHARED-INITIALIZE to get the
    ;; INITARGs of the slot-definition metaobject.
    (import-functions-from-host '(get-properties) e3)
    (setf (sicl-genv:special-variable 'sicl-clos:+unbound-slot-value+ e3 t)
          10000000)
    (load-file "CLOS/slot-bound-using-index.lisp" e3)
    (load-file "CLOS/standard-instance-access.lisp" e3)
    ;; We use the non-generic version of (SETF
    ;; SLOT-VALUE-USING-CLASS), because the generic function takes an
    ;; object as its second argument and the class of that object as
    ;; its first argument.  Therefore the two arguments clash.  They
    ;; can't both be bridge objects, and they can't both be ersatz
    ;; objects.
    (load-file-protected "CLOS/slot-value-etc-support.lisp" e3)
    (load-file "CLOS/slot-value-etc-defuns.lisp" e3)
    (import-function-from-host '(setf sicl-genv:constant-variable) e3)
    (load-file "CLOS/instance-slots-offset-defconstant.lisp" e3)
    (load-file "CLOS/shared-initialize-support.lisp" e3)
    ;; Instead of loading the file containing the definition of
    ;; SHARED-INITIALIZE-DEFAULT, we define our own version here
    ;; so that we can have it call the main workhorse function
    ;; SHARED-INITIALIZE-DEFAULT-USING-CLASS-AND-SLOTS in E3.
    (setf (sicl-genv:fdefinition 'sicl-clos::shared-initialize-default e4)
          (lambda (instance slot-names &rest initargs)
            (let* ((class-of (sicl-genv:fdefinition 'class-of e4))
                   (class (funcall class-of instance))
                   (class-slots (sicl-genv:fdefinition 'sicl-clos:class-slots e3))
                   (slots (funcall class-slots class)))
              (apply (sicl-genv:fdefinition
                      'sicl-clos::shared-initialize-default-using-class-and-slots
                      e3)
                     instance slot-names class slots initargs))))
    (load-file "CLOS/shared-initialize-defgenerics.lisp" e4)
    (load-file "CLOS/shared-initialize-defmethods.lisp" e4)
    (load-file "CLOS/initialize-instance-support.lisp" e4)
    (load-file "CLOS/initialize-instance-defgenerics.lisp" e4)
    (load-file "CLOS/initialize-instance-defmethods.lisp" e4)
    (load-file "CLOS/make-instance-support.lisp" e3)
    (setf (sicl-genv:fdefinition 'make-instance e4)
          (lambda (class &rest initargs)
            (let ((class-metaobject
                    (if (symbolp class)
                        (sicl-genv:find-class class e3)
                        class)))
              (apply (sicl-genv:fdefinition 'sicl-clos::make-instance-default e3)
                     class-metaobject
                     (sicl-genv:fdefinition 'initialize-instance e4)
                     initargs))))))
