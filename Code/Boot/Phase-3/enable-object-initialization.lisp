(cl:in-package #:sicl-boot-phase-3)

(defun define-class-of (e3)
    (setf (sicl-genv:fdefinition 'class-of e3)
          (lambda (object)
            (let ((result (cond ((typep object 'sicl-boot-phase-2::header)
                                 (slot-value object 'sicl-boot-phase-2::%class))
                                ((consp object)
                                 (sicl-genv:find-class 'cons e3))
                                ((symbolp object)
                                 (sicl-genv:find-class 'symbol e3))
                                ((integerp object)
                                 (sicl-genv:find-class 'fixnum e3))
                                ((streamp object)
                                 (sicl-genv:find-class 't e3))
                                (t
                                 (class-of object)))))
              result))))

(defun enable-object-initialization (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)) boot
    (define-class-of e3)
    ;; The support code for SHARED-INITIALIZE in phase 3 will need to
    ;; access various slots of class metaobjects and slot-definition
    ;; metaobjects.  Since we are initializing objects in E3, the
    ;; class metaobjects for these objects are located in E2.
    ;; Therefor, it is handy to load the support code for
    ;; SHARED-INITIALIZE into E2.  Notice, however, that we do not
    ;; want to define SHARED-INITIALIZE itself in E2 because we
    ;; already have a definition for it there (imported from the
    ;; host), and we do want to call SHARED-INITIALIZE from functions
    ;; in E3.
    ;;
    ;; GET-PROPERTIES is called by SHARED-INITIALIZE to get the
    ;; INITARGs of the slot-definition metaobject.
    (import-functions-from-host '(get-properties) e2)
    (setf (sicl-genv:special-variable 'sicl-clos:+unbound-slot-value+ e2 t)
          10000000)
    (load-file "CLOS/slot-bound-using-index.lisp" e2)
    (load-file "CLOS/slot-value-etc-defgenerics.lisp" e2)
    (load-file "CLOS/slot-value-etc-support.lisp" e2)
    (load-file "CLOS/slot-value-etc-defmethods.lisp" e2)
    (import-function-from-host '(setf sicl-genv:constant-variable) e2)
    (load-file "CLOS/instance-slots-offset-defconstant.lisp" e2)
    (load-file "CLOS/shared-initialize-support.lisp" e2)
    ;; Instead of loading the file containing the definition of
    ;; SHARED-INITIALIZE-DEFAULT, we define our own version here
    ;; so that we can have it call the main workhorse function
    ;; SHARED-INITIALIZE-DEFAULT-USING-CLASS-AND-SLOTS in E2.
    (setf (sicl-genv:fdefinition 'sicl-clos::shared-initialize-default e3)
          (lambda (instance slot-names &rest initargs)
            (let* ((class-of (sicl-genv:fdefinition 'class-of e3))
                   (class (funcall class-of instance))
                   (class-slots (sicl-genv:fdefinition 'sicl-clos:class-slots e2))
                   (slots (funcall class-slots class)))
              (apply (sicl-genv:fdefinition
                      'sicl-clos::shared-initialize-default-using-class-and-slots
                      e2)
                     instance slot-names class slots initargs))))
    (load-file "CLOS/shared-initialize-defgenerics.lisp" e3)
    (load-file "CLOS/shared-initialize-defmethods.lisp" e3)
    (load-file "CLOS/initialize-instance-support.lisp" e3)
    (load-file "CLOS/initialize-instance-defgenerics.lisp" e3)
    (load-file "CLOS/initialize-instance-defmethods.lisp" e3)
    (load-file "CLOS/make-instance-support.lisp" e2)
    (setf (sicl-genv:fdefinition 'make-instance e3)
          (lambda (class &rest initargs)
            (let ((class-metaobject
                    (if (symbolp class)
                        (sicl-genv:find-class class e2)
                        class)))
              (apply (sicl-genv:fdefinition 'sicl-clos::make-instance-default e2)
                     class-metaobject
                     (sicl-genv:fdefinition 'initialize-instance e3)
                     initargs))))))
