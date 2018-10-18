(cl:in-package #:sicl-new-boot-phase-3)

(defun enable-class-finalization (boot)
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)) boot
    (setf (sicl-genv:special-variable
           'sicl-clos::*standard-direct-slot-definition* e2 t)
          (sicl-genv:find-class 'sicl-clos:standard-direct-slot-definition e1))
    (setf (sicl-genv:special-variable
           'sicl-clos::*standard-effective-slot-definition* e2 t)
          (sicl-genv:find-class 'sicl-clos:standard-effective-slot-definition e1))
    (sicl-genv:fmakunbound 'sicl-clos:direct-slot-definition-class e2)
    (import-functions-from-host
     '(last remove-duplicates reduce copy-list
       mapcar union find-if-not eql count)
     e2)
    (load-file "CLOS/slot-definition-class-support.lisp" e2)
    (load-file "CLOS/slot-definition-class-defgenerics.lisp" e2)
    (load-file "CLOS/slot-definition-class-defmethods.lisp" e2)
    (load-file "CLOS/class-finalization-defgenerics.lisp" e2)
    (load-file "CLOS/class-finalization-support.lisp" e2)
    (load-file "CLOS/class-finalization-defmethods.lisp" e2)))

(defun finalize-all-classes (boot)
  (format *trace-output* "Finalizing all classes.~%")
  (let* ((e2 (sicl-new-boot:e2 boot))
         (finalization-function
           (sicl-genv:fdefinition 'sicl-clos:finalize-inheritance e2)))
    (do-all-symbols (var)
      (let ((class (sicl-genv:find-class var e2)))
        (unless (null class)
          (funcall finalization-function class)))))
  (format *trace-output* "Done finalizing all classes.~%"))

(defun enable-object-initialization (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    ;; The function CLASS-OF is called by SHARED-INITIALIZE in order
    ;; to get the slot-definition metaobjects.
    (setf (sicl-genv:fdefinition 'class-of e3)
          (lambda (object)
            (slot-value object 'sicl-new-boot-phase-2::%class)))
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
    (load-file "CLOS/instance-slots-offset-defconstant.lisp" e2)
    (load-file "CLOS/shared-initialize-support.lisp" e2)
    ;; The version of SHARED-INITIALIZE-DEFAULT that was defined when
    ;; we loaded the previous file has two problems.  One, it is
    ;; defined in E2, but we need for it to be defined in E3 so that
    ;; it is called from the default method on SHARED-INITIALIZE.  The
    ;; other is that it uses the version of CLASS-OF in E2, but we
    ;; need for it to use the definition of CLASS-OF in E3.
    ;; Therefore, we define a special version of it here.
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
    (import-function-from-host 'error e2)
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

(defun boot-phase-3 (boot)
  (format *trace-output* "Start of phase 3~%")
  (with-accessors ((e1 sicl-new-boot:e1)
                   (e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)
                   (e4 sicl-new-boot:e4)) boot
    (change-class e3 'environment)
    (enable-class-finalization boot)
    (finalize-all-classes boot)
    (enable-defmethod-in-e3 boot)
    (enable-object-initialization boot)
    (load-accessor-defgenerics boot)
    (create-mop-classes boot)))
