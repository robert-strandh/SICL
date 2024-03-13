(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun boot (boot)
  (format *trace-output* "**************** Phase 2~%")
  (let* ((client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (setf (sb:e2 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    ;; We saved the SICL class named T under the name sb::sicl-t.  Now
    ;; we need to put it back as class T in E1, because it is going to
    ;; be used as a specializer in bridge generic functions.
    (setf (clo:find-class client (sb:e1 boot) 't)
          (clo:find-class client (sb:e1 boot) 'sb::sicl-t))
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (sb:import-khazern client global-environment)
    (sb:define-environment-functions client global-environment)
    (clo:make-variable client global-environment
                       @sicl-environment:*environment*
                       global-environment)
    (clo:make-variable client global-environment
                       @sicl-environment:*client*
                       client)
    (define-ensure-method-combination-template
        client (sb:e1 boot) global-environment)
    (define-^ensure-method-combination
        client (sb:e1 boot) global-environment)
    (define-find-method-combination-template client global-environment)
    (setf (clo:fdefinition
           client global-environment
           @sicl-clos:^ensure-generic-function-using-class)
          (clo:fdefinition
           client (sb:e1 boot)
           @sicl-clos:ensure-generic-function-using-class))
    (setf (clo:fdefinition
           client global-environment @sicl-clos:^ensure-class-using-class)
          (clo:fdefinition
           client (sb:e1 boot) @sicl-clos:ensure-class-using-class))
    (setf (clo:fdefinition
           client global-environment
           @sicl-clos:^ensure-method-using-generic-function)
          (clo:fdefinition
           client (sb:e1 boot)
           @sicl-clos:ensure-method-using-generic-function))
    (sb:ensure-asdf-system client environment "sicl-clos-ensure-metaobject")
    (define-ecclesia-functions client (sb:e1 boot) global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-combination")
    ;; VALIDATE-SUPERCLASS is going to be called with the class T as
    ;; one of the arguments, but in phase 1 we replace the target
    ;; class T by the host class T so that unspecialized methods, or
    ;; methods specialized to T use the host class T.  So
    ;; VALIDATE-SUPERCLASS is not functional as it is in E1.  Sice we
    ;; do not expect to load code for which VALIDATE-SUPERCLASS
    ;; returns NIL, we make it always return T.
    (setf (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:validate-superclass)
          (constantly t))
    ;; This might be a mistake.  The thing is that the code for adding
    ;; reader and writer methods loaded into E1 calls
    ;; ENSURE-GENERIC-FUNCTION, but the generic function it needs to
    ;; ensure is in E2.  And ENSURE-GENERIC-FUNCTION in E1 has been
    ;; set in phase 1 to call ERROR.  The easy way to fix this is to
    ;; replace ENSURE-GENERIC-FUNCTION in E1 by the new one now in E2.
    ;; But I am not very happy about modifying E1 in phase 2.  A
    ;; better solution would be for the code that adds readers and
    ;; writers to call (say) _ENSURE-GENERIC-FUNCTION, and to define
    ;; that function in E1 to be the ENSURE-GENERIC-FUNCTION of E2.
    ;; But that code is in Clostrophilia, and it is not ideal to have
    ;; Clostrophilia code reflect the SICL bootstrapping procedure. 
    (setf (clo:fdefinition client (sb:e1 boot) 'ensure-generic-function)
          (clo:fdefinition
           client global-environment 'ensure-generic-function))
    (setf (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class client global-environment 't)))
    (setf (clo:fdefinition
           client global-environment @sicl-clos:^make-instance)
          (clo:fdefinition client (sb:e1 boot) 'make-instance))
    ;; ADD-DIRECT-METHOD is called by ADD-METHOD, but it doesn't make
    ;; sense to add a SICL method to a host class.
    (setf (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:add-direct-method)
          (constantly nil))
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy")
    (sb:ensure-asdf-system
     client environment "sicl-asdf-packages")
    (setf (clo:macro-function
           client global-environment @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function
           client global-environment @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     client environment "predicament-base" :load-system-file t)
    ;; The system predicament base contains the definition of the
    ;; variable PREDICAMENT-ASDF:*STRING-DESIGNATORS*, so when we
    ;; loaded that system into E2, that variable got defined in E2.
    ;; However, we now want to load a a package definition into E2
    ;; that uses the value of that variable at read time.  But read
    ;; time is done by Eclector in the host envronment, and that
    ;; variable is not defined in the host environment.  So we do that
    ;; with the following code.
    (let* ((symbol @predicament-asdf:*string-designators*)
           (value (clo:symbol-value client global-environment symbol)))
      (eval `(defparameter ,symbol ',value)))
    (sb:ensure-asdf-system
     client environment "predicament-packages-intrinsic")
    (setf (clo:fdefinition
           client (sb:e1 boot)
           @clostrophilia:find-class-standard-object)
          (constantly (clo:find-class
                       client global-environment 'standard-object)))
    (clo:make-variable
     client (sb:e1 boot) @clostrophilia:*standard-object*
     (clo:find-class client global-environment 'standard-object))
    (clo:make-variable
     client (sb:e1 boot) @clostrophilia:*funcallable-standard-object*
     (clo:find-class
      client global-environment @clostrophilia:funcallable-standard-object))
    (let* ((name @clostrophilia:find-method-combination)
           (function (clo:fdefinition client global-environment name)))
      (clo:make-variable client (sb:e1 boot)
                         @sicl-clos:*standard-method-combination*
                         (funcall function client 'standard '())))
    ;; Predicament defines ERROR and WARN as generic functions, but
    ;; currently, ERROR and WARN are imported from the host, so we
    ;; need to remove them first.
    (clo:fmakunbound client global-environment 'error)
    (clo:fmakunbound client global-environment 'warn)
    (setf (clo:macro-function client global-environment 'check-type)
          (constantly nil))
    (sb:ensure-asdf-system client environment "sicl-conditions")
    (sb:ensure-asdf-system client environment "sicl-array-support")
    (sb:ensure-asdf-system client environment "sicl-array-load-time")
    (sb:ensure-asdf-system client environment "sicl-arithmetic-base")
    ;; I have no idea why this is necessary
    (let ((symbol1 @ecclesia:list-structure)
          (symbol2 (find-symbol "LIST-STRUCTURE" "ECCLESIA")))
      (setf (clo:fdefinition client global-environment symbol2)
            (clo:fdefinition client (sb:e1 boot) symbol1)))
    (sb:ensure-asdf-system client environment "sicl-type-support")
    ;; (let ((*features* '(:sicl)))
    ;;   (sb:ensure-asdf-system client environment "ctype"))
    (setf (clo:find-class client global-environment 'string)
          (find-class 'string))
    ;; The macro RESTART-CASE analyzes the REPORT expression using a
    ;; TYPECASE form which expands to a sequence of calls to TYPEP, so
    ;; we need to define TYPEP to handle those cases before we can
    ;; load Predicament.  We must not forget to replace TYPEP later,
    ;; because it is not fit to be used to determine the type of
    ;; objects in E3.
    (setf (clo:fdefinition client global-environment 'typep)
          (lambda (object type-specifier)
            (ecase type-specifier
              (null (null object))
              (string (stringp object)))))
    (sb:ensure-asdf-system client environment "predicament-common"))
  boot)
