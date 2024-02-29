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
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy"))
  boot)
