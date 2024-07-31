(cl:in-package #:sicl-new-boot-phase-4)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun boot (boot)
  (format *trace-output* "**************** Phase 4~%")
  (let* (#+sbcl(sb-ext:*evaluator-mode* :interpret)
         (client (make-instance 'client))
         (environment (create-environment client))
         (global-environment
           (trucler:global-environment client environment))
         (env:*client* client)
         (env:*environment* global-environment))
    (setf (sb:e4 boot) global-environment)
    (reinitialize-instance client
      :environment global-environment)
    (clo:make-variable
     client global-environment '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client global-environment)
    (sb:define-backquote-macros client global-environment)
    (import-from-host client global-environment)
    (setf (clo:fdefinition client global-environment 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition
                        client global-environment function-or-name))
                   arguments)))
    (sb:import-khazern client global-environment)
    (sb:fill-environment client global-environment)
    (sb:define-client-and-environment-variables client global-environment)
    (sb:define-environment-functions client global-environment)
    (sb:define-clostrophilia-find-method-combination-template
        client global-environment)
    (sb:ensure-asdf-system client environment "clostrophilia-slot-value-etc")
    (sb:define-straddle-functions client global-environment (sb:e3 boot))
    (sb:ensure-asdf-system client environment "sicl-clos-ensure-metaobject")
    (setf (clo:fdefinition
           client (sb:e3 boot) @clostrophilia:ensure-generic-function+1)
          (clo:fdefinition
           client global-environment 'ensure-generic-function))
    (sb:define-ecclesia-functions client (sb:e2 boot) global-environment)
    (sb:ensure-asdf-system
     client environment "clostrophilia-method-combination")
    (setf (clo:fdefinition
           client (sb:e3 boot) @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class client global-environment 't)))
    (sb:ensure-asdf-system
     client environment "clostrophilia-class-hierarchy")
    (setf (clo:symbol-value client (sb:e3 boot) @clostrophilia:*class-t+1*)
          (clo:find-class client global-environment 't))
    (setf (clo:macro-function
           client global-environment @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function
           client global-environment @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     client environment "predicament-base" :load-system-file t)
    (sb:ensure-asdf-system
     client environment "predicament-packages-intrinsic")
    (setf (clo:fdefinition
           client (sb:e2 boot)
           @clostrophilia:find-class-standard-object)
          (constantly (clo:find-class
                       client global-environment 'standard-object)))
    (clo:make-variable
     client (sb:e3 boot) @clostrophilia:*standard-object*
     (clo:find-class client global-environment 'standard-object))
    (clo:make-variable
     client (sb:e3 boot) @clostrophilia:*funcallable-standard-object*
     (clo:find-class
      client global-environment @clostrophilia:funcallable-standard-object))
    (let* ((name @clostrophilia:find-method-combination)
           (function (clo:fdefinition client global-environment name)))
      (clo:make-variable client (sb:e3 boot)
                         @sicl-clos:*standard-method-combination*
                         (funcall function client 'standard '())))
    (sb:ensure-asdf-system
     client environment "sicl-new-boot-phase-2-additional-classes")
    (define-class-of-and-stamp client (sb:e3 boot) global-environment)
    (setf (clo:fdefinition client (sb:e3 boot) @clostrophilia:class-of+1)
          (clo:fdefinition client global-environment 'class-of))
    (setf (clo:fdefinition client (sb:e3 boot) @clostrophilia:stamp+1)
          (clo:fdefinition client global-environment @clostrophilia:stamp))
    (load-predicament client environment global-environment)
    (clo:make-variable client (sb:e3 boot)
                       @predicament:*condition-maker* 'make-condition)
    (let ((symbol @clostrophilia:standard-instance-access))
      (setf (clo:fdefinition client global-environment symbol)
            #'sb:standard-instance-access)
      (setf (clo:fdefinition client global-environment `(setf ,symbol))
            #'(setf sb:standard-instance-access)))
    (sb:ensure-asdf-system
     client environment "clostrophilia-slot-value-etc-using-class")
    ;;; During bootstrapping, we set the unbound slot value to
    ;;; something that is easier to manipulate during debugging.
    (setf (clo:symbol-value
           client global-environment @clostrophilia:+unbound-slot-value+)
          99999)
    (sb:ensure-asdf-system
     client environment "clostrophilia-standard-object-initialization")
    (setf (clo:fdefinition
           client (sb:e3 boot) @sicl-clos:initialize-instance+1)
          (clo:fdefinition client global-environment 'initialize-instance))
    (sb:ensure-asdf-system
     client environment "clostrophilia-standard-object-initialization-aux")
    (let ((symbol-class (clo:find-class client global-environment 'symbol))
          (finalize-inheritance
            (clo:fdefinition
             client (sb:e3 boot) @clostrophilia:finalize-inheritance)))
      (funcall finalize-inheritance symbol-class))
    (setf (clo:fdefinition
           client global-environment @clostrophilia:shared-initialize-aux-1)
          (clo:fdefinition
           client (sb:e3 boot) @clostrophilia:shared-initialize-aux))
    ;; ctype uses ASSERT and ASSERT expands to RESTART-CASE which
    ;; contains a TYPECASE which expands to TYPEP, but we don't have
    ;; TYPEP since the very purpose of ctype is to define TYPEP.  But
    ;; assertions should not fail at this point in the process anyway.
    (setf (clo:macro-function client global-environment 'assert)
          (lambda (form environment)
            ;; We might put some trace output here.
            (declare (ignore form environment))
            nil))
    ;; FIXME: TYPEXPAND should be defined by code from SICL-TYPE being
    ;; loaded, rather than by defining it here.
    (setf (clo:fdefinition client global-environment @sicl-type:typexpand)
          (lambda (type-specifier &optional (environment global-environment))
            (clo:type-expand client environment type-specifier)))
    (setf (clo:fdefinition
           client global-environment @sicl-clos:intern-eql-specializer-1)
          (clo:fdefinition
           client (sb:e3 boot) @sicl-clos:intern-eql-specializer))
    (sb:with-intercepted-function-cells
        ((make-instance
             (clo:ensure-operator-cell client (sb:e3 boot) 'make-instance)))
      (sb:ensure-asdf-system
       client environment "clostrophilia-class-finalization")
      (sb:ensure-asdf-system
       client environment "clostrophilia-method-combination-base"))
    (setf (clo:fdefinition client (sb:e3 boot) @sicl-clos:find-class+1)
          (clo:fdefinition client global-environment 'find-class))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e3 boot) 'make-instance)))
      (sb:ensure-asdf-system
       client environment "clostrophilia-generic-function-invocation")
      (sb:ensure-asdf-system client environment "acclimation")
      (sb:ensure-asdf-system client environment "ecclesia"))
    (sb:ensure-asdf-system
     client environment "clostrophilia-dependent-maintenance")
    (setf (clo:fdefinition
           client global-environment @clostrophilia:subtypep-1)
          (constantly t))
    (setf (clo:fdefinition
           client global-environment @sicl-clos:subtypep-1)
          (constantly t))
    (sb:ensure-asdf-system
     client environment "clostrophilia-generic-function-initialization")
    (setf (clo:fdefinition
           client global-environment @clostrophilia:allocate-general-instance)
          #'sb:allocate-general-instance))
  boot)
