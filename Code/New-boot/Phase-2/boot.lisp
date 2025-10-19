(cl:in-package #:sicl-new-boot-phase-2)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols client))

(defun boot (boot)
  (format *trace-output* "**************** Phase 2~%")
  (let* ((client (make-instance 'client))
         (w2 (create-environment client))
         (e2
           (trucler:global-environment client w2))
         (env:*client* client)
         (env:*environment* e2))
    (setf (sb:e2 boot) e2)
    (reinitialize-instance client
      :environment e2)
    ;; We saved the SICL class named T under the name sb::sicl-t.  Now
    ;; we need to put it back as class T in E1, because it is going to
    ;; be used as a specializer in bridge generic functions.
    (setf (clo:find-class client (sb:e1 boot) 't)
          (clo:find-class client (sb:e1 boot) 'sb::sicl-t))
    (clo:make-variable
     client e2 '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions client e2)
    (sb:define-backquote-macros client e2)
    (import-from-host client e2)
    (setf (clo:fdefinition client e2 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition
                        client e2 function-or-name))
                   arguments)))
    (sb:import-khazern client e2)
    (sb:fill-environment client e2)
    (sb:define-client-and-environment-variables client e2)
    (sb:define-environment-functions client e2)
    (setf (clo:fdefinition client (sb:e1 boot) @sicl-clos:find-class+1)
          (clo:fdefinition client e2 'find-class))
    ;;; FIXME: Define these functions by loading SICL-specific code
    (setf (clo:fdefinition
           client e2 @clostrophilia:small-integer=)
          #'=)
    (setf (clo:fdefinition
           client e2 @clostrophilia:small-integer<)
          #'<)
    (sb:define-clostrophilia-find-method-combination-template
        client e2)
    (let ((symbol @clostrophilia:standard-instance-access))
      (setf (clo:fdefinition client e2 symbol)
            #'sb:standard-instance-access)
      (setf (clo:fdefinition client e2 `(setf ,symbol))
            #'(setf sb:standard-instance-access)))
    (sb:ensure-asdf-system client w2 "sicl-primop")
    (setf (clo:fdefinition client e2 @sicl-primop:primop)
          #'sb:primop)
    (sb:ensure-asdf-system client w2 "clostrophilia-slot-value-etc")
    (sb:define-straddle-functions client e2 (sb:e1 boot))
    (sb:ensure-asdf-system client w2 "sicl-clos-ensure-metaobject")
    (sb:define-ecclesia-functions client (sb:e1 boot) e2)
    (sb:with-intercepted-function-names
        (list (cons @clostrophilia:ensure-method-combination
                    @clostrophilia:^ensure-method-combination))
      (sb:ensure-asdf-system
       client w2 "clostrophilia-method-combination"))
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
    (setf (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:ensure-generic-function+1)
          (clo:fdefinition
           client e2 'ensure-generic-function))
    (setf (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class client e2 't)))
    ;; ADD-DIRECT-METHOD is called by ADD-METHOD, but it doesn't make
    ;; sense to add a SICL method to a host class.
    (setf (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:add-direct-method)
          (constantly nil))
    (sb:ensure-asdf-system
     client w2 "clostrophilia-class-hierarchy")
    (sb:ensure-asdf-system client w2 "sicl-arithmetic-base")
    (sb:ensure-asdf-system client w2 "sicl-arithmetic-class-hierarchy")
    (sb:ensure-asdf-system client w2 "sicl-arithmetic-operations") 
    (setf (clo:symbol-value client (sb:e1 boot) @clostrophilia:*class-t+1*)
          (clo:find-class client e2 't))
    (sb:ensure-asdf-system
     client w2 "sicl-asdf-packages")
    (setf (clo:macro-function
           client e2 @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function
           client e2 @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     client w2 "predicament-base" :load-system-file t)
    ;; The system predicament base contains the definition of the
    ;; variable PREDICAMENT-ASDF:*STRING-DESIGNATORS*, so when we
    ;; loaded that system into E2, that variable got defined in E2.
    ;; However, we now want to load a package definition into E2 that
    ;; uses the value of that variable at read time.  But read time is
    ;; done by Eclector in the host envronment, and that variable is
    ;; not defined in the host environment.  So we do that with the
    ;; following code.
    (let* ((symbol @predicament-asdf:*string-designators*)
           (value (clo:symbol-value client e2 symbol)))
      (eval `(defparameter ,symbol ',value)))
    (sb:ensure-asdf-system
     client w2 "predicament-packages-intrinsic")
    (setf (clo:fdefinition
           client (sb:e1 boot)
           @clostrophilia:find-class-standard-object)
          (constantly (clo:find-class
                       client e2 'standard-object)))
    (clo:make-variable
     client (sb:e1 boot) @clostrophilia:*standard-object*
     (clo:find-class client e2 'standard-object))
    (clo:make-variable
     client (sb:e1 boot) @clostrophilia:*funcallable-standard-object*
     (clo:find-class
      client e2 @clostrophilia:funcallable-standard-object))
    (let* ((name @clostrophilia:find-method-combination)
           (function (clo:fdefinition client e2 name)))
      (clo:make-variable client (sb:e1 boot)
                         @sicl-clos:*standard-method-combination*
                         (funcall function client 'standard '())))
    (sb:ensure-asdf-system
     client w2 "sicl-new-boot-phase-2-additional-classes")
    (define-class-of-and-stamp client (sb:e1 boot) e2)
    (setf (clo:fdefinition client (sb:e1 boot) @clostrophilia:class-of+1)
          (clo:fdefinition client e2 'class-of))
    (setf (clo:fdefinition client (sb:e1 boot) @clostrophilia:stamp+1)
          (clo:fdefinition client e2 @clostrophilia:stamp))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e1 boot) 'make-instance)))
      (load-predicament client w2 e2))
    (clo:make-variable client (sb:e1 boot)
                       @predicament:*condition-maker* 'make-condition)
    (sb:ensure-asdf-system
     client w2 "clostrophilia-slot-value-etc-using-class")
    ;;; During bootstrapping, we set the unbound slot value to
    ;;; something that is easier to manipulate during debugging.
    (setf (clo:symbol-value
           client e2 @clostrophilia:+unbound-slot-value+)
          99999)
    (sb:ensure-asdf-system
     client w2 "clostrophilia-standard-object-initialization")
    (setf (clo:fdefinition
           client (sb:e1 boot) @sicl-clos:initialize-instance+1)
          (clo:fdefinition client e2 'initialize-instance))
    (sb:ensure-asdf-system
     client w2 "clostrophilia-standard-object-initialization-aux")
    (setf (clo:fdefinition
           client e2 @clostrophilia:shared-initialize-aux-1)
          (clo:fdefinition
           client (sb:e1 boot) @clostrophilia:shared-initialize-aux))
    ;; ctype uses ASSERT and ASSERT expands to RESTART-CASE which
    ;; contains a TYPECASE which expands to TYPEP, but we don't have
    ;; TYPEP since the very purpose of ctype is to define TYPEP.  But
    ;; assertions should not fail at this point in the process anyway.
    (setf (clo:macro-function client e2 'assert)
          (lambda (form environment)
            ;; We might put some trace output here.
            (declare (ignore form environment))
            nil))
    ;; FIXME: TYPEXPAND should be defined by code from SICL-TYPE being
    ;; loaded, rather than by defining it here. 
    (setf (clo:fdefinition client e2 @sicl-type:typexpand)
          (lambda (type-specifier &optional (environment e2))
            (clo:type-expand client environment type-specifier)))
    (setf (clo:fdefinition
           client e2 @sicl-clos:intern-eql-specializer-1)
          (clo:fdefinition
           client (sb:e1 boot) @sicl-clos:intern-eql-specializer))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e1 boot) 'make-instance)))
      (sb:ensure-asdf-system
       client w2 "clostrophilia-class-finalization")
      (sb:ensure-asdf-system
       client w2 "clostrophilia-method-combination-base"))
    (sb:ensure-asdf-system
     client w2 "sicl-new-boot-class-finalization")
    (setf (clo:fdefinition client e2
                           @clostrophilia:make-method-instance)
          (clo:fdefinition client (sb:e1 boot) 'make-instance))
    (setf (clo:fdefinition client e2 'compile)
          (lambda (should-be-nil lambda-expression)
            (assert (null should-be-nil))
            (let ((cst (cst:cst-from-expression lambda-expression)))
              (sb:with-intercepted-function-cells
                  ((make-instance 
                       (clo:ensure-operator-cell
                        client (sb:e1 boot) 'make-instance)))
                (sb:eval-cst client cst w2)))))
    (clo:make-variable
     client e2 'lambda-list-keywords lambda-list-keywords)
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e1 boot) 'make-instance)))
      (sb:ensure-asdf-system
       client w2 "clostrophilia-generic-function-invocation")
      (sb:ensure-asdf-system client w2 "acclimation")
      (sb:ensure-asdf-system client w2 "ecclesia"))
    (sb:ensure-asdf-system
     client w2 "clostrophilia-dependent-maintenance")
    (setf (clo:fdefinition
           client e2 @clostrophilia:subtypep-1)
          (constantly t))
    (setf (clo:fdefinition
           client e2 @sicl-clos:subtypep-1)
          (constantly t))
    (sb:ensure-asdf-system
     client w2 "clostrophilia-generic-function-initialization")
    (setf (clo:fdefinition
           client e2 @clostrophilia:allocate-general-instance)
          #'sb:allocate-general-instance)
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e1 boot) 'make-instance)))
      (sb:ensure-asdf-system
       client w2 "clostrophilia-class-initialization"))
    (sb:ensure-asdf-system
     client w2 "clostrophilia-method-initialization")
    (sb:ensure-asdf-system
     client w2 "clostrophilia-slot-definition-initialization")
    ;; We don't expect to see any floating-point numbers during
    ;; bootstrapping.
    (setf (clo:fdefinition
           client e2 @sicl-arithmetic:single-float-p)
          (constantly nil))
    (setf (clo:fdefinition
           client e2 @sicl-arithmetic:double-float-p)
          (constantly nil))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e1 boot) 'make-instance))
         (class-name
          (clo:ensure-operator-cell client (sb:e1 boot) 'class-name)))
      (load-ctype client w2 e2))
    ;; The ctype library defines SUBCLASSP to call
    ;; SICL-CLOS:CLASS-PRECEDENCE-LIST with the subclass as an
    ;; argument.  But in E2, the arguments to SUBCLASSP are bridge
    ;; objects, so they need to be accessed using host generic
    ;; functions located in E1.  So we redefine SUBCLASSP here.
    (setf (clo:fdefinition client e2 @ctype:subclassp)
          (lambda (sub super)
            (let ((class-precedence-list
                    (clo:fdefinition
                     client (sb:e1 boot) @sicl-clos:class-precedence-list)))
              (member super (funcall class-precedence-list sub)))))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell client (sb:e1 boot) 'make-instance)))
      (sb:ensure-asdf-system
       client w2 "sicl-clos-ensure-metaobject-using"))
    (setf (clo:fdefinition client e2
                           @clostrophilia:set-funcallable-instance-function)
          (fdefinition 'closer-mop:set-funcallable-instance-function))
    (sb:ensure-asdf-system client w2 "sicl-clos-make-instance"))
  boot)
