(cl:in-package #:sicl-new-boot-phase-3)

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols c3))

(defun boot (boot)
  (format *trace-output* "**************** Phase 3~%")
  (let* ((c3 (make-instance 'client))
         (w3 (create-environment c3))
         (e3
           (trucler:global-environment c3 w3))
         (env:*client* c3)
         (env:*environment* e3))
    (setf (sb:e3 boot) e3)
    (reinitialize-instance c3
      :environment e3)
    (clo:make-variable
     c3 e3 '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions c3 e3)
    (sb:define-backquote-macros c3 e3)
    (import-from-host c3 e3)
    (setf (clo:fdefinition c3 e3 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition
                        c3 e3 function-or-name))
                   arguments)))
    (sb:import-khazern c3 e3)
    (sb:fill-environment c3 e3)
    (sb:define-client-and-environment-variables c3 e3)
    (sb:define-environment-functions c3 e3)
    (sb:define-clostrophilia-find-method-combination-template
        c3 e3)
    ;;; FIXME: Define these functions by loading SICL-specific code
    (setf (clo:fdefinition
           c3 e3 @clostrophilia:small-integer=)
          #'=)
    (setf (clo:fdefinition
           c3 e3 @clostrophilia:small-integer<)
          #'<)
    (let ((symbol @clostrophilia:standard-instance-access))
      (setf (clo:fdefinition c3 e3 symbol)
            #'sb:standard-instance-access)
      (setf (clo:fdefinition c3 e3 `(setf ,symbol))
            #'(setf sb:standard-instance-access)))
    (sb:ensure-asdf-system c3 w3 "sicl-primop")
    (setf (clo:fdefinition c3 e3 @sicl-primop:primop)
          #'sb:primop)
    (sb:with-intercepted-function-names
        (list (cons @clostrophilia:slot-boundp-using-only-class
                    @clostrophilia:slot-boundp-using-only-class-1)
              (cons @clostrophilia:slot-value-using-only-class
                    @clostrophilia:slot-value-using-only-class-1)
              (cons (list 'setf @clostrophilia:slot-boundp-using-only-class)
                    (list 'setf @clostrophilia:slot-boundp-using-only-class-1))
              (cons @clostrophilia:slot-makunbound-using-only-class
                    @clostrophilia:slot-makunbound-using-only-class-1))
      (sb:ensure-asdf-system c3 w3 "clostrophilia-slot-value-etc"))
    (sb:define-straddle-functions c3 e3 (sb:e2 boot))
    (sb:ensure-asdf-system c3 w3 "sicl-clos-ensure-metaobject")
    (setf (clo:fdefinition
           c3 (sb:e2 boot) @clostrophilia:ensure-generic-function+1)
          (clo:fdefinition
           c3 e3 'ensure-generic-function))
    (sb:define-ecclesia-functions c3 (sb:e1 boot) e3)
    (sb:with-intercepted-function-names
        (list (cons @clostrophilia:ensure-method-combination
                    @clostrophilia:^ensure-method-combination))
      (sb:ensure-asdf-system
       c3 w3 "clostrophilia-method-combination"))
    (setf (clo:fdefinition
           c3 (sb:e2 boot) @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class c3 e3 't)))
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-class-hierarchy")
    (sb:ensure-asdf-system c3 w3 "sicl-arithmetic-base")
    (sb:ensure-asdf-system c3 w3 "sicl-arithmetic-class-hierarchy")
    (sb:ensure-asdf-system c3 w3 "sicl-arithmetic-operations") 
    (setf (clo:symbol-value c3 (sb:e2 boot) @clostrophilia:*class-t+1*)
          (clo:find-class c3 e3 't))
        (setf (clo:macro-function
           c3 e3 @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function
           c3 e3 @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     c3 w3 "predicament-base" :load-system-file t)
        (sb:ensure-asdf-system
     c3 w3 "predicament-packages-intrinsic")
    (setf (clo:fdefinition
           c3 (sb:e1 boot)
           @clostrophilia:find-class-standard-object)
          (constantly (clo:find-class
                       c3 e3 'standard-object)))
    (clo:make-variable
     c3 (sb:e2 boot) @clostrophilia:*standard-object*
     (clo:find-class c3 e3 'standard-object))
    (clo:make-variable
     c3 (sb:e2 boot) @clostrophilia:*funcallable-standard-object*
     (clo:find-class
      c3 e3 @clostrophilia:funcallable-standard-object))
    (let* ((name @clostrophilia:find-method-combination)
           (function (clo:fdefinition c3 e3 name)))
      (clo:make-variable c3 (sb:e2 boot)
                         @sicl-clos:*standard-method-combination*
                         (funcall function c3 'standard '())))
    (sb:ensure-asdf-system
     c3 w3 "sicl-new-boot-phase-2-additional-classes")
    (define-class-of-and-stamp c3 (sb:e2 boot) e3)
    (setf (clo:fdefinition c3 (sb:e2 boot) @clostrophilia:class-of+1)
          (clo:fdefinition c3 e3 'class-of))
    (setf (clo:fdefinition c3 (sb:e2 boot) @clostrophilia:stamp+1)
          (clo:fdefinition c3 e3 @clostrophilia:stamp))
    (load-predicament c3 w3 e3)
    (clo:make-variable c3 (sb:e2 boot)
                       @predicament:*condition-maker* 'make-condition)
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-slot-value-etc-using-class")
    ;;; During bootstrapping, we set the unbound slot value to
    ;;; something that is easier to manipulate during debugging.
    (setf (clo:symbol-value
           c3 e3 @clostrophilia:+unbound-slot-value+)
          99999)
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-standard-object-initialization")
    (setf (clo:fdefinition
           c3 (sb:e2 boot) @sicl-clos:initialize-instance+1)
          (clo:fdefinition c3 e3 'initialize-instance))
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-standard-object-initialization-aux")
    (setf (clo:fdefinition
           c3 e3 @clostrophilia:shared-initialize-aux-1)
          (clo:fdefinition
           c3 (sb:e2 boot) @clostrophilia:shared-initialize-aux))
    ;; ctype uses ASSERT and ASSERT expands to RESTART-CASE which
    ;; contains a TYPECASE which expands to TYPEP, but we don't have
    ;; TYPEP since the very purpose of ctype is to define TYPEP.  But
    ;; assertions should not fail at this point in the process anyway.
    (setf (clo:macro-function c3 e3 'assert)
          (lambda (form environment)
            ;; We might put some trace output here.
            (declare (ignore form environment))
            nil))
    ;; FIXME: TYPEXPAND should be defined by code from SICL-TYPE being
    ;; loaded, rather than by defining it here.
    (setf (clo:fdefinition c3 e3 @sicl-type:typexpand)
          (lambda (type-specifier &optional (environment e3))
            (clo:type-expand c3 environment type-specifier)))
    (setf (clo:fdefinition
           c3 e3 @sicl-clos:intern-eql-specializer-1)
          (clo:fdefinition
           c3 (sb:e2 boot) @sicl-clos:intern-eql-specializer))
    (sb:with-intercepted-function-cells
        ((make-instance
             (clo:ensure-operator-cell c3 (sb:e2 boot) 'make-instance)))
      (sb:ensure-asdf-system
       c3 w3 "clostrophilia-class-finalization")
      (sb:ensure-asdf-system
       c3 w3 "clostrophilia-method-combination-base"))
    (sb:ensure-asdf-system
     c3 w3 "sicl-new-boot-class-finalization")
    (setf (clo:fdefinition c3 e3
                           @clostrophilia:make-method-instance)
          (clo:fdefinition c3 (sb:e2 boot) 'make-instance))
    (setf (clo:fdefinition c3 e3 'compile)
          (lambda (should-be-nil lambda-expression)
            (assert (null should-be-nil))
            (let ((cst (cst:cst-from-expression lambda-expression)))
              (sb:with-intercepted-function-cells
                  ((make-instance 
                       (clo:ensure-operator-cell
                        c3 (sb:e2 boot) 'make-instance)))
                (sb:eval-cst c3 cst w3)))))
    (clo:make-variable
     c3 e3 'lambda-list-keywords lambda-list-keywords)
    (setf (clo:fdefinition c3 (sb:e2 boot) @sicl-clos:find-class+1)
          (clo:fdefinition c3 e3 'find-class))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell c3 (sb:e2 boot) 'make-instance)))
      (sb:ensure-asdf-system
       c3 w3 "clostrophilia-generic-function-invocation")
      (sb:ensure-asdf-system c3 w3 "acclimation")
      (sb:ensure-asdf-system c3 w3 "ecclesia"))
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-dependent-maintenance")
    (setf (clo:fdefinition
           c3 e3 @clostrophilia:subtypep-1)
          (constantly t))
    (setf (clo:fdefinition
           c3 e3 @sicl-clos:subtypep-1)
          (constantly t))
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-generic-function-initialization")
    (setf (clo:fdefinition
           c3 e3 @clostrophilia:allocate-general-instance)
          #'sb:allocate-general-instance)
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell c3 (sb:e2 boot) 'make-instance)))
      (sb:ensure-asdf-system
       c3 w3 "clostrophilia-class-initialization"))
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-method-initialization")
    (sb:ensure-asdf-system
     c3 w3 "clostrophilia-slot-definition-initialization")
    ;; We don't expect to see any floating-point numbers during
    ;; bootstrapping.
    (setf (clo:fdefinition
           c3 e3 @sicl-arithmetic:single-float-p)
          (constantly nil))
    (setf (clo:fdefinition
           c3 e3 @sicl-arithmetic:double-float-p)
          (constantly nil))
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell c3 (sb:e2 boot) 'make-instance))
         (class-name
          (clo:ensure-operator-cell c3 (sb:e2 boot) 'class-name)))
      (load-ctype c3 w3 e3))
    ;; The ctype library defines SUBCLASSP to call
    ;; SICL-CLOS:CLASS-PRECEDENCE-LIST with the subclass as an
    ;; argument.  But in E3, the arguments to SUBCLASSP are ersatz
    ;; objects, so they need to be accessed using generic functions
    ;; located in E2.  So we redefine SUBCLASSP here.
    (setf (clo:fdefinition c3 e3 @ctype:subclassp)
          (lambda (sub super)
            (let ((class-precedence-list
                    (clo:fdefinition
                     c3 (sb:e2 boot) @sicl-clos:class-precedence-list)))
              (member super (funcall class-precedence-list sub)))))
    (sb:with-intercepted-function-cells
        ((make-instance
             (clo:ensure-operator-cell c3 (sb:e2 boot) 'make-instance)))
      (sb:ensure-asdf-system
       c3 w3 "sicl-clos-ensure-metaobject-using"))
    (setf (clo:fdefinition c3 e3
                           @clostrophilia:set-funcallable-instance-function)
          (fdefinition 'closer-mop:set-funcallable-instance-function))
    (sb:ensure-asdf-system c3 w3 "sicl-clos-make-instance"))
  boot)
