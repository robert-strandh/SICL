(cl:in-package #:sicl-new-boot-phase-4)

;;; In phases 1, 2, and 3, we allowed for cells from an environment E'
;;; (or completely constructed cells) to be inserted into a different
;;; environment E through the use of the macro
;;; WITH-INTERCEPTED-FUNCTION-CELLS.  In phase 4, we can no longer
;;; allow this, because environment E4 is supposed to be the model for
;;; the main environment in the final system.

(eval-when (:compile-toplevel) (sb:enable-parcl-symbols c4))

(defun boot (boot)
  (format *trace-output* "**************** Phase 4~%")
  (let* ((c4 (make-instance 'client))
         (environment (create-environment c4))
         (e4 (trucler:global-environment c4 environment))
         (e3 (sb:e3 boot))
         (env:*client* c4)
         (env:*environment* e4))
    (setf (sb:e4 boot) e4)
    (reinitialize-instance c4 :environment e4)
    (clo:make-variable c4 e4 '*package* (find-package '#:common-lisp-user))
    (sb:define-package-functions c4 e4)
    (sb:define-backquote-macros c4 e4)
    (import-from-host c4 e4)
    (setf (clo:fdefinition c4 e4 'funcall)
          (lambda (function-or-name &rest arguments)
            (apply #'funcall
                   (if (functionp function-or-name)
                       function-or-name
                       (clo:fdefinition c4 e4 function-or-name))
                   arguments)))
    (sb:import-khazern c4 e4)
    (sb:fill-environment c4 e4)
    (sb:define-client-and-environment-variables c4 e4)
    (sb:define-environment-functions c4 e4)
    (sb:define-clostrophilia-find-method-combination-template c4 e4)
    ;;; FIXME: Define these functions by loading SICL-specific code
    (setf (clo:fdefinition c4 e4 @clostrophilia:small-integer=)
          #'=)
    (setf (clo:fdefinition c4 e4 @clostrophilia:small-integer<)
          #'<)
    (let ((symbol @clostrophilia:standard-instance-access))
      (setf (clo:fdefinition c4 e4 symbol)
            #'sb:standard-instance-access)
      (setf (clo:fdefinition c4 e4 `(setf ,symbol))
            #'(setf sb:standard-instance-access)))
    (sb:ensure-asdf-system c4 environment "sicl-primop")
    (setf (clo:fdefinition c4 e4 @sicl-primop:primop)
          #'sb:primop)
    (sb:ensure-asdf-system c4 environment "clostrophilia-slot-value-etc")
    (sb:define-straddle-functions c4 e4 e3)
    (sb:ensure-asdf-system c4 environment "sicl-clos-ensure-metaobject")
    (setf (clo:fdefinition
           c4 e3 @clostrophilia:ensure-generic-function+1)
          (clo:fdefinition
           c4 e4 'ensure-generic-function))
    (sb:define-ecclesia-functions c4 (sb:e2 boot) e4)
    (sb:ensure-asdf-system c4 environment "clostrophilia-method-combination")
    (setf (clo:fdefinition c4 e3 @clostrophilia:find-class-t)
          (lambda ()
            (clo:find-class c4 e4 't)))
    (sb:ensure-asdf-system c4 environment "clostrophilia-class-hierarchy")
    (sb:ensure-asdf-system c4 environment "sicl-arithmetic-base")
    (sb:ensure-asdf-system c4 environment
                           "sicl-arithmetic-class-hierarchy")
    (sb:ensure-asdf-system c4 environment "sicl-arithmetic-operations") 
    (setf (clo:symbol-value c4 e3 @clostrophilia:*class-t+1*)
          (clo:find-class c4 e4 't))
    (setf (clo:macro-function c4 e4 @asdf-user:defsystem)
          (constantly nil))
    (setf (clo:macro-function c4 e4 @asdf:defsystem)
          (constantly nil))
    (sb:ensure-asdf-system
     c4 environment "predicament-base" :load-system-file t)
    (sb:ensure-asdf-system c4 environment "predicament-packages-intrinsic")
    (setf (clo:fdefinition
           c4 (sb:e2 boot)
           @clostrophilia:find-class-standard-object)
          (constantly (clo:find-class c4 e4 'standard-object)))
    (clo:make-variable
     c4 e3 @clostrophilia:*standard-object*
     (clo:find-class c4 e4 'standard-object))
    (clo:make-variable
     c4 e3 @clostrophilia:*funcallable-standard-object*
     (clo:find-class c4 e4 @clostrophilia:funcallable-standard-object))
    (let* ((name @clostrophilia:find-method-combination)
           (function (clo:fdefinition c4 e4 name)))
      (clo:make-variable c4 e3
                         @sicl-clos:*standard-method-combination*
                         (funcall function c4 'standard '())))
    (sb:ensure-asdf-system
     c4 environment "sicl-new-boot-phase-2-additional-classes")
    (define-class-of-and-stamp c4 e3 e4)
    (setf (clo:fdefinition c4 e3 @clostrophilia:class-of+1)
          (clo:fdefinition c4 e4 'class-of))
    (setf (clo:fdefinition c4 e3 @clostrophilia:stamp+1)
          (clo:fdefinition c4 e4 @clostrophilia:stamp))
    (load-predicament c4 environment e4)
    (clo:make-variable c4 e3
                       @predicament:*condition-maker* 'make-condition)
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-slot-value-etc-using-class")
    ;;; During bootstrapping, we set the unbound slot value to
    ;;; something that is easier to manipulate during debugging.
    (setf (clo:symbol-value c4 e4 @clostrophilia:+unbound-slot-value+)
          99999)
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-standard-object-initialization")
    (setf (clo:fdefinition c4 e3 @sicl-clos:initialize-instance+1)
          (clo:fdefinition c4 e4 'initialize-instance))
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-standard-object-initialization-aux")
    (setf (clo:fdefinition c4 e4 @clostrophilia:shared-initialize-aux-1)
          (clo:fdefinition
           c4 e3 @clostrophilia:shared-initialize-aux))
    ;; ctype uses ASSERT and ASSERT expands to RESTART-CASE which
    ;; contains a TYPECASE which expands to TYPEP, but we don't have
    ;; TYPEP since the very purpose of ctype is to define TYPEP.  But
    ;; assertions should not fail at this point in the process anyway.
    (setf (clo:macro-function c4 e4 'assert)
          (lambda (form environment)
            ;; We might put some trace output here.
            (declare (ignore form environment))
            nil))
    ;; FIXME: TYPEXPAND should be defined by code from SICL-TYPE being
    ;; loaded, rather than by defining it here.
    (setf (clo:fdefinition c4 e4 @sicl-type:typexpand)
          (lambda (type-specifier &optional (environment e4))
            (clo:type-expand c4 environment type-specifier)))
    (setf (clo:fdefinition c4 e4 @sicl-clos:intern-eql-specializer-1)
          (clo:fdefinition
           c4 e3 @sicl-clos:intern-eql-specializer))
    (sb:with-intercepted-function-cells
        ((make-instance
             (clo:ensure-operator-cell c4 e3 'make-instance)))
      (sb:ensure-asdf-system
       c4 environment "clostrophilia-class-finalization")
      (sb:ensure-asdf-system
       c4 environment "clostrophilia-method-combination-base"))
    (setf (clo:fdefinition c4 e3 @sicl-clos:find-class+1)
          (clo:fdefinition c4 e4 'find-class))
    (setf (clo:fdefinition c4 e4 'compile)
          (lambda (should-be-nil lambda-expression)
            (assert (null should-be-nil))
            (let ((cst (cst:cst-from-expression lambda-expression)))
              (sb:eval-cst c4 cst environment))))
    (clo:make-variable c4 e4 'lambda-list-keywords lambda-list-keywords)
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell c4 e3 'make-instance)))
      (sb:ensure-asdf-system
       c4 environment "clostrophilia-generic-function-invocation")
      (sb:ensure-asdf-system c4 environment "acclimation")
      (sb:ensure-asdf-system c4 environment "ecclesia"))
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-dependent-maintenance")
    (setf (clo:fdefinition c4 e4 @clostrophilia:subtypep-1)
          (constantly t))
    (setf (clo:fdefinition c4 e4 @sicl-clos:subtypep-1)
          (constantly t))
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-generic-function-initialization")
    (setf (clo:fdefinition c4 e4 @clostrophilia:allocate-general-instance)
          #'sb:allocate-general-instance)
    (sb:with-intercepted-function-cells
        ((make-instance
          (clo:ensure-operator-cell c4 e3 'make-instance)))
      (sb:ensure-asdf-system
       c4 environment "clostrophilia-class-initialization"))
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-method-initialization")
    (sb:ensure-asdf-system
     c4 environment "clostrophilia-slot-definition-initialization")
    ;; We don't expect to see any floating-point numbers during
    ;; bootstrapping.
    (setf (clo:fdefinition c4 e4 @sicl-arithmetic:single-float-p)
          (constantly nil))
    (setf (clo:fdefinition c4 e4 @sicl-arithmetic:double-float-p)
          (constantly nil))
    ;; The ctype library is unusual in that it creates instances of
    ;; its classes at load time.  For that to work, we need to use the
    ;; version of MAKE-INSTANCE in E3.  But we want to use the cell
    ;; from E4 so that we can load the final version of MAKE-INSTANCE
    ;; into it later.  We solve this dilemma by temporarily setting
    ;; MAKE-INSTANCE in E4 to be the same as MAKE-INSTANCE in E3, and
    ;; then we remove that definition after ctype has been loaded.
    (setf (clo:fdefinition c4 (sb:e4 boot) 'make-instance)
          (clo:fdefinition c4 e3 'make-instance))
    (load-ctype c4 environment e4)
    (clo:fmakunbound c4 (sb:e4 boot) 'make-instance)
    ;; The ctype library defines SUBCLASSP to call
    ;; SICL-CLOS:CLASS-PRECEDENCE-LIST with the subclass as an
    ;; argument.  But in E4, the arguments to SUBCLASSP are ersatz
    ;; objects, and they need to be accessed using generic functions
    ;; located in E3.  So we redefine SUBCLASSP here.
    (setf (clo:fdefinition c4 e4 @ctype:subclassp)
          (lambda (sub super)
            (let ((class-precedence-list
                    (clo:fdefinition
                     c4 e3 @sicl-clos:class-precedence-list)))
              (member super (funcall class-precedence-list sub)))))
    (sb:ensure-asdf-system
     c4 environment "sicl-clos-ensure-metaobject-using")
    (setf (clo:fdefinition c4 e4
                           @clostrophilia:set-funcallable-instance-function)
          (fdefinition 'closer-mop:set-funcallable-instance-function))
    (sb:ensure-asdf-system c4 environment "sicl-clos-make-instance"))
  boot)
