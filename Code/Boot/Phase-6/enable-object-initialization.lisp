(cl:in-package #:sicl-boot-phase-6)

(defun enable-object-initialization (boot)
  (with-accessors ((e5 sicl-boot:e5)
                   (e6 sicl-boot:e6))
      boot
    (setf (sicl-genv:special-variable 'sicl-clos:+unbound-slot-value+ e5 t)
          10000000)
    (load-fasl "CLOS/slot-bound-using-index.fasl" e5)
    (load-fasl "CLOS/standard-instance-access.fasl" e5)
    ;; We use the non-generic version of (SETF
    ;; SLOT-VALUE-USING-CLASS), because the generic function takes an
    ;; object as its second argument and the class of that object as
    ;; its first argument.  Therefore the two arguments clash.  They
    ;; can't both be bridge objects, and they can't both be ersatz
    ;; objects.
    (load-fasl "CLOS/slot-value-etc-support.fasl" e5)
    (load-fasl "CLOS/slot-value-etc-defuns.fasl" e5)
    (load-fasl "CLOS/instance-slots-offset-defconstant.fasl" e5)
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::shared-initialize-default-using-class)
         e6)
      (load-fasl "CLOS/shared-initialize-support.fasl" e5))
    (untrace)
    (load-fasl "CLOS/shared-initialize-defgenerics.fasl" e6)
    (load-fasl "CLOS/shared-initialize-defmethods.fasl" e6)
    (load-fasl "CLOS/initialize-instance-support.fasl" e6)
    (load-fasl "CLOS/initialize-instance-defgenerics.fasl" e6)
    (load-fasl "CLOS/initialize-instance-defmethods.fasl" e6)
    (sicl-boot:define-make-instance e5 e6)))
