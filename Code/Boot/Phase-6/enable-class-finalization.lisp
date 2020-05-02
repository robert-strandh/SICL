(cl:in-package #:sicl-boot-phase-6)

(defun define-effective-slot-definition-class (boot)
  (with-accessors ((e4 sicl-boot:e4)
                   (e5 sicl-boot:e5))
      boot
    (sicl-boot:with-straddled-function-definitions
        ((sicl-clos::effective-slot-definition-class-default)
         e5)
      (load-fasl "CLOS/effective-slot-definition-class-support.fasl" e4))
    (load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" e5)
    (load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" e5)))

(defun define-class-finalization (boot)
  (with-accessors ((e5 sicl-boot:e5)) boot
    (load-fasl "CLOS/class-finalization-defgenerics.fasl" e5)
    (load-fasl "CLOS/class-finalization-support.fasl" e5)
    (load-fasl "CLOS/class-finalization-defmethods.fasl" e5)))

(defun enable-class-finalization (boot)
  (define-effective-slot-definition-class boot)
  (define-class-finalization boot))
