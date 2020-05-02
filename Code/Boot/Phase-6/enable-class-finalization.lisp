(cl:in-package #:sicl-boot-phase-6)

(defun define-effective-slot-definition-class (ea eb)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::effective-slot-definition-class-default)
       eb)
    (load-fasl "CLOS/effective-slot-definition-class-support.fasl" ea))
  (load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" eb)
  (load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" eb))

(defun define-class-finalization (eb)
  (load-fasl "CLOS/class-finalization-defgenerics.fasl" eb)
  (load-fasl "CLOS/class-finalization-support.fasl" eb)
  (load-fasl "CLOS/class-finalization-defmethods.fasl" eb))

(defun enable-class-finalization (ea eb)
  (define-effective-slot-definition-class ea eb)
  (define-class-finalization eb))
