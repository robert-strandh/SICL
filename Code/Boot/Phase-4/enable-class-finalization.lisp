(cl:in-package #:sicl-boot-phase-4)

(defun define-effective-slot-definition-class (boot)
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    (setf (sicl-genv:fdefinition
           'sicl-clos::effective-slot-definition-class-default e3)
          (lambda (class &rest initargs)
            (declare (ignore class initargs))
            (sicl-genv:find-class
             'sicl-clos:standard-effective-slot-definition e2)))
    (load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" e3)
    (load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" e3)))

(defun define-class-finalization (boot)
  (with-accessors ((e3 sicl-boot:e3)) boot
    (load-fasl "CLOS/class-finalization-defgenerics.fasl" e3)
    (load-fasl "CLOS/class-finalization-support.fasl" e3)
    (load-fasl "CLOS/class-finalization-defmethods.fasl" e3)))

(defun enable-class-finalization (boot)
  (define-effective-slot-definition-class boot)
  (define-class-finalization boot))
