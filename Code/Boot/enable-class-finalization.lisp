(cl:in-package #:sicl-boot)

(defun define-effective-slot-definition-class (load-fasl ea eb)
  (sicl-boot:with-straddled-function-definitions
      ((sicl-clos::effective-slot-definition-class-default)
       eb)
    (funcall load-fasl "CLOS/effective-slot-definition-class-support.fasl" ea))
  (funcall load-fasl "CLOS/effective-slot-definition-class-defgeneric.fasl" eb)
  (funcall load-fasl "CLOS/effective-slot-definition-class-defmethods.fasl" eb))

(defun define-class-finalization (load-fasl eb)
  (funcall load-fasl "CLOS/class-finalization-defgenerics.fasl" eb)
  (funcall load-fasl "CLOS/class-finalization-support.fasl" eb)
  (funcall load-fasl "CLOS/class-finalization-defmethods.fasl" eb))

(defun enable-class-finalization (load-fasl ea eb)
  (define-effective-slot-definition-class load-fasl ea eb)
  (define-class-finalization load-fasl eb))
