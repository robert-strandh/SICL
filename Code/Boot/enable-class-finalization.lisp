(cl:in-package #:sicl-boot)

(defun define-effective-slot-definition-class (ea eb)
  (with-straddled-function-definitions
      ((sicl-clos::effective-slot-definition-class-default)
       eb)
    (load-source "CLOS/effective-slot-definition-class-support.lisp" ea))
  (load-source "CLOS/effective-slot-definition-class-defgeneric.lisp" eb)
  (load-source "CLOS/effective-slot-definition-class-defmethods.lisp" eb))

(defun define-class-finalization (eb)
  (load-source "CLOS/class-finalization-defgenerics.lisp" eb)
  (load-source "CLOS/class-finalization-support.lisp" eb)
  (load-source "CLOS/class-finalization-defmethods.lisp" eb))

(defun define-allocate-class-prototype (eb)
  (setf (sicl-genv:fdefinition 'sicl-clos::allocate-class-prototype eb)
        (constantly nil)))

(defun enable-class-finalization (ea eb)
  (define-effective-slot-definition-class ea eb)
  (define-allocate-class-prototype eb)
  (define-class-finalization eb))
