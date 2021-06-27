(cl:in-package #:sicl-boot-phase-6)

(defun define-default-superclasses (e5)
  (load-source-file "CLOS/default-superclasses-defgeneric.lisp" e5)
  (load-source-file "CLOS/default-superclasses-defmethods.lisp" e5))

(defun enable-class-initialization (e5)
  (load-source-file "CLOS/add-remove-direct-subclass.lisp" e5)
  (load-source-file "CLOS/direct-slot-definition-class.lisp" e5)
  (define-default-superclasses e5)
  (load-source-file "CLOS/reader-writer-method-class.lisp" e5)
  (load-source-file "CLOS/add-accessor-method.lisp" e5)
  (with-intercepted-function-cells
      (e5
       (functionp (list (constantly t))))
    (load-source-file "CLOS/class-initialization-support.lisp" e5))
  (load-source-file "CLOS/class-initialization-defmethods.lisp" e5)
  (load-source-file "CLOS/reinitialize-instance-defgenerics.lisp" e5)
  (load-source-file "CLOS/reinitialize-instance-support.lisp" e5)
  (load-source-file "CLOS/reinitialize-instance-defmethods.lisp" e5))

(defun define-ensure-class-using-class (e5)
  (load-source-file "CLOS/ensure-class-using-class-support.lisp" e5)
  (load-source-file "CLOS/ensure-class-using-class-defgenerics.lisp" e5)
  (load-source-file "CLOS/ensure-class-using-class-defmethods.lisp" e5))

(defun enable-defclass (e5)
  (enable-class-initialization e5)
  (define-ensure-class-using-class e5))
