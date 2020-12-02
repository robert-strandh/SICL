(cl:in-package #:sicl-boot-phase-4)

(defun enable-object-allocation (e4)
  (setf (env:fdefinition (env:client e4) e4 'sicl-clos::allocate-general-instance)
        (lambda (class size)
          (make-instance 'sicl-boot:header
            :class class
            :rack (make-array size :initial-element 10000000))))
  (load-source-file "CLOS/stamp-offset-defconstant.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-support.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-defgeneric.lisp" e4)
  (load-source-file "CLOS/effective-slot-definition-class-defmethods.lisp" e4)
  (load-source-file "CLOS/allocate-instance-support.lisp" e4)
  (load-source-file "CLOS/allocate-instance-defgenerics.lisp" e4)
  (load-source-file "CLOS/allocate-instance-defmethods.lisp" e4))

(defun enable-class-finalization (e3 e4)
  (load-source-file "CLOS/class-finalization-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:class-direct-superclasses
        (env:function-cell (env:client e4) e4 'sicl-clos:class-direct-superclasses))
       (make-instance
        (env:function-cell (env:client e3) e3 'make-instance)))
    (load-source-file "CLOS/class-finalization-support.lisp" e4))
  (with-intercepted-function-cells
      (e4
       (sicl-clos:effective-slot-definition-class
        (env:function-cell
         (env:client e3) e3 'sicl-clos:effective-slot-definition-class)))
    (load-source-file "CLOS/class-finalization-defmethods.lisp" e4)))

(defun define-add-remove-direct-subclass (e4)
  (load-source-file "CLOS/add-remove-direct-subclass-support.lisp" e4)
  (load-source-file "CLOS/add-remove-direct-subclass-defgenerics.lisp" e4)
  (load-source-file "CLOS/add-remove-direct-subclass-defmethods.lisp" e4))

(defun define-direct-slot-definition-class (e3 e4)
  (with-intercepted-function-cells
      (e3
       (find-class (env:function-cell (env:client e4) e4 'find-class)))
    (load-source-file "CLOS/direct-slot-definition-class-support.lisp" e3))
  (load-source-file "CLOS/direct-slot-definition-class-defgeneric.lisp" e3)
  (load-source-file "CLOS/direct-slot-definition-class-defmethods.lisp" e3))

(defun define-reader-writer-method-class (e3 e4)
  (with-intercepted-function-cells
      (e3
       (find-class (env:function-cell (env:client e4) e4 'find-class)))
    (load-source-file "CLOS/reader-writer-method-class-support.lisp" e3))
  (load-source-file "CLOS/reader-writer-method-class-defgenerics.lisp" e3)
  (load-source-file "CLOS/reader-writer-method-class-defmethods.lisp" e3))

(defun define-default-superclasses (e3 e4)
  (load-source-file "CLOS/default-superclasses-defgeneric.lisp" e3)
  (with-intercepted-function-cells
      (e3
       (find-class (env:function-cell (env:client e4) e4 'find-class)))
    (load-source-file "CLOS/default-superclasses-defmethods.lisp" e3)))

(defun enable-class-initialization (e3 e4 e5)
  (define-add-remove-direct-subclass e4)
  (define-direct-slot-definition-class e3 e4)
  (define-default-superclasses e3 e4)
  (define-reader-writer-method-class e3 e4)
  (with-intercepted-function-cells
      (e4
       (make-instance (env:function-cell (env:client e3) e3 'make-instance))
       (sicl-clos:reader-method-class
        (env:function-cell (env:client e3) e3 'sicl-clos:reader-method-class))
       (sicl-clos:writer-method-class
        (env:function-cell (env:client e3) e3 'sicl-clos:writer-method-class))
       (find-class (env:function-cell (env:client e5) e5 'find-class))
       (ensure-generic-function
        (env:function-cell (env:client e5) e5 'ensure-generic-function)))
    (load-source-file "CLOS/add-accessor-method.lisp" e4))
  (with-intercepted-function-cells
      (e4
       (make-instance (env:function-cell (env:client e3) e3 'make-instance))
       (sicl-clos:direct-slot-definition-class
        (env:function-cell (env:client e3) e3
                           'sicl-clos:direct-slot-definition-class))
       (sicl-clos:default-superclasses
        (env:function-cell (env:client e3) e3
                           'sicl-clos:default-superclasses))
       (functionp (list (constantly t)))
       ;; There is a test to verify that each direct superclass is a class
       ;; and it always is during bootstrapping.
       (typep (list (constantly t)))
       ;; There is a call to VALIDATE-SUPERCLASS, and during
       ;; bootstrapping, it will always return true.
       (sicl-clos:validate-superclass (list (constantly t))))
    (load-source-file "CLOS/class-initialization-support.lisp" e4))
  (with-intercepted-function-cells
      (e4
       (sicl-clos:method-function
        (env:function-cell (env:client e3) e3 'sicl-clos:method-function)))
    (load-source-file "CLOS/class-initialization-defmethods.lisp" e4))
   (load-source-file "CLOS/reinitialize-instance-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:default-superclasses
        (env:function-cell (env:client e3) e3 'sicl-clos:default-superclasses)))
    (load-source-file "CLOS/reinitialize-instance-support.lisp" e4))
   (load-source-file "CLOS/reinitialize-instance-defmethods.lisp" e4))

(defun define-ensure-class-using-class (e3 e4 e5)
  (with-intercepted-function-cells
      (e4
       (make-instance (env:function-cell (env:client e3) e3 'make-instance))
       (find-class
        (env:function-cell (env:client e5) e5 'find-class))
       ((setf find-class)
        (env:function-cell (env:client e5) e5 '(setf find-class))))
    (load-source-file "CLOS/ensure-class-using-class-support.lisp" e4))
  (load-source-file "CLOS/ensure-class-using-class-defgenerics.lisp" e4)
  (load-source-file "CLOS/ensure-class-using-class-defmethods.lisp" e4))

(defun define-ensure-class (e4 e5)
  (with-intercepted-function-cells
      (e5
       (sicl-clos:ensure-class-using-class
        (env:function-cell
         (env:client e4) e4 'sicl-clos:ensure-class-using-class)))
    (load-source-file "CLOS/ensure-class.lisp" e5)))

(defun enable-defclass (e3 e4 e5)
  (enable-object-allocation e4)
  (enable-class-finalization e3 e4)
  (enable-class-initialization e3 e4 e5)
  (define-ensure-class-using-class e3 e4 e5)
  (define-ensure-class e4 e5))
