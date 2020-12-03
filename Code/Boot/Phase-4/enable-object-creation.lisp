(cl:in-package #:sicl-boot-phase-4)

(defun enable-object-initialization (e3 e4)
  (setf (env:constant-variable (env:client e3) e3 'sicl-clos::+unbound-slot-value+)
        10000000)
  (load-source-file "CLOS/slot-bound-using-index.lisp" e3)
  (load-source-file "CLOS/standard-instance-access.lisp" e3)
  (import-functions-from-host '(slot-unbound) e3)
  (with-intercepted-function-cells
      (e3
       (class-of
        (list (lambda (object)
                (slot-value object 'sicl-boot::%class)))))
    (load-source-file "CLOS/slot-value-etc-support.lisp" e3))
  (load-source-file "CLOS/instance-slots-offset-defconstant.lisp" e3)
  (load-source-file "CLOS/shared-initialize-support.lisp" e3)
  (load-source-file "CLOS/shared-initialize-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (sicl-clos::shared-initialize-default-using-class
        (env:function-cell
         (env:client e3) e3 'sicl-clos::shared-initialize-default-using-class)))
    (load-source-file "CLOS/shared-initialize-defmethods.lisp" e4))
  (load-source-file "CLOS/initialize-instance-support.lisp" e4)
  (load-source-file "CLOS/initialize-instance-defgenerics.lisp" e4)
  (load-source-file "CLOS/initialize-instance-defmethods.lisp" e4))

(defun enable-make-instance (e3 e4)
  (with-intercepted-function-cells
      (e3
       (find-class
        (env:function-cell (env:client e4) e4 'find-class))
       (initialize-instance
        (env:function-cell (env:client e4) e4 'initialize-instance)))
    (load-source-file "CLOS/make-instance-support.lisp" e3)
    (load-source-file "CLOS/make-instance-defgenerics.lisp" e3)
    (load-source-file "CLOS/make-instance-defmethods.lisp" e3)))

(defun enable-slot-value (e3 e4)
  (load-source-file "CLOS/slot-value-etc-defgenerics.lisp" e3)
  (load-source-file "CLOS/slot-value-etc-defmethods.lisp" e3)
  (with-intercepted-function-cells
      (e4
       (sicl-clos:class-slots
        (env:function-cell (env:client e3) e3 'sicl-clos:class-slots))
       (sicl-clos:slot-definition-name
        (env:function-cell (env:client e3) e3 'sicl-clos-slot-definition-name))
       (sicl-clos:slot-value-using-class
        (env:function-cell (env:client e3) e3 'sicl-clos:slot-value-using-class))
       ((setf sicl-clos:slot-value-using-class)
        (env:function-cell (env:client e3) e3 '(setf sicl-clos:slot-value-using-class)))
       (sicl-clos:slot-boundp-using-class
        (env:function-cell (env:client e3) e3 'sicl-clos:slot-boundp-using-class))
       (sicl-clos:slot-makunbound-using-class
        (env:function-cell (env:client e3) e3 'sicl-clos:slot-makunbound-using-class))
       (slot-missing
        (env:function-cell (env:client e3) e3 'slot-missing)))
    (load-source-file "CLOS/slot-value-etc-specified-defuns.lisp" e4)))

(defun enable-object-creation (e3 e4)
  (enable-object-initialization e3 e4)
  (enable-make-instance e3 e4)
  (enable-slot-value e3 e4))
