(cl:in-package #:sicl-boot-phase-5)

(defun enable-object-initialization (e4 e5)
  (setf (env:constant-variable (env:client e4) e4 'sicl-clos::+unbound-slot-value+)
        10000000)
  (load-source-file "CLOS/slot-bound-using-index.lisp" e4)
  (load-source-file "CLOS/standard-instance-access.lisp" e4)
  (load-source-file "CLOS/instance-slots-offset-defconstant.lisp" e4)
  (import-functions-from-host
   '(sicl-loop::list-car sicl-loop::list-cdr)
   e5)
  (with-intercepted-function-cells
      (e5
       (sicl-clos:slot-definition-name
        (env:function-cell
         (env:client e4) e4 'sicl-clos:slot-definition-name))
       (sicl-clos:slot-definition-initargs
        (env:function-cell
         (env:client e4) e4 'sicl-clos:slot-definition-initargs))
       (sicl-clos:slot-definition-initfunction
        (env:function-cell
         (env:client e4) e4 'sicl-clos:slot-definition-initfunction))
       (sicl-clos::slot-boundp-using-class-default
        (env:function-cell
         (env:client e4) e4 'sicl-clos::slot-boundp-using-class-default))
       ((setf sicl-clos::slot-value-using-class-default)
        (env:function-cell
         (env:client e4) e4 '(setf sicl-clos::slot-value-using-class-default)))
       ((setf sicl-clos::standard-instance-access)
        (env:function-cell
         (env:client e4) e4 '(setf sicl-clos::standard-instance-access)))
       (sicl-clos:class-slots
        (env:function-cell (env:client e4) e4 'sicl-clos:class-slots)))
    (load-source-file "CLOS/shared-initialize-support.lisp" e5))
  (load-source-file "CLOS/shared-initialize-defgenerics.lisp" e5)
  (load-source-file "CLOS/shared-initialize-defmethods.lisp" e5)
  (load-source-file "CLOS/initialize-instance-support.lisp" e5)
  (load-source-file "CLOS/initialize-instance-defgenerics.lisp" e5)
  (load-source-file "CLOS/initialize-instance-defmethods.lisp" e5))

(defun enable-make-instance (e4 e5)
  (with-intercepted-function-cells
      (e4
       (find-class
        (env:function-cell (env:client e5) e5 'find-class))
       (initialize-instance
        (env:function-cell (env:client e5) e5 'initialize-instance)))
    (load-source-file "CLOS/make-instance-support.lisp" e4)
    (load-source-file "CLOS/make-instance-defgenerics.lisp" e4)
    (load-source-file "CLOS/make-instance-defmethods.lisp" e4)))

(defun enable-slot-value (e5)
  (setf (env:constant-variable
         (env:client e5) e5 'sicl-clos::+unbound-slot-value+)
        10000000)
  (load-source-file "CLOS/standard-instance-access.lisp" e5)
  (load-source-file "CLOS/slot-bound-using-index.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-defgenerics.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-support.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-defmethods.lisp" e5)
  (load-source-file "CLOS/slot-value-etc-specified-defuns.lisp" e5))

(defun enable-object-creation (e4 e5)
  (enable-slot-value e5)
  ;; (enable-object-initialization e4 e5)
  ;; (enable-make-instance e4 e5))
  )
