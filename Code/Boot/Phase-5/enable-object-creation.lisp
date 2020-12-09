(cl:in-package #:sicl-boot-phase-5)

(defun enable-object-initialization (e5)
  (load-source-file "CLOS/standard-instance-access.lisp" e5)
  (load-source-file "CLOS/instance-slots-offset-defconstant.lisp" e5)
  (import-functions-from-host
   '(sicl-loop::list-car sicl-loop::list-cdr)
   e5)
  (load-source-file "CLOS/shared-initialize-support.lisp" e5)
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
  (enable-object-initialization e5)
  ;; (enable-make-instance e4 e5))
  )
