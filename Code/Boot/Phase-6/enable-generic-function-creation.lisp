(cl:in-package #:sicl-boot-phase-6)

(defun enable-generic-function-initialization (e5)
  (load-source-file "CLOS/no-applicable-method.lisp" e5)
  (with-intercepted-function-cells
      (e5
       ;; FIXME: this one should also assign the slots in the
       ;; function object.
       (sicl-clos:set-funcallable-instance-function
        (list #'sicl-host-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e5))
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e5)
  (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-support.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-defmethods.lisp" e5))

(defun enable-generic-function-creation (e5)
  (enable-generic-function-initialization e5))
