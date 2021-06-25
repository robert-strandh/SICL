(cl:in-package #:sicl-boot-phase-5)

(defun define-classp (e4)
  (load-source-file "CLOS/classp-defgeneric.lisp" e4)
  (load-source-file "CLOS/classp-defmethods.lisp" e4))

(defun define-sub-specializer-p (e4)
  (load-source-file "CLOS/sub-specializer-p.lisp" e4))

(defun define-compute-applicable-methods (e4 e5)
  (with-intercepted-function-cells
      (e4
       (class-of
        (env:function-cell (env:client e5) e5 'class-of)))
    (load-source-file "CLOS/compute-applicable-methods-support.lisp" e4))
  (load-source-file "CLOS/compute-applicable-methods-defgenerics.lisp" e4)
  (load-source-file "CLOS/compute-applicable-methods-defmethods.lisp" e4))

(defun define-compute-effective-method (e4)
  (load-source-file "CLOS/compute-effective-method-support.lisp" e4)
  (load-source-file "CLOS/compute-effective-method-defgenerics.lisp" e4)
  (load-source-file "CLOS/compute-effective-method-defmethods.lisp" e4))

(defun define-compute-discriminating-function (e3 e4 e5)
  (load-source-file "CLOS/compute-discriminating-function-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       (make-instance (env:function-cell (env:client e3) e3 'make-instance)))
    (load-source-file "CLOS/maybe-replace-method.lisp" e4))
  (with-intercepted-function-cells
      (e4
       ;; FIXME: this one should also assign the slots in the
       ;; function object.
       (sicl-clos:set-funcallable-instance-function
        (list #'sicl-host-mop:set-funcallable-instance-function))
       (class-of (env:function-cell (env:client e5) e5 'class-of))
       (no-applicable-method (list #'no-applicable-method)))
    (load-source-file "CLOS/compute-discriminating-function-support.lisp" e4))
  (load-source-file "CLOS/discriminating-automaton.lisp" e4)
  (define-error-functions '(sicl-clos::compute-test-tree) e4)
  (load-source-file "CLOS/discriminating-tagbody.lisp" e4)
  (define-error-functions '(sicl-clos::make-cdr) e4)
  (load-source-file "CLOS/compute-discriminating-function-support-c.lisp" e4)
  (load-source-file "CLOS/compute-discriminating-function-defmethods.lisp" e4))

(defun enable-compute-discriminating-function (e3 e4 e5)
  (define-classp e4)
  (define-sub-specializer-p e4)
  (define-compute-applicable-methods e4 e5)
  (define-compute-effective-method e4)
  (define-compute-discriminating-function e3 e4 e5))

(defun enable-defgeneric (e3 e4 e5)
  (with-intercepted-function-cells
      (e4
       (make-instance (env:function-cell (env:client e3) e3 'make-instance))
       (sicl-clos:class-precedence-list
        (env:function-cell (env:client e3) e3 'sicl-clos:class-precedence-list))
       (sicl-clos:class-prototype
        (env:function-cell (env:client e3) e3 'sicl-clos:class-prototype)))
    (load-source-file "CLOS/ensure-generic-function-using-class-support.lisp" e4))
  (load-source-file "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e4)
  (with-intercepted-function-cells
      (e4
       ((setf fdefinition)
        (env:function-cell (env:client e5) e5 '(setf fdefinition))))
    (load-source-file "CLOS/ensure-generic-function-using-class-defmethods.lisp" e4))
  (with-intercepted-function-cells
      (e5
       (sicl-clos:ensure-generic-function-using-class
        (env:function-cell
         (env:client e4) e4 'sicl-clos:ensure-generic-function-using-class)))
    (load-source-file "CLOS/ensure-generic-function-defun.lisp" e5))
  (load-source-file "CLOS/defgeneric-defmacro.lisp" e5))
