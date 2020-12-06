(cl:in-package #:sicl-boot-phase-5)

(defun define-classp (e5)
  (load-source-file "CLOS/classp-defgeneric.lisp" e5)
  (load-source-file "CLOS/classp-defmethods.lisp" e5))

(defun define-sub-specializer-p (e5)
  (load-source-file "CLOS/sub-specializer-p.lisp" e5))

(defun define-compute-applicable-methods (e5)
  (load-source-file "CLOS/compute-applicable-methods-support.lisp" e5)
  (load-source-file "CLOS/compute-applicable-methods-defgenerics.lisp" e5)
  (load-source-file "CLOS/compute-applicable-methods-defmethods.lisp" e5))

(defun define-compute-effective-method (e5)
  (load-source-file "CLOS/compute-effective-method-support.lisp" e5)
  (load-source-file "CLOS/compute-effective-method-defgenerics.lisp" e5)
  (load-source-file "CLOS/compute-effective-method-defmethods.lisp" e5))

(defun define-compute-discriminating-function (e4 e5)
  (load-source-file "CLOS/compute-discriminating-function-defgenerics.lisp" e5)
  (with-intercepted-function-cells
      (e5
       (class-of (env:function-cell (env:client e4) e4 'class-of))
       (make-instance (env:function-cell (env:client e4) e4 'make-instance)))
    (load-source-file "CLOS/maybe-replace-method.lisp" e5))
  (with-intercepted-function-cells
      (e5
       ;; FIXME: this one should also assign the slots in the
       ;; function object.
       (sicl-clos:set-funcallable-instance-function
        (list #'closer-mop:set-funcallable-instance-function))
       (no-applicable-method (list #'no-applicable-method)))
    (load-source-file "CLOS/compute-discriminating-function-support.lisp" e5))
  (load-source-file "CLOS/discriminating-automaton.lisp" e5)
  (define-error-functions '(sicl-clos::compute-test-tree) e5)
  (load-source-file "CLOS/discriminating-tagbody.lisp" e5)
  (define-error-functions '(sicl-clos::make-cdr) e5)
  (load-source-file "CLOS/compute-discriminating-function-support-c.lisp" e5)
  (load-source-file "CLOS/compute-discriminating-function-defmethods.lisp" e5))

(defun enable-compute-discriminating-function (e4 e5)
  (define-classp e5)
  (define-sub-specializer-p e5)
  (define-compute-applicable-methods e5)
  (define-compute-effective-method e5)
  (define-compute-discriminating-function e4 e5))

(defun enable-defgeneric (e4 e5)
  (with-intercepted-function-cells
      (e5
       (make-instance (env:function-cell (env:client e4) e4 'make-instance))
       (sicl-clos:class-precedence-list
        (env:function-cell (env:client e4) e4 'sicl-clos:class-precedence-list))
       (sicl-clos:class-prototype
        (env:function-cell (env:client e4) e4 'sicl-clos:class-prototype)))
    (load-source-file "CLOS/ensure-generic-function-using-class-support.lisp" e5))
  (load-source-file "CLOS/ensure-generic-function-using-class-defgenerics.lisp" e5)
  (load-source-file "CLOS/ensure-generic-function-using-class-defmethods.lisp" e5))
