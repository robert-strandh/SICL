(cl:in-package #:sicl-boot-phase-5)

(defun enable-generic-function-initialization (e5)
  (load-source-file "CLOS/discriminating-automaton.lisp" e5)
  (define-error-functions
      '(sicl-clos::compute-test-tree)
      e5)
  (load-source-file "CLOS/discriminating-tagbody.lisp" e5)
  (define-error-functions
      '(sicl-clos::make-cdr)
      e5)
  (load-source-file "CLOS/classp-defgeneric.lisp" e5)
  (load-source-file "CLOS/classp-defmethods.lisp" e5)
  (load-source-file "CLOS/sub-specializer-p.lisp" e5)
  (load-source-file "CLOS/compute-applicable-methods-defgenerics.lisp" e5)
  (load-source-file "CLOS/compute-applicable-methods-support.lisp" e5)
  (load-source-file "CLOS/compute-applicable-methods-defmethods.lisp" e5)
  (load-source-file "CLOS/compute-effective-method-defgenerics.lisp" e5)
  (load-source-file "CLOS/compute-effective-method-support.lisp" e5)
  (load-source-file "CLOS/compute-effective-method-defmethods.lisp" e5)
  (load-source-file "CLOS/no-applicable-method-defgenerics.lisp" e5)
  (load-source-file "CLOS/no-applicable-method-defmethods.lisp" e5)
  (load-source-file "CLOS/compute-discriminating-function-defgenerics.lisp" e5)
  (with-intercepted-function-cells
      (e5
       ;; FIXME: this one should also assign the slots in the
       ;; function object.
       (sicl-clos:set-funcallable-instance-function
        (list #'closer-mop:set-funcallable-instance-function)))
    (load-source-file "CLOS/compute-discriminating-function-support.lisp" e5)
    (load-source-file "CLOS/compute-discriminating-function-support-c.lisp" e5)
    (load-source-file "CLOS/compute-discriminating-function-defmethods.lisp" e5)
    (load-source-file "CLOS/invalidate-discriminating-function.lisp" e5))
  (import-functions-from-host
   '(cleavir-code-utilities:parse-generic-function-lambda-list
     cleavir-code-utilities:required)
   e5)
  (load-source-file "CLOS/generic-function-initialization-support.lisp" e5)
  (load-source-file "CLOS/generic-function-initialization-defmethods.lisp" e5))

(defun enable-generic-function-creation (e5)
  (enable-generic-function-initialization e5))
