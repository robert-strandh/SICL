(cl:in-package #:sicl-new-boot-phase-3)

(defun enable-generic-function-invocation (boot)
  (with-accessors ((e2 sicl-new-boot:e2)
                   (e3 sicl-new-boot:e3)) boot
    (load-file "CLOS/compute-applicable-methods-defgenerics.lisp" e3)
    (load-file "CLOS/compute-applicable-methods-defmethods.lisp" e3)
    (load-file "CLOS/compute-effective-method-defgenerics.lisp" e3)
    (load-file "CLOS/compute-effective-method-support.lisp" e3)
    (load-file "CLOS/compute-effective-method-support-c.lisp" e3)
    (load-file "CLOS/compute-effective-method-defmethods-b.lisp" e3)
    (load-file "CLOS/no-applicable-method-defgenerics.lisp" e3)
    (load-file "CLOS/no-applicable-method.lisp" e3)
    (import-functions-from-host '(list* caddr find subseq) e3)
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-p e3)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-p e2))
    (setf (sicl-genv:fdefinition 'sicl-clos::general-instance-access e3)
          (sicl-genv:fdefinition 'sicl-clos::general-instance-access e2))
    (setf (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e3)
          (sicl-genv:fdefinition '(setf sicl-clos::general-instance-access) e2))
    (setf (sicl-genv:fdefinition 'sicl-clos:set-funcallable-instance-function e3)
          #'closer-mop:set-funcallable-instance-function)
    (load-file "CLOS/compute-discriminating-function-support.lisp" e3)
    (import-functions-from-host
     '(assoc 1+
       sicl-clos::add-path
       sicl-clos::compute-discriminating-tagbody
       sicl-clos::extract-transition-information
       sicl-clos::make-automaton)
     e3)
    (load-file "CLOS/compute-discriminating-function-support-c.lisp" e3)
    (load-file "CLOS/compute-discriminating-function-defmethods.lisp" e3)))
