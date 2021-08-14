(cl:in-package #:sicl-boot-phase-3)
(defun boot (boot)
  (format *trace-output* "Start phase 3~%")
  (with-accessors ((e1 sicl-boot:e1)
                   (e2 sicl-boot:e2)
                   (e3 sicl-boot:e3))
      boot
    (change-class e3 'environment
                  :client (make-instance 'client :environment e3))
    (import-functions-from-host
     '(slot-unbound
       no-applicable-method
       cleavir-code-utilities:proper-list-p
       cleavir-code-utilities:extract-required
       cleavir-code-utilities:canonicalize-generic-function-lambda-list
       sicl-method-combination:define-method-combination-expander
       sicl-loop::list-car sicl-loop::list-cdr
       shared-initialize initialize-instance reinitialize-instance
       sicl-host-mop:method-function
       sicl-clos:parse-defmethod sicl-clos:canonicalize-specializers
       (setf env:macro-function))
     e3)
    (prepare-this-phase e1 e2 e3)
    (load-source-file "Structure/packages.lisp" e3)
    (load-source-file "Symbol/symbol-value-etc-defuns.lisp" e3)
    (sicl-boot:create-accessor-defgenerics e3)
    (sicl-boot:create-mop-classes e3)
    (setf (env:find-class (env:client e3) e3 'symbol) (find-class 'symbol))
    (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e3)
    (load-source-file "CLOS/class-readers-defmethods-before.lisp" e3)))
