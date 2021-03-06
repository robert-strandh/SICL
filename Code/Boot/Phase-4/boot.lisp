(cl:in-package #:sicl-boot-phase-4)

(defun boot (boot)
  (format *trace-output* "Start phase 4~%")
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (change-class e4 'environment
                  :client (make-instance 'client :environment e4))
    (import-functions-from-host
     '(cleavir-code-utilities:parse-generic-function-lambda-list
       cleavir-code-utilities:parse-specialized-lambda-list
       cleavir-code-utilities:separate-function-body
       cleavir-code-utilities:required
       sicl-method-combination:define-method-combination-expander
       sicl-loop::list-car sicl-loop::list-cdr)
     e4)
    (prepare-this-phase e2 e3 e4)
    (load-source-file "Package-and-symbol/symbol-value-etc-defuns.lisp" e4)
    (sicl-boot:create-accessor-defgenerics e4)
    (sicl-boot:create-mop-classes e4)
    ;; (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e4)
    ;; (load-source-file "CLOS/class-readers-defmethods-before.lisp" e4)
    (load-source-file "Package-and-symbol/symbol-value-etc-defuns.lisp" e4)))
