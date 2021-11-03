(cl:in-package #:sicl-boot-phase-4)

(defun boot (boot)
  (format *trace-output* "Start phase 4~%")
  (with-accessors ((e2 sicl-boot:e2)
                   (e3 sicl-boot:e3)
                   (e4 sicl-boot:e4))
      boot
    (change-class e4 'environment)
    (change-class (env:client e4) 'client)
    (import-functions-from-host
     '(cleavir-code-utilities:proper-list-p
       cleavir-code-utilities:canonicalize-generic-function-lambda-list
       cleavir-code-utilities:extract-required
       cleavir-code-utilities:canonicalize-specialized-lambda-list
       cleavir-code-utilities:separate-function-body
       sicl-method-combination:define-method-combination-expander)
     e4)
    (prepare-this-phase e2 e3 e4)
    (load-source-file "Symbol/symbol-value-etc-defuns.lisp" e4)
    (sicl-boot:create-accessor-defgenerics e4)
    (sicl-boot:create-mop-classes e4)
    ;; (load-source-file "CLOS/class-readers-forward-referenced-class-defmethods.lisp" e4)
    ;; (load-source-file "CLOS/class-readers-defmethods-before.lisp" e4)
    (load-source-file "Symbol/symbol-value-etc-defuns.lisp" e4)))
