(cl:in-package #:sicl-boot-phase-4)

(defun import-from-host (e4)
  (import-functions-from-host
   '(cleavir-code-utilities:proper-list-p
     cleavir-code-utilities:canonicalize-generic-function-lambda-list
     cleavir-code-utilities:extract-required
     cleavir-code-utilities:canonicalize-specialized-lambda-list
     cleavir-code-utilities:separate-function-body
     sicl-method-combination:define-method-combination-expander)
   e4))

