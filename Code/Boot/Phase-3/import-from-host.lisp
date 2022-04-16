(cl:in-package #:sicl-boot-phase-3)

(defun import-from-host (e3)
  (import-functions-from-host
   '(slot-unbound
     no-applicable-method
     cleavir-code-utilities:proper-list-p
     cleavir-code-utilities:extract-required
     cleavir-code-utilities:canonicalize-generic-function-lambda-list
     sicl-method-combination:define-method-combination-expander
     shared-initialize initialize-instance reinitialize-instance
     sicl-host-mop:method-function
     sicl-clos:parse-defmethod sicl-clos:canonicalize-specializers
     (setf env:macro-function))
   e3))


