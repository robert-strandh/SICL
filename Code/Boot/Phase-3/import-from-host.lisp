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
     (setf env:macro-function)
     ;; FIND-IF-NOT is used in COMPUTE-EFFECTIVE-SLOT-DEFINITION to
     ;; determine whether a slot has an :INITFORM
     find-if-not
     ;; FIND-IF is used in ADD-METHOD to determine whether an existing
     ;; method needs to be removed before the new one is added.
     find-if
     ;; REMOVE-DUPLICATES and REDUCE are used in order to compute all
     ;; superclasses of a given class, for the purpose of computing
     ;; the class precedence list.  This is done by appending the
     ;; class precedence lists of the superclasses and then removing
     ;; duplicates.
     remove-duplicates reduce)
   e3))


