(cl:in-package #:sicl-boot-phase-4)

(defun import-from-host (e4)
  (import-functions-from-host
   '(cleavir-code-utilities:proper-list-p
     cleavir-code-utilities:canonicalize-generic-function-lambda-list
     cleavir-code-utilities:extract-required
     cleavir-code-utilities:canonicalize-specialized-lambda-list
     cleavir-code-utilities:separate-function-body
     sicl-method-combination:define-method-combination-expander
     ;; POSITION-IF is used in the parser of DEFMETHOD forms to find
     ;; the position of the lambda list, possibly preceded by a bunch
     ;; of method qualifiers.
     position-if
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
     remove-duplicates reduce
     ;; FIND is used in the computation of the class precedence list
     find
     ;; REMOVE is used at compile time to parse DEFGENERIC forms, and
     ;; in several places in CLOS at run time.
     remove
     ;; SORT is used in CLOS at run time to compute the discriminating
     ;; automaton
     sort
     ;; SUBSEQ is used at compile time to parse DEFMETHOD forms, and
     ;; at run time in several places in CLOS, like to compute
     ;; applicable methods and to compute the discriminating function
     subseq)
   e4))

