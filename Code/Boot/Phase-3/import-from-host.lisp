(cl:in-package #:sicl-boot-phase-3)

(defun import-number-functions (e3)
  (import-functions-from-host
   '(+ - < <= = /= floor 1+ 1-)
   e3))

(defun import-code-utilities (e3)
  (import-functions-from-host
   '(cleavir-code-utilities:proper-list-p
     cleavir-code-utilities:extract-required
     cleavir-code-utilities:canonicalize-generic-function-lambda-list)
   e3))

(defun import-misc (e3)
  (import-functions-from-host
   '(slot-unbound
     no-applicable-method
     sicl-method-combination:define-method-combination-expander
     shared-initialize initialize-instance reinitialize-instance
     sicl-host-mop:method-function
     sicl-clos:parse-defmethod sicl-clos:canonicalize-specializers
     (setf env:macro-function))
   e3))

(defun import-sequence-functions (e3)
  (import-functions-from-host
   '(;; FIND-IF-NOT is used in COMPUTE-EFFECTIVE-SLOT-DEFINITION to
     ;; determine whether a slot has an :INITFORM.
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
     ;; automaton.
     sort
     ;; SUBSEQ is used at compile time to parse DEFMETHOD forms, and
     ;; at run time in several places in CLOS, like to compute
     ;; applicable methods and to compute the discriminating function.
     subseq
     ;; POSITION is used at run time by CLOS to compute applicable methods
     ;; and to determine which of two specializers is more specific.
     position
     ;; REVERSE is used in several places at run time, for instance
     ;; for computing class precedence lists.
     reverse
     ;; COUNT is used in CLOS in order to finalize inheritance.
     count
     ;; LENGTH is used in various places at run time, such as in CLOS for
     ;; SHARED-INITIALIZE.
     length)
   e3))

(defun import-from-host (e3)
  (import-number-functions e3)
  (import-code-utilities e3)
  (import-misc e3)
  (import-sequence-functions e3))