(cl:in-package #:sicl-boot-phase-5)

(defun import-from-host (e5)
  (import-functions-from-host
   '(;; MISMATCH is used by the string comparison functions.  And
     ;; STRING= is called at compile time by the LOOP macro to
     ;; determine which LOOP keyword was given.
     mismatch
     ;; NREVERSE and POSITION-IF-NOT are called by FORMAT to parse
     ;; arguments.  And the compiler macro of FORMAT is called at
     ;; compile time, so these functions are needed at compile time.
     nreverse position-if-not
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
     sort)
   e5))
