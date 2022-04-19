(cl:in-package #:sicl-boot-phase-5)

(defun import-number-functions (e5)
  (import-functions-from-host
   '(+ - * < <= = > >= /= floor 1+ 1-)
   e5))

(defun import-sequence-functions (e5)
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
     ;; FIND is used in the computation of the class precedence list.
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
     length
     ;; ELT is used for parsing DEFMETHOD forms.
     elt)
   e5))

(defun import-from-host (e5)
  (import-number-functions e5)
  (import-sequence-functions e5))