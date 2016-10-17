(cl:in-package #:asdf-user)

;;;; This system implements an adaptation of Kildall's algorithm so
;;;; that instead of computing information about common subexpressions
;;;; it does "value numbering".
;;;;
;;;; In this version of value numbering, we are only concerned with
;;;; lexical locations and assignment instructions.  Any other
;;;; operation is considered unique in that it creates a fresh
;;;; symbolic value.  In other words, we do not attempt to identify
;;;; symbolic values of two similar operations with identical symbolic
;;;; values as arguments.
;;;;
;;;; The information computed by this system will allow us to
;;;; determine whether the value of some lexical location L1 at some
;;;; program point P1 is always the same as the value of some lexical
;;;; location L2 at some program point P2 as a result of a chain of
;;;; assignment instructions from P1 to P2.
;;;;
;;;; The main use for this kind of information is to determine whether
;;;; a test at P2 is superfluous and can be removed after path
;;;; replication from P1 to P2.  There are a few situations where we
;;;; want to remove such superfluous tests:
;;;;
;;;;   * Frequently, we want to compute both the CAR and the CDR of
;;;;     some variable V in the program.  When these operations are
;;;;     inlined, there will be a test to determine whether V contains
;;;;     a CONS cell, then whether it contains NIL.  This test will be
;;;;     repeated for the two operations.  By using path replication,
;;;;     we can remove the second similar test.
;;;;
;;;;   * When a predicate such as CONSP is inlined, it assigns either
;;;;     T or NIL to a variable V.  When a call to the predicate is
;;;;     used as the test of an IF, soon afterwards, this variable or
;;;;     a temporary variable assigned to its value will be the
;;;;     argument of an EQ-INSTRUCTION testing whether it is NIL.  By
;;;;     using path replication, we preserve the value assigned to V
;;;;     in the program counter until the EQ-INSTRUCTION is reached.
;;;;     As a result, we can first remove both the EQ-INSTRUCTION, and
;;;;     then we recognize that the value assigned to V is not used,
;;;;     so the assignments of T and NIL to V can be removed as well.

(defsystem :cleavir-simple-value-numbering
  :depends-on (:cleavir-meter
	       :cleavir-hir-transformations
	       :cleavir-liveness)
  :serial t
  :components
  ((:file "packages")
   (:file "meter")
   (:file "simple-value-numbering")))

;;  LocalWords:  inlined subexpressions
