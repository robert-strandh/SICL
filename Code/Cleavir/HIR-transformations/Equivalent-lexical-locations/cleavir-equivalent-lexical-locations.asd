(cl:in-package #:asdf-user)

;;;; The purpose of this system is to compute equivalence of lexical
;;;; locations in a HIR program.  For the purpose of this system, a
;;;; PROGRAM POINT is the state immediately PRECEDING some
;;;; instruction.  This system computes, for each such program point,
;;;; which lexical locations are equivalent at that point.  By
;;;; EQUIVALENT, we mean that they provably contain the same value at
;;;; that point.
;;;;
;;;; We use a simplified version of the algorithm as documented in the
;;;; paper "A Unified Approach to Global Program Optimization" by Gary
;;;; A. Kildall.
;;;;
;;;; The paper by Kildall is mostly a general description of the now
;;;; traditional technique for forward data flow analysis, using
;;;; fixpoint iteration.  However, section 4 of that paper is entitled
;;;; "Common Subexpression Elimination", and the technique used in
;;;; this system is based on that section.  Despite the title of the
;;;; section, it is not a technique for common subexpression
;;;; elimination as it is now defined, but instead a technique for
;;;; what is now called "Global Value Numbering".
;;;;
;;;; The essence of section 4 of the paper is the representation of
;;;; the data being manipulated by the algorithm.  The expressions
;;;; computed by a program are organized as a PARTITION for each
;;;; program point.  A partition is a set of equivalence classes.  The
;;;; expressions that are elements of some equivalence class of the
;;;; partition are the ones that can be proven to be equivalent at
;;;; that program point.

(defsystem :cleavir-equivalent-lexical-locations
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "equivalent-lexical-locations")))

;;  LocalWords:  Subexpression fixpoint subexpression
