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
;;;;
;;;; The technique in the paper is capable of determining equivalence
;;;; of every expression used in the program.  Traditionally, global
;;;; value numbering is used to avoid recomputing some value that has
;;;; already been computed and that is available in some lexical
;;;; variable.  This use of global value numbering is dubious these
;;;; days, because it is often cheaper to recompute the value of an
;;;; expression than to store a value for later use.  In particular,
;;;; when the processor has relatively few registers (as is the case
;;;; with the x86-family processors), keeping a value for later use
;;;; might require spilling a register to the stack, which involves a
;;;; memory write and a memory read.  In comparison, recomputing the
;;;; value of a simple expression such as the addition of two register
;;;; operands is extremely fast.  For that reason, we do not use the
;;;; general technique of the paper.

(defsystem :cleavir-equivalent-lexical-locations
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "equivalent-lexical-locations")))

;;  LocalWords:  Subexpression fixpoint subexpression
