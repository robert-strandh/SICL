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
;;;;
;;;; Instead of avoiding computations of expressions, we use global
;;;; value numbering to avoid redundant tests.  The idea is that if
;;;; some test is performed on some lexical location L1 by some
;;;; instruction I1, and there exists an instruction I2 dominated by
;;;; I1 that performs the same test on some lexical location L2
;;;; equivalent to L1, then the test at I2 can be eliminated.  To
;;;; eliminate I2, we use local rewrite techniques to replicate the
;;;; instructions between I1 and I2.  In one replica, the test in I1
;;;; has one outcome, and in the other replica, the test in I2 has the
;;;; opposite outcome.  Thus, rather than having the two sets of paths
;;;; converge in I2 and perform the test again, we can eliminate I2
;;;; altogether.  Path replication has been proposed in the paper
;;;; "Avoiding Conditional Branches by Code Replication" by Frank
;;;; Mueller and David B. Whalley, but they do not explain how the
;;;; replication is done.
;;;;
;;;; As opposed to the traditional use of global value numbering,
;;;; using it to eliminate redundant tests does not require an
;;;; additional value to be stored for later use.  Instead, the
;;;; outcome of the test is fully encoded in the value of the program
;;;; counter.  Furthermore, the operation that is avoided (i.e., the
;;;; second test) is potentially very costly on modern architectures.
;;;; We therefore consider this optimization to be an important one.
;;;;
;;;; Since a test is always performed on some lexical variable, we do
;;;; not need to compute the equivalence of all expressions in the
;;;; program.  We only need the equivalence between lexical variables.
;;;; The partitions manipulated here, are therefore partitions of the
;;;; lexical variables of the program.  We represent such a partition
;;;; as a set (represented as a list) of equivalence classes, where
;;;; each equivalence class is represented as a set (also represented
;;;; as a list) of lexical variables.  As it turns out, we do not need
;;;; to include an equivalence class with a single lexical location in
;;;; it, which improves performance of this technique.

(defsystem :cleavir-equivalent-lexical-locations
  :depends-on (:cleavir-hir
	       :cleavir-meter)
  :serial t
  :components
  ((:file "packages")
   (:file "meter")
   (:file "equivalent-lexical-locations")))

;;  LocalWords:  Subexpression fixpoint subexpression
