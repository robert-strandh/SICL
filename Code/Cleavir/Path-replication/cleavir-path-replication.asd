(cl:in-package #:asdf-user)

;;;; Consider a set S of instructions and an instruction D such that D
;;;; dominates every element of S and D is not an element of S.  Let R
;;;; be the set of instructions that are ancestors of some instruction
;;;; in S and that are dominated by D.  The set R includes D.  This
;;;; transformation turns R into the empty set by systematically
;;;; removing instructions from the set according to specific rules.
;;;; In order to preserve the semantics of the program, when an
;;;; instruction is removed from R, one or more equivalent instruction
;;;; are added as successors of some instruction in S.
;;;;
;;;; The transformation is accomplished by three local rewrite rules.
;;;;
;;;; The first rewrite rule is applicable to instructions in R that
;;;; have more than one predecessor.  Let I be such an instruction.
;;;; The rewrite rule consists of replicating I as many times as it
;;;; has predecessors in R so that each replica has a single successor
;;;; in R.  Clearly, this rewrite rule preserves the semantics of the
;;;; program.
;;;;
;;;; The second rewrite rule is applicable to instructions in R with a
;;;; single predecessor.  Let I be such an instruction and let P be
;;;; its predecessor.  An additional requirement for this rewrite rule
;;;; to be applicable is that no output written by P is the input of
;;;; I.  Because of this second requirement, the semantics of the
;;;; program are preserved by this rewrite rule as well.
;;;;
;;;; FIXME: say more.

(defsystem :cleavir-path-replication
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "rewrite")
   (:file "applicability")
   (:file "path-replication")))
