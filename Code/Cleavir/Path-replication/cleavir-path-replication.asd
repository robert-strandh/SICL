(cl:in-package #:asdf-user)

;;;; Consider a set S of instructions such that all the members of S
;;;; are replicas of some initial instruction I with two successors.
;;;; Also consider an instruction D such that D dominates every
;;;; element of S and D is not an element of S.  Let R be the set of
;;;; instructions that are ancestors of some instruction in S and that
;;;; are dominated by D.  The set R includes D.  This transformation
;;;; turns R into the empty set by systematically removing
;;;; instructions from the set according to specific rules.  In order
;;;; to preserve the semantics of the program, when an instruction is
;;;; removed from R, one or more equivalent instruction are added as
;;;; successors of some instruction in S.  Notice that it is possible
;;;; that the instructions in S are elements of R.  This situation may
;;;; occur when there is a back arc from the initial instruction I to
;;;; one of its predecessors.  For this transformation to apply, no
;;;; instruction in R is allowed to write to an output that is an
;;;; input of I.
;;;;
;;;; To understand the purpose of the transformation, imagine that
;;;; instructions have one of the three colors black, red, and blue.
;;;; Initially, all instructions are black.  An instruction is colored
;;;; red if it has been determined that it is dominated by the first
;;;; (or left) successor of I.  It is colored blue if it has been
;;;; determined that it is dominated by the second (or right)
;;;; successor of I.  In other words, for red and blue instructions we
;;;; know the outcome of the test of I.
;;;;
;;;; The transformation is accomplished by three local rewrite rules.
;;;; These rewrite rules apply to some element s of S.
;;;;
;;;; The first rewrite rule is applicable when some predecessor p of
;;;; some element s of S is red or blue.  If p is red, then s is
;;;; replaced as a successor of p by the first successor of s.  If p
;;;; is blue, then s is replaced as a successor of p by the second
;;;; successor of s.
;;;;
;;;; The second rewrite rule is applicable when s has more than one
;;;; predecessor.  We assume that the first rewrite rule has already
;;;; been applied if possible, so that all the predecessors are black.
;;;; The rewrite rule consists of replicating s as many times as it
;;;; has predecessors in R so that each replica has a single
;;;; predecessor in R.  Clearly, this rewrite rule preserves the
;;;; semantics of the program.
;;;;
;;;; The third rewrite rule is applicable to when s has a single
;;;; predecessor.  Let p be the predecessor of s.  We exchange the
;;;; order of p and s by removing p and adding a replica of p in each
;;;; of the two branches of s.  The replica of p that is added to the
;;;; first (left) branch of s is colored red, and the replica of p
;;;; that is added to the second (right) branch of s is colored blue.
;;;; Since we require that no output written by p is an input of s,
;;;; the semantics of the program are preserved by this rewrite rule
;;;; as well.
;;;;
;;;; FIXME: Prove termination.

(defsystem :cleavir-path-replication
  :depends-on (:cleavir-hir)
  :serial t
  :components
  ((:file "packages")
   (:file "rewrite")
   (:file "applicability")
   (:file "path-replication")))
