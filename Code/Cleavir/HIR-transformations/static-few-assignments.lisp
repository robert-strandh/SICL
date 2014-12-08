(cl:in-package #:cleavir-hir-transformations)

;;;; This transformation generates a notation similar to SSA.  It is
;;;; similar in that it keeps all the static versions of every dynamic
;;;; variable.  That property is what makes type inference more
;;;; efficient, because different static versions of the same variable
;;;; can have different types, It is different from SSA in that there
;;;; are no Phi instructions.  Where SSA would have a Phi instruction
;;;; at the beginning of some basic block B, this notation instead has
;;;; an assignment instruction on each of the incoming arcs of B.  
