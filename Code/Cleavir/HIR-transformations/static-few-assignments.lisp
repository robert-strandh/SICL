(cl:in-package #:cleavir-hir-transformations)

;;;; This transformation generates a notation similar to SSA.  It is
;;;; similar in that it keeps all the static versions of every dynamic
;;;; variable.  That property is what makes type inference more
;;;; efficient, because different static versions of the same variable
;;;; can have different types, It is different from SSA in that there
;;;; are no Phi instructions.  Where SSA would have a Phi instruction
;;;; at the beginning of some basic block B, this notation instead has
;;;; an assignment instruction on each of the incoming arcs of B.  
;;;;
;;;; It might appear that type inference could be simplified if
;;;; everything was SSA, because then an SSA variable can only have a
;;;; single type in the entire program, which would remove the
;;;; requirement to keep type information for each variable at each
;;;; program point.  However, in the general case, there are lexical
;;;; variables that have indefinite extent, and those variables can
;;;; not benefit from transformation to SSA or from this
;;;; transformation, for that matter.  Therefore, the type inference
;;;; machinery still has to be designed to keep type information for
;;;; each variable at each point in the program. 
