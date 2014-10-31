(cl:in-package #:cleavir-hir-transformations)

;;;; Inlining calls is done at the HIR level.  That way, we can easily
;;;; take advantage of the box/unbox optimizations and the type
;;;; inference; transformations that are also accomplished at this
;;;; level.
;;;;
;;;; Inlining requires access to the global environment, because that
;;;; is where the HIR version of each function is stored.  We have yet
;;;; to determine the exact way to communicate this environment to
;;;; Cleavir, and in which way Cleavir will access it.


; LocalWords:  Inlining unbox optimizations
