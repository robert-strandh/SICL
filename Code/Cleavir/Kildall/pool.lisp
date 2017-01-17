(in-package #:cleavir-kildall)

;;;; Pools are half the importance of the algorithm but can be very
;;;; simple. The algorithm basically returns an association map of
;;;; instructions to pools, so a pool should have whatever
;;;; instruction-specific information you are tracking.
;;;;
;;;; The set of pools must form a meet-semilattice, i.e. it has an
;;;; operation "^" (here, POOL-MEET) such that
;;;;  * A^B exists and is a pool for all A and B that are pools
;;;;  * ^ is commutative (A^B = B^A)
;;;;  * ^ is assocative (A^(B^C) = (A^B)^C)
;;;;
;;;; ^ induces a partial order, <=, such that A <= B iff A ^ B = A
;;;;
;;;; The semilattice of pools must additionally be finite or the
;;;; algorithm may not terminate.
;;;;
;;;; ...and that's it, but that's a lot of jargon. Intuitively, the
;;;; meet operation should be whatever you'd do to combine pools
;;;; from different arcs to the same instruction. For example, for
;;;; liveness, each pool is just the set of live variables. If an
;;;; instruction I has two successors with live variable sets X & Y,
;;;; the set of live variables after I should be the union of
;;;; X and Y (before starting on I-specific stuff). So meet is just
;;;; set union. Union is usually a "join" operation, but join and
;;;; meet are duals, meaning that union is a fine meet as long as
;;;; you remember to flip a few things.
;;;;
;;;; An additional function, POOL-ENTRY, should return the entry
;;;; pool for a given instruction. This means that, if the
;;;; instruction is one of the first processed by the algorithm, it
;;;; should have this pool.

(defgeneric pool-meet (specialization pool1 pool2))

(defgeneric pool<= (specialization pool1 pool2))
