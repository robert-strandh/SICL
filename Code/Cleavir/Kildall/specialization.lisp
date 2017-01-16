(in-package #:cleavir-kildall)

;;;; A "specialization" is an object that represents the particular
;;;; thing Kildall's algorithm is being used for. The original
;;;; paper uses it for liveness, CSE, register optimization, etc.
;;;;
;;;; In practical terms it's just what all these functions dispatch
;;;; on. It doesn't have to have any slots if it doesn't need them.
;;;;
;;;; Specializations MUST dispatch on:
;;;;  * POOL-MEET
;;;;  * POOL<=
;;;;  * ENTRY-POOL
;;;;  * TRANSFER
;;;; Everything else is optional.

(defclass specialization () ()) ; not really used...

;;; These classes control the type of traversal being done.
;;; "Forward" means "in the direction of control flow"; the
;;; algorithm will start at ENTER instructions and go forward.
;;; "Reverse" means the opposite. The algorithm will start
;;; everywhere, because RETURN instructions don't always exist.
(defclass forward-traverse (specialization) ())
(defclass reverse-traverse (specialization) ())

;;; "spread" means that transfer functions will return exactly one
;;; value, which is used for all successors or predecessors.
;;; "single" means that transfer functions will return n values for
;;; an instruction with n successors/predecessors, and the mth
;;; value will be passed to the mth successor/predecessor.
(defclass forward-spread-traverse (forward-traverse) ())
(defclass forward-single-traverse (forward-traverse) ())
(defclass reverse-spread-traverse (reverse-traverse) ())

;;; This function returns the initial pools used by the algorithm.
;;; The initial work list will consist of whatever instructions
;;; start the traversal, paired with the results of this function.
(defgeneric entry-pool (specialization instruction))
