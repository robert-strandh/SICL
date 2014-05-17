(in-package #:cleavir-mir)

;;;; Prune an instruction graph.  The input is the initial instruction
;;;; of a valid instruction graph.  Pruning it consists of removing
;;;; instructions that can not be reached from the initial
;;;; instruction.
;;;;
;;;; The general idea is as follows:
;;;;
;;;;   1. Find all instructions reachable from the initial instruction
;;;;      by following successors.  Call this set A.
;;;;
;;;;   2. Find pairs of instructions X and Y, such that X is NOT a
;;;;      member of A and Y IS a member of A, and Y is a successor of
;;;;      X.  Notice that Y must have at least two predecessors.  For
;;;;      each such pair, remove X as a predecessor of Y.
;;;;
;;;; Removing X as a predecessor of Y is trivial unless Y is a
;;;; PHI-INSTRUCTION, because it suffices to remove X from the list of
;;;; predecessors in Y.
;;;;
;;;; If Y is a PHI-INSTRUCTION, on the other hand, things get a bit
;;;; more complicated.  Let Y0 = Y, and let Yi+1 be a successor of Yi
;;;; such that Yi is the only predecessor of Yi+1 and Yi+1 is a
;;;; PHI-INSTRUCTION.  The instruction graph is valid, so Yi and Yi+1
;;;; have the same number of inputs.  To remove X as a predecessor of
;;;; Y0:
;;;;
;;;;   1. Remove the k:th input of each Yi, where k is the position of
;;;;      X in the list of inputs of Y0.  If the result of this
;;;;      operation is that the length of the list of inputs is 1,
;;;;      then replace the PHI-INSTRUCTION with an assignment
;;;;      instruction.
;;;;
;;;;   2. Remove X from the list of inputs Y0.
