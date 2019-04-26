(cl:in-package #:cleavir-hir-transformations)

;;;; Recall that an ENTER-INSTRUCTION is the successor of no other
;;;; instruction.
;;;;
;;;; The ENTER-INSTRUCTIONs of a program constitute a directed tree.
;;;; An ENTER-INSTRUCTION I closer to the root of the tree than some
;;;; other ENTER-INSTRUCTION J is said to be located FURTHER OUT than
;;;; J.
;;;;
;;;; We introduce the concept of OWNERSHIP.  This concept is defined
;;;; for lexical variables and for instructions.  The OWNER of an
;;;; instruction or a lexical variable is an ENTER-INSTRUCTION.
;;;;
;;;; Ownership for instructions is defined as follows:
;;;;
;;;;   * The owner of an ENTER-INSTRUCTION is itself.
;;;;
;;;;   * For all instruction types the owner of the successors of an
;;;;     instruction I is the same as the owner of I.
;;;;
;;;; The owner of a datum D is the outermost ENTER-INSTRUCTION of all
;;;; the owners of all the instructions using D.
;;;;
;;;; Each ENTER-INSTRUCTION A defines a PROCEDURE which is a the set
;;;; of all the instructions owned by A.  We extend the definition of
;;;; ownership so that a procedure P is the owner of some instruction
;;;; or datum X if an only if the unique ENTER-INSTRUCTION of P is the
;;;; owner of X.

;;; Compute the owner of each instruction.  The return value is an EQ
;;; hash table mapping an instruction to its owner
(defun compute-instruction-owners (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (setf (gethash instruction result) owner))
     initial-instruction)
    result))

;;; Compute and return a hash table that maps each lexical location,
;;; whether static or dynamic to its owner.
;;;
;;; The technique used here is to traverse the instructions using
;;; MAP-INSTRUCTIONS-BY/WITH-OWNER which guarantees that the
;;; instruction graph is traversed "top-down", so that the first time
;;; we encounter some lexical location L being by some instruction I,
;;; we know that the owner of L is the same as the owner of I.  If we
;;; encounter L again at some later point, we just check whether it
;;; has been attributed an owner already, in which case we do nothing.
(defun compute-location-owners (initial-instruction)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-with-owner 
     (lambda (instruction owner)
       (loop for datum in (append (cleavir-ir:inputs instruction)
				  (cleavir-ir:outputs instruction))
	     do (when (null (gethash datum result))
		  (setf (gethash datum result) owner))))
     initial-instruction)
    result))

