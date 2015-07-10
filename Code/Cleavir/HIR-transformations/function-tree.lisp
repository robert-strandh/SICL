(cl:in-package #:cleavir-hir-transformations)

;;;; In this file we define functions for building and traversing a
;;;; FUNCTION TREE.  Such a tree defines the nesting of functions in a
;;;; program.  A node (other than the root) of the tree contains a
;;;; slot holding an ENCLOSE-INSTRUCTION.  Each node contains a list
;;;; of CHILDREN that are also tree nodes.  The children of a node N
;;;; (other than the root) containing some ENCLOSE-INSTRUCTION E are
;;;; nodes that contain the ENCLOSE-INSTRUCTIONs owned by the CODE of
;;;; (i.e., the ENTER-INSTRUCTION associated with) E.  The children of
;;;; the root node are nodes containing the ENCLOSE-INSTRUCTIONs owned
;;;; by the ENTER-INSTRUCTION INITIAL-INSTRUCTION.  In addition to a
;;;; list of children, the root node also contains an EQ hash table
;;;; that maps ENTER-INSTRUCTIONs to tree nodes such that the tree
;;;; node is either the root, or it contains the ENCLOSE-INSTRUCTION
;;;; that the ENTER-INSTRUCTION is associated with.

(defclass tree-node ()
  ((%children :initform '() :accessor children)))

(defclass function-tree (tree-node)
  ((%initial-instruction :initarg :initial-instruction
			 :reader initial-instruction)
   (%tree-nodes :initarg :tree-nodes :reader tree-nodes)))

(defclass interior-node (tree-node)
  ((%enclose-instruction :initarg :enclose-instruction
			 :reader enclose-instruction)))

(defun build-function-tree (initial-instruction)
  (let* (;; Create an EQ hash table that maps ENTER-INSTRUCTIONs to
	 ;; tree nodes such that the tree node is either the root, or
	 ;; it contains the ENCLOSE-INSTRUCTION that the
	 ;; ENTER-INSTRUCTION is associated with.
	 (table (make-hash-table :test #'eq))
	 ;; Create the root of the tree that is ultimately going to be
	 ;; returned as the value of this function.
	 (root (make-instance 'function-tree
		 :tree-nodes table
		 :initial-instruction initial-instruction)))
    ;; Enter the root into the table.
    (setf (gethash initial-instruction table) root)
    (cleavir-ir:map-instructions-by/with-owner
     (lambda (instruction owner)
       (when (typep instruction 'cleavir-ir:enclose-instruction)
	 (let ((node (make-instance 'interior-node
		       :enclose-instruction instruction)))
	   (push node (children (gethash owner table)))
	   (setf (gethash (cleavir-ir:code instruction) table) node))))
     initial-instruction)
    root))
