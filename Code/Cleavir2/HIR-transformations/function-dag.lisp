(cl:in-package #:cleavir-hir-transformations)

;;;; In this file we define functions for building and traversing a
;;;; FUNCTION DAG.  Such a DAG defines the nesting of functions in a
;;;; program.  A interior node in the graph contains an
;;;; ENCLOSE-INSTRUCTION.  Each node contains a list of CHILDREN that
;;;; are also DAG nodes.
;;;;
;;;; Let N be some interior node.  Let EN be the ENCLOSE-INSTRUCTION
;;;; of N, and let CEN be the CODE input of EN (i.e., an
;;;; ENTER-INSTRUCTION).  Consider some child node M of N, and let EM
;;;; be the ENCLOSE-INSTRUCTION of M.  Then CEN is the owner of EM.
;;;;
;;;; Let R be the root node and CR be the ENTER-INSTRUCTION
;;;; INITIAL-INSTRUCTION passed as an argument to BUILD-FUNCTION-DAG.
;;;; Consider some child S of R, and let ES be the ENCLOSE-INSTRUCTION
;;;; of S.  Then CR is the owner of ES.
;;;;
;;;; In addition to an ENCLOSE-INSTRUCTION and a list of children,
;;;; each interior node also contains a list of parents, each of which
;;;; is another DAG node.
;;;;
;;;; In addition to a list of children, the root node also contains an
;;;; EQ hash table that each ENTER-INSTRUCTION to a list of to DAG
;;;; nodes.  In this table, the ENTER-INSTRUCTION INITIAL-INSTRUCTION
;;;; maps to the singleton list containing the root of the DAG.  Let
;;;; CN be an ENTER-INSTRUCTION of the table other than
;;;; INITIAL-INSTRUCTION, and let N be an element of the list that CN
;;;; maps to.  Let EN be the ENCLOSE-INSTRUCTION of N.  Then CN is the
;;;; CODE input of EN.

(defgeneric enter-instruction (node))

(defclass dag-node ()
  ((%children :initform '() :accessor children)))

(defclass function-dag (dag-node)
  ((%initial-instruction :initarg :initial-instruction
                         :reader initial-instruction)
   (%dag-nodes :initarg :dag-nodes :reader dag-nodes)))

(defmethod enter-instruction ((node function-dag))
  (initial-instruction node))

(defclass interior-node (dag-node)
  ((%parents :initarg :parents :accessor parents)
   (%enclose-instruction :initarg :enclose-instruction
                         :reader enclose-instruction)))

(defmethod enter-instruction ((node interior-node))
  (cleavir-ir:code (enclose-instruction node)))

(defun build-function-dag (initial-instruction)
  (let* (;; Create an EQ hash table that maps ENTER-INSTRUCTIONs to
         ;; DAG nodes such that the DAG node is either the root, or it
         ;; contains the ENCLOSE-INSTRUCTION that the
         ;; ENTER-INSTRUCTION is associated with.
         (table (make-hash-table :test #'eq))
         ;; Create the root of the DAG that is ultimately going to be
         ;; returned as the value of this function.
         (root (make-instance 'function-dag
                 :dag-nodes table
                 :initial-instruction initial-instruction)))
    ;; Enter the root into the table.
    (setf (gethash initial-instruction table) (list root))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (typep instruction 'cleavir-ir:enclose-instruction)
         (let* ((parents (gethash owner table))
                (node (make-instance 'interior-node
                        :parents parents
                        :enclose-instruction instruction)))
           (loop for parent in parents
                 do (push node (children parent)))
           (push node (gethash (cleavir-ir:code instruction) table nil)))))
     initial-instruction)
    root))

;;; Removes an enclose instruction from the function-dag. If INHERIT
;;; is true, then children of the deleted node are reattached to the
;;; parent of the deleted node.
(defun remove-enclose-from-function-dag (dag enclose &optional inherit)
  (let* ((table (dag-nodes dag))
         (enter (cleavir-ir:code enclose))
         (nodes (gethash enter table)))
    (dolist (node nodes)
      (when (eq (enclose-instruction node) enclose)
        (dolist (parent (parents node))
          (setf (children parent)
                (remove node (children parent)))
          (when inherit
            (dolist (child (children node))
              (push child (children parent))
              (nsubstitute parent node (parents child)))))
        (setf (gethash enter table)
              (remove enclose nodes))
        (return)))))

(defun add-enclose-to-parents (enclose parents)
  (let ((node (make-instance 'interior-node
                             :parents parents
                             :enclose-instruction enclose)))
    (dolist (parent parents)
      (push node (children parent)))
    node))
