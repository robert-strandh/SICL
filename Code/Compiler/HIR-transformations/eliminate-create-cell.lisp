(cl:in-package #:sicl-hir-transformations)

;;; Return an EQ hash table that maps each instruction of type
;;; INSTRUCTION-TYPE to its owner.  One day, we may have to extend
;;; this functionality to give an arbitrary number of "owners" for
;;; each instruction, for example when a function can have multiple
;;; entry points.
(defun compute-instruction-owners (initial-instruction instruction-type)
  (let ((result (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (typep instruction instruction-type)
         (setf (gethash instruction result) owner)))
     initial-instruction)
    result))

;;; Return an EQ hash table that maps each CREATE-CELL-INSTRUCTION to
;;; its owner.
(defun compute-create-cell-owners (initial-instruction)
  (compute-instruction-owners initial-instruction
                              'cleavir-ir:create-cell-instruction))

;;; Return an EQ hash table that maps each ENCLOSE-INSTRUCTION to its
;;; owner.
(defun compute-enclose-owners (initial-instruction)
  (compute-instruction-owners initial-instruction
                              'cleavir-ir:enclose-instruction))

;;; Given an ENTER-INSTRUCTION, return a list of all the
;;; ENCLOSE-INSTRUCTIONs that have that ENTER-INSTRUCTION as its CODE.
(defun find-enclose-instructions (initial-instruction enter-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (and (typep instruction 'cleavir-ir:enclose-instruction)
                  (eq (cleavir-ir:code instruction) enter-instruction))
         (push instruction result)))
     initial-instruction)
    result))

(defun find-create-cell-instructions (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:create-cell-instruction)
         (push instruction result)))
     initial-instruction)
    result))

;;; Given a list of CREATE-CELL-INSTRUCTIONs, return a list of
;;; "transfer items" induced by those CREATE-CELL-INSTRUCTIONs.  A
;;; transfer item is a list (ENT ENC1 ... ENCn) where ENT is an
;;; ENTER-INSTRUCTION and each ENCi is an ENCLOSE-INSTRUCTION having
;;; ENT as its CODE.  A transfer item is induced by a
;;; CREATE-CELL-INSTRUCTION C if and only if there is a path in the
;;; HIR graph from the initial instruction to C.
;;;
;;; The concept of a transfer item is important, because every
;;; ENCLOSE-INSTRUCTION in a transfer item must have an imported
;;; lexical variable added to the end of its import list, and the
;;; ENTER-INSTRUCTION in the item must have an additional FETCH
;;; instruction following it, and that FETCH must fetch the item in
;;; the static environment corresponding to the new imported lexical
;;; variable in the ENCLOSE instructions.
(defun compute-transfer-items (initial-instruction create-cell-instructions)
  (let ((create-cell-owners (compute-create-cell-owners initial-instruction))
        (enclose-owners (compute-enclose-owners initial-instruction))
        (visited (make-hash-table :test #'eq))
        (result '()))
    (labels ((process-enter (enter-instruction)
               (unless (or (gethash enter-instruction visited)
                           (eq enter-instruction initial-instruction))
                 (setf (gethash enter-instruction visited) t)
                 (let ((enclose-instructions
                         (find-enclose-instructions initial-instruction
                                                    enter-instruction)))
                   (push (cons enter-instruction enclose-instructions)
                         result)
                   (loop for enclose in enclose-instructions
                         for owner = (gethash enclose enclose-owners)
                         do (process-enter owner))))))
      (loop for create-cell in create-cell-instructions
            for owner = (gethash create-cell create-cell-owners)
            do (process-enter owner)))
    result))
