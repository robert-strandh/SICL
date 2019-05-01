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

;;; Return an EQ hash table that maps each CREATE-CELL instruction to
;;; its owner.
(defun compute-create-cell-owners (initial-instruction)
  (compute-instruction-owners initial-instruction
                              'cleavir-ir:create-cell-instruction))

;;; Return an EQ hash table that maps each ENCLOSE-INSTRUCTION to its
;;; owner.
(defun compute-enclose-owners (initial-instruction)
  (compute-instruction-owners initial-instruction
                              'cleavir-ir:enclose-instruction))

