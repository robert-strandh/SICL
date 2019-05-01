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
(defun compute-transfer-items (initial-instruction
                               create-cell-instructions
                               create-cell-owners
                               enclose-owners)
  (let ((visited (make-hash-table :test #'eq))
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

;;; Given an ENTER-INSTRUCTION and an index (a non-negative integer),
;;; insert a new FETCH-INSTRUCTION immediately after the
;;; ENTER-INSTRUCTION.  The new FETCH-INSTRUCTION should have an
;;; immediate input with INDEX as its value, and it should have a new
;;; exclusive lexical location as its output.  The new lexical
;;; location is returned.
(defun insert-fetch (enter-instruction index)
  (let* ((immediate-input (make-instance 'cleavir-ir:immediate-input
                           :value index))
         (lexical-output (make-instance 'cleavir-ir:lexical-location
                           :name (gensym "cc")))
         (dynamic-environment
           (cleavir-ir:dynamic-environment-output enter-instruction))
         (fetch (make-instance 'cleavir-ir:fetch-instruction
                  :input immediate-input
                  :output lexical-output
                  :dynamic-environment-location dynamic-environment)))
    (cleavir-ir:insert-instruction-after fetch enter-instruction)
    lexical-output))

;;; Given a list of pairs, each pair consisting of an
;;; ENTER-INSTRUCTION and an index.  For each pair, insert a
;;; FETCH-INSTRUCTION after the ENTER-INSTRUCTION that fetches the
;;; item of the index.  Return a hash table mapping ENTER-INSTRUCTIONs
;;; to the lexical location written to by the new FETCH instruction.
(defun insert-fetches (pairs)
  (let ((result (make-hash-table :test #'eq)))
    (loop for (enter . index) in pairs
          do (setf (gethash enter result)
                   (insert-fetch enter index)))
    result))

;;; Given a list of transfer iterms (see COMPUTE-TRANSFER-ITEMS),
;;; return a list of pairs, each pair consisting of the
;;; ENTER-INSTRUCTION of the transfer item, and the length of the
;;; import list of the first associated ENCLOSE-INSTRUCTION (they
;;; should all have import lists of the same length).
(defun compute-fetch-pairs (transfer-items)
  (loop for (enter enclose . rest)  in transfer-items
        collect (cons enter (length (cleavir-ir:inputs enclose)))))

;;; Eliminate a single CREATE-CELL-INSTRUCTION.  LEXICAL-LOCATION is a
;;; lexical location that contains the function cell containing a
;;; function that creates a cell.  We do it by turning the
;;; CREATE-CELL-INSTRUCTION into a FUNCALL-INSTRUCTION and we precede
;;; the FUNCALL-INSTRUCTION with a CAR-INSTRUCTION taking the CAR of
;;; the function cell.
(defun eliminate-create-cell-instruction (instruction location)
  (let* ((function-location (make-instance 'cleavir-ir:lexical-location
                              :name (gensym "ccfun")))
         (car-instruction (make-instance 'cleavir-ir:car-instruction
                            :input location
                            :output function-location
                            :dynamic-environment-location
                            (cleavir-ir:dynamic-environment-location instruction))))
    (change-class instruction 'cleavir-ir:funcall-instruction
                  :inputs (list function-location))
    (cleavir-ir:insert-instruction-before car-instruction instruction)))

(defun insert-create-cell-function-cell (initial-instruction)
  (let* ((lexical-location (make-instance 'cleavir-ir:lexical-location
                             :name (gensym "ccfcell")))
         (instruction (make-instance 'find-function-cell-instruction
                        :name 'create-cell
                        :inputs '()
                        :output lexical-location
                        :dynamic-environment-location
                        (cleavir-ir:dynamic-environment-output initial-instruction))))
    (cleavir-ir:insert-instruction-after instruction initial-instruction)
    lexical-location))

(defun eliminate-create-cell-instructions (initial-instruction)
  (let* ((create-cell-instructions
           (find-create-cell-instructions initial-instruction))
         (create-cell-owners
           (compute-create-cell-owners initial-instruction))
         (enclose-owners (compute-enclose-owners initial-instruction))
         (transfer-items (compute-transfer-items initial-instruction
                                                 create-cell-instructions
                                                 create-cell-owners
                                                 enclose-owners))
         (fetch-pairs (compute-fetch-pairs transfer-items))
         (locations (insert-fetches fetch-pairs)))
    (setf (gethash initial-instruction locations)
          (insert-create-cell-function-cell initial-instruction))
    (loop for transfer-item in transfer-items
          do (loop for enclose in (rest transfer-item)
                   for owner = (gethash enclose enclose-owners)
                   for location = (gethash owner locations)
                   do (setf (cleavir-ir:inputs enclose)
                            (append (cleavir-ir:inputs enclose)
                                    (list location)))))))
