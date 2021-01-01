(in-package #:cleavir-hir-transformations)

;;; FIXME: move to cleavir-ir
(deftype call-instruction ()
  '(or cleavir-ir:funcall-instruction
    cleavir-ir:multiple-value-call-instruction))

;;; An incremental version of compute destinies.  Compute the
;;; destinies only from a given enclose-instruction.
(defun find-enclose-destinies (enclose-instruction)
  (let ((destinies '())
        (worklist (cleavir-ir:outputs enclose-instruction)))
    (loop (when (null worklist)
            (return destinies))
          (let ((work (pop worklist)))
            ;; Note that we could hit the same work multiple times, so
            ;; we use pushnew liberally.
            (loop for next in (cleavir-ir:using-instructions work)
                  do (typecase next
                       ;; Here is where we could allow other
                       ;; instructions, etc.
                       (cleavir-ir:assignment-instruction
                        (push (first (cleavir-ir:outputs next)) worklist))
                       (call-instruction
                        (if (eq work (first (cleavir-ir:inputs next)))
                            ;; callee
                            (pushnew next destinies :test #'eq)
                            ;; arguments
                            (pushnew :escape destinies :test #'eq)))
                       (t ; treat as unknown
                        (pushnew :escape destinies :test #'eq))))))))

(defun destiny-find-encloses (call-instruction)
  (let ((worklist (list (first (cleavir-ir:inputs call-instruction))))
        (encloses '()))
    (loop (when (null worklist)
            (return encloses))
          (let ((work (pop worklist)))
            ;; Note that we could hit the same work multiple times, so
            ;; we use pushnew liberally.
            (loop for next in (cleavir-ir:defining-instructions work)
                  do (typecase next
                       ;; Here is where we could allow other
                       ;; instructions, etc.
                       (cleavir-ir:assignment-instruction
                        (push (first (cleavir-ir:inputs next)) worklist))
                       (cleavir-ir:enclose-instruction
                        (pushnew next encloses))))))))

;;; Compute a hash table from enclose instructions to "destinies".
;;; A destiny is a list. Elements of the list are either call instructions or :escape.
;;; Each list has no duplicates.
(defun compute-destinies (initial-instruction)
  (let ((destinies (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (when (typep i 'cleavir-ir:enclose-instruction)
         (setf (gethash i destinies)
               (find-enclose-destinies i))))
     initial-instruction)
    destinies))
