(cl:in-package #:sicl-hir-to-cl)

;;; Return true if and only if INSTRUCTION is a leader.  We exclude
;;; the ENTER-INSTRUCTION and consider its successor a leader instead.
;;; Furthermore, we do not want a basic block to contain instructions
;;; with different dynamic environments.  Therefore, we also consider
;;; an instruction a leader if it has a predecessor that is in a
;;; different dynamic environment.
(defun leaderp (instruction)
  (let* ((predecessors (cleavir-ir:predecessors instruction))
         (length (length predecessors)))
    (and (not (zerop length))
         (or (> length 1)
             (let ((predecessor (first predecessors)))
               (or (> (length (cleavir-ir:successors predecessor)) 1)
                   (typep predecessor 'cleavir-ir:enter-instruction)
                   (not (eq (cleavir-ir:dynamic-environment-location instruction)
                            (cleavir-ir:dynamic-environment-location predecessor)))))))))

;;; Find the leaders in a single function, i.e. do not follow the CODE
;;; of ENCLOSE instructions.
(defun find-leaders (enter-instruction)
  (let ((visited-p (make-hash-table :test #'eq))
        (result '()))
    (labels ((process-instruction (instruction)
               (unless (gethash instruction visited-p)
                 (setf (gethash instruction visited-p) t)
                 (when (leaderp instruction)
                   (push instruction result))
                 (loop for successor in (cleavir-ir:successors instruction)
                       do (process-instruction successor)))))
      (process-instruction enter-instruction))
    result))
             
(defclass basic-block ()
  ((%instructions :initarg :instructions :accessor instructions)))

(defun compute-basic-blocks (enter-instruction)
  (let ((leaderp (make-hash-table :test #'eq))
        (leaders (find-leaders enter-instruction)))
    (loop for leader in leaders
          do (setf (gethash leader leaderp) t))
    (flet ((compute-basic-block (leader)
             (let ((basic-block (make-instance 'basic-block :instructions (list leader))))
               (loop for last-instruction = (first (instructions basic-block))
                     for successors = (cleavir-ir:successors last-instruction)
                     until (or (/= (length successors) 1)
                               (gethash (first successors) leaderp))
                     do (push (first successors) (instructions basic-block))
                     finally (setf (instructions basic-block)
                                   (nreverse (instructions basic-block)))
                             (return basic-block)))))
      (loop for leader in leaders
            collect (compute-basic-block leader)))))
