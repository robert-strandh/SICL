(cl:in-package #:cleavir-path-replication)

(defun move-and-color (instruction red-instructions blue-instructions)
  (move-instruction instruction)
  (let ((successors (cleavir-ir:successors instruction)))
    (setf (gethash (first successors) red-instructions) t)
    (setf (gethash (second successors) blue-instructions) t)))

;;; Replicate all the instructions between DOMINATOR and INSTRUCTION.
;;; This transformation is accomplished by moving INSTRUCTION so that
;;; it precedes its current predecessors until it is precedes
;;; DOMINATOR.
;;;
;;; If DOMINATOR is not a dominator of INSTRUCTION, then this
;;; transformation will fail in some spectacular way.
;;;
;;; Also, if any of the instructions between DOMINATOR and INSTRUCTION
;;; writes a lexical location that is an input of INSTRUCTION, then
;;; this transformation will also fail.
(defun path-replication (instruction dominator)
  (loop with red-instructions = (make-hash-table :test #'eq)
        with blue-instructions = (make-hash-table :test #'eq)
        with work-list = (list instruction)
        until (null work-list)
        do (let* ((instruction (pop work-list))
                  (predecessors (cleavir-ir:predecessors instruction)))
             (assert (not (null predecessors)))
             (cond ((> (length predecessors) 1)
                    (setf work-list
                          (append (replicate-instruction instruction)
                                  work-list)))
                   ((eq (first predecessors) dominator)
                    ;; If the sole predecessor is DOMINATOR, then we
                    ;; move INSTRUCTION, and we are done.
                    (move-and-color instruction
                                    red-instructions
                                    blue-instructions))
                   (t
                    ;; If the sole predecessor is some instruction
                    ;; other than DOMINATOR, then we move INSTRUCTION,
                    ;; but we put it back on WORK-LIST so that it is
                    ;; processed again.
                    (push instruction work-list)
                    (move-and-color instruction
                                    red-instructions
                                    blue-instructions))))))
