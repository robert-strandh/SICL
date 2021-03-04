(cl:in-package #:sicl-mir-to-lir)

;;; Find back arcs in a single function, defined by the
;;; ENTER-INSTRUCTION given as an argument.
(defun find-back-arcs (enter-instruction)
  (let ((result (make-hash-table :test #'eq))
        (visited-p (make-hash-table :test #'eq))
        (on-stack-p (make-hash-table :test #'eq)) )
    (labels ((traverse (predecessor instruction)
               (if (gethash instruction visited-p)
                   (when (gethash instruction on-stack-p)
                     (push instruction (gethash predecessor result)))
                   (progn (setf (gethash instruction visited-p) t)
                          (setf (gethash instruction on-stack-p) t)
                          (loop for successor in (cleavir-ir:successors instruction)
                                do (traverse instruction successor))
                          (setf (gethash instruction on-stack-p) nil)))))
      (traverse nil enter-instruction)
      result)))
