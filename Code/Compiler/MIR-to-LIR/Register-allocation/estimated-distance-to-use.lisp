(cl:in-package #:sicl-register-allocation)

(defun initialize (work-list input-pool)
  (loop for instruction in work-list
        do (setf (gethash instruction input-pool) (make-pool))
           (loop for input in (cleavir-ir:inputs instruction)
                 when (typep input 'cleavir-ir:lexical-location)
                   do (setf (gethash instruction input-pool)
                            (add-variable input (gethash instruction input-pool))))))

;;; Compute the input estimated distance to use as a function of 
(defgeneric handle-instruction (instruction input-pool output-pool back-arcs))

;;; Compute estimated distance to use for a single function, defined
;;; by an ENTER-INSTRUCTION.
(defun compute-estimated-distance-to-user
    (enter-instruction back-arcs)
  (let ((work-list '())
        (input-pool (make-hash-table :test #'eq))
        (output-pool (make-hash-table :test #'eq)))
    (cleavir-ir:map-local-instructions
     (lambda (instruction) (push instruction work-list))
     enter-instruction)
    (initialize work-list input-pool)
    (loop until (null work-list)
          do (let* ((instruction-to-process (pop work-list))
                    (additional-instructions
                      (handle-instruction
                       instruction-to-process
                       input-pool
                       output-pool
                       back-arcs)))
               (setf work-list (append additional-instructions work-list))))))
