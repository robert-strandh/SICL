(cl:in-package #:sicl-register-allocation)

(defun initialize (work-list input-info)
  (loop for instruction in work-list
        do (setf (gethash instruction input-info) (make-pool))
           (loop for input in (cleavir-ir:inputs instruction)
                 when (typep input 'cleavir-ir:lexical-location)
                   do (let ((pool (make-pool (cons input 0))))
                        (setf (gethash instruction input-info)
                              (pool-meet 1 pool (gethash instruction input-info)))))))

;;; Compute the input estimated distance to use as a function of 
(defgeneric handle-instruction (instruction input-info output-info back-arcs))

;;; Compute estimated distance to use for a single function, defined
;;; by an ENTER-INSTRUCTION.
(defun compute-estimated-distance-to-user
    (enter-instruction back-arcs)
  (let ((work-list '())
        (input-info (make-hash-table :test #'eq))
        (output-info (make-hash-table :test #'eq)))
    (cleavir-ir:map-local-instructions
     (lambda (instruction) (push instruction work-list))
     enter-instruction)
    (initialize work-list input-info)
    (loop until (null work-list)
          do (let* ((instruction-to-process (pop work-list))
                    (additional-instructions
                      (handle-instruction
                       instruction-to-process
                       input-info
                       output-info
                       back-arcs)))
               (setf work-list (append additional-instructions work-list))))))
