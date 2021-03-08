(cl:in-package #:sicl-register-allocation)

(defun initialize (work-list input-pool)
  (loop until (emptyp work-list)
        do (let ((instruction (pop-item work-list)))
             (setf (gethash instruction input-pool) (make-pool))
             (loop for input in (cleavir-ir:inputs instruction)
                   when (typep input 'cleavir-ir:lexical-location)
                     do (setf (gethash instruction input-pool)
                              (add-variable
                               (gethash instruction input-pool) input))))))

(defgeneric compute-new-output-pool (instruction input-pool back-arcs))

(defmethod compute-new-output-pool (instruction input-pool back-arcs)
  (let ((successors (cleavir-ir:successors instruction)))
    (ecase (length successors)
      (1 (gethash (first successors) input-pool))
      (2 (destructuring-bind (first second) successors
           (pool-meet (if (gethash first back-arcs)
                          (if (gethash second back-arcs) 1/2 9/10)
                          (if (gethash second back-arcs) 1/10 1/2))
                      (gethash first input-pool)
                      (gethash second input-pool)))))))

(defmethod compute-new-output-pool
    ((instruction cleavir-ir:catch-instruction) input-pool back-arcs)
  (gethash (first (cleavir-ir:successors instruction)) input-pool))

(defgeneric compute-new-input-pool (instruction output-pool))

(defmethod compute-new-input-pool (instruction output-pool)
  (let ((result (gethash instruction output-pool)))
    (loop for output in (cleavir-ir:outputs instruction)
          do (setf result (remove-variable result output)))
    (setf result (increment-all-distances result))
    (loop for input in (cleavir-ir:inputs instruction)
          when (typep input 'cleavir-ir:lexical-location)
            do (let* ((entry (find input result
                                   :test #'eq :key #'lexical-location))
                      (p (if (null entry) 0 (call-probability entry))))
                 (setf result
                       (cons (make-pool-item input 0 p)
                             (if (null entry)
                                 result
                                 (remove entry result :test #'eq))))))
    result))

(defgeneric handle-instruction
    (instruction input-pool output-pool back-arcs))

(defmethod handle-instruction
    (instruction input-pool output-pool back-arcs)
  (let ((new-output-pool
          (compute-new-output-pool instruction input-pool back-arcs)))
    (if (pool<= new-output-pool (gethash instruction output-pool))
        '()
        (progn
          (setf (gethash instruction output-pool) new-output-pool)
          (let ((new-input-pool
                  (compute-new-input-pool instruction output-pool)))
            (if (pool<= new-input-pool (gethash instruction input-pool))
                '()
                (progn
                  (setf (gethash instruction input-pool) new-input-pool)
                  (cleavir-ir:predecessors instruction))))))))

;;; Compute estimated distance to use for a single function, defined
;;; by an ENTER-INSTRUCTION.
(defun compute-estimated-distance-to-use
    (enter-instruction back-arcs)
  (let ((work-list (make-instance 'work-list))
        (input-pool (make-hash-table :test #'eq))
        (output-pool (make-hash-table :test #'eq)))
    (cleavir-ir:map-local-instructions
     (lambda (instruction) (push-item work-list instruction))
     enter-instruction)
    (initialize work-list input-pool)
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (unless (null (cleavir-ir:successors instruction))
         (push-item work-list instruction)))
     enter-instruction)
    (loop until (emptyp work-list)
          do (let* ((instruction-to-process (pop-item work-list))
                    (additional-instructions
                      (handle-instruction
                       instruction-to-process
                       input-pool
                       output-pool
                       back-arcs)))
               (loop for instruction in additional-instructions
                     do (push-item work-list instruction))))
    (values input-pool output-pool)))
