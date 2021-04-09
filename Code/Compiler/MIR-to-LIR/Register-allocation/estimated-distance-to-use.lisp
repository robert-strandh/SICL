(cl:in-package #:sicl-register-allocation)


;;; During computation of estimated distance to use, these two
;;; variables will each hold a hash table mapping instructions to
;;; pools.
(defvar *input-pools*)
(defvar *output-pools*)

(defun input-pool (instruction)
  (gethash instruction *input-pools*))

(defun (setf input-pool) (input-pool instruction)
  (setf (gethash instruction *input-pools*) input-pool))

(defun output-pool (instruction)
  (gethash instruction *output-pools*))

(defun (setf output-pool) (output-pool instruction)
  (setf (gethash instruction *output-pools*) output-pool))

;;; Return a list of the inputs of INSTRUCTION that are
;;; lexical-locations.
(defun lexical-location-inputs (instruction)
  (loop for input in (cleavir-ir:inputs instruction)
        when (typep input 'cleavir-ir:lexical-location)
          collect input))

(defun initialize (work-list)
  (loop until (emptyp work-list)
        do (let ((instruction (pop-item work-list)))
             (setf (input-pool instruction) (make-pool))
             (loop for input in (lexical-location-inputs instruction)
                   do (setf (input-pool instruction)
                            (add-variable
                             (input-pool instruction) input))))))

(defgeneric compute-new-output-pool (instruction back-arcs))

(defmethod compute-new-output-pool (instruction back-arcs)
  (let ((successors (cleavir-ir:successors instruction)))
    (ecase (length successors)
      (1 (input-pool (first successors)))
      (2 (destructuring-bind (first second) successors
           (pool-meet (if (gethash first back-arcs)
                          (if (gethash second back-arcs) 1/2 9/10)
                          (if (gethash second back-arcs) 1/10 1/2))
                      (input-pool first)
                      (input-pool second)))))))

(defmethod compute-new-output-pool
    ((instruction cleavir-ir:catch-instruction) back-arcs)
  (input-pool (first (cleavir-ir:successors instruction))))

(defun call-instruction-p (instruction)
  (typep instruction
         '(or
           cleavir-ir:funcall-instruction
           cleavir-ir:named-call-instruction
           cleavir-ir:catch-instruction
           cleavir-ir:bind-instruction
           cleavir-ir:unwind-instruction
           cleavir-ir:initialize-values-instruction
           cleavir-ir:enclose-instruction
           cleavir-ir:multiple-value-call-instruction)))

;;; The derived input pool is a prototype input pool that is
;;; determined from the output pool without taking into account
;;; whether a lexical location exists among the inputs of the
;;; instruction.  So it is the output pool, minus the entries
;;; corresponding to lexical locations in the output of the
;;; instruction, and with each distance incremented.  If the
;;; instruction is a call instruction, then the call probability of an
;;; entry in the derived input pool is 10.  Otherwise, it is the
;;; preserved probability of the entry in the output pool it is
;;; derived from.
(defun compute-derived-input-pool (instruction)
  (let ((call-instruction-p (call-instruction-p instruction))
        (outputs (cleavir-ir:outputs instruction)))
    (loop for entry in (output-pool instruction)
          for lexical-location = (lexical-location entry)
          for distance = (distance entry)
          for call-probability = (call-probability entry)
          unless (member lexical-location outputs :test #'eq)
            collect (make-instance 'pool-item
                      :lexical-location lexical-location
                      :distance (1+ distance)
                      :call-probability
                      (if call-instruction-p 10 call-probability)))))

(defgeneric compute-new-input-pool (instruction))

;;; We first compute the derived input pool.  Then, for every lexical
;;; input, if it appears in the derived input pool, we just set the
;;; distance of that entry to 0.  Otherwise, we add a new entry with
;;; distance 0, and probability 0.
(defmethod compute-new-input-pool (instruction)
  (let* ((derived (compute-derived-input-pool instruction))
         (result derived)
         (inputs (lexical-location-inputs instruction)))
    (loop for input in inputs
          for entry = (find input derived
                            :test #'eq :key #'lexical-location)
          do (if (null entry)
                 (push (make-instance 'pool-item
                         :lexical-location input
                         :distance 0
                         :call-probability 0)
                       result)
                 (reinitialize-instance entry :distance 0)))
    result))

(defgeneric compute-estimated-distance-to-use-for-instruction
    (instruction back-arcs))

(defmethod compute-estimated-distance-to-use-for-instruction
    (instruction back-arcs)
  (let ((new-output-pool
          (compute-new-output-pool instruction back-arcs)))
    (if (pool<= new-output-pool (output-pool instruction))
        '()
        (progn
          (setf (output-pool instruction) new-output-pool)
          (let ((new-input-pool
                  (compute-new-input-pool instruction)))
            (if (pool<= new-input-pool (input-pool instruction))
                '()
                (progn
                  (setf (input-pool instruction) new-input-pool)
                  (cleavir-ir:predecessors instruction))))))))

;;; Compute estimated distance to use for a single function, defined
;;; by an ENTER-INSTRUCTION.
(defun compute-estimated-distance-to-use
    (enter-instruction back-arcs)
  (let ((work-list (make-instance 'work-list)))
    (cleavir-ir:map-local-instructions
     (lambda (instruction) (push-item work-list instruction))
     enter-instruction)
    (initialize work-list)
    (cleavir-ir:map-local-instructions
     (lambda (instruction)
       (unless (null (cleavir-ir:successors instruction))
         (push-item work-list instruction)))
     enter-instruction)
    (loop until (emptyp work-list)
          do (let* ((instruction-to-process (pop-item work-list))
                    (additional-instructions
                      (compute-estimated-distance-to-use-for-instruction
                       instruction-to-process
                       back-arcs)))
               (loop for instruction in additional-instructions
                     do (push-item work-list instruction))))))
