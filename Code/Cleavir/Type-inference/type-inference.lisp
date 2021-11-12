(cl:in-package #:cleavir-type-inference)

(defun compute-initial-work-list (initial-instruction)
  (let ((result '()))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (unless (typep instruction 'cleavir-ir:enter-instruction)
         (push instruction result)))
     initial-instruction)
    result))

(defun compute-initial-dictionary (initial-instruction liveness)
  (let ((result (make-dictionary)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (loop for predecessor in (remove-duplicates
                                 (cleavir-ir:predecessors instruction))
             for live = (cleavir-liveness:live-before liveness instruction)
             do (loop for var in live
                      when (typep var 'cleavir-ir:lexical-location)
                        do (push (cons var t)
                                 (arc-bag predecessor instruction
                                          result))
                      when (typep var 'cleavir-ir:values-location)
                        do (push (cons var (values-top))
                                 (arc-bag predecessor instruction
                                          result)))))
     initial-instruction)
    result))

(defun infer-types (initial-instruction
                    &key (liveness (cleavir-liveness:liveness
                                    initial-instruction)))
  (let ((*work-list* (compute-initial-work-list initial-instruction))
        (*dictionary* (compute-initial-dictionary initial-instruction liveness)))
    (loop until (null *work-list*)
          do (process-instruction (pop *work-list*)))
    *dictionary*))

(defun inferred-enter-types (initial-instruction types)
  ;; Given how few enter instructions are usually around, using a hash
  ;; table is a bit overkill.
  (let ((result (make-hash-table :test 'eq)))
    ;; First put all the enter instructions in the result.  Default
    ;; value is bottom, which will remain if it never returns
    ;; normally.
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (instruction)
       (when (typep instruction 'cleavir-ir:enter-instruction)
         (setf (gethash instruction result) (values-bottom))))
     initial-instruction)
    ;; Now find all return instructions. join the values at each
    ;; with whatever is already known.
    (cleavir-ir:map-instructions-with-owner
     (lambda (instruction owner)
       (when (typep instruction 'cleavir-ir:return-instruction)
         (setf (gethash owner result)
               (values-binary-join
                (gethash owner result)
                (find-type (first (cleavir-ir:inputs instruction))
                           (instruction-input instruction types))))))
     initial-instruction)
    ;; Finally, make it a bit more presentable.
    (maphash (lambda (k v)
               (setf (gethash k result)
                     `(function * ,(values-descriptor->type v))))
             result)
    result))
