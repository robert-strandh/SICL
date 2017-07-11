(in-package #:cleavir-liveness)

;;; class for holding liveness information.
(defclass liveness ()
  ((%before :initarg :before :reader before)
   (%after :initarg :after :reader after)))

;;; traverse/domain for kildall.
;;; Lists SEEM to be okay
(defclass liveness-traverse
    (cleavir-kildall:iterate-mixin
     cleavir-kildall:start-everywhere-mixin)
  ())

(defmethod cleavir-kildall:make-pool
    ((s liveness-traverse) instruction)
  (declare (ignore instruction))
  nil)

(defmacro do-all-predecessors ((name instruction) &body body)
  `(loop for ,name in (cleavir-ir:predecessors ,instruction)
         do (tagbody ,@body)))

(defmethod cleavir-kildall:transfer
    ((s liveness-traverse) instruction)
  (let* ((pool (cleavir-kildall:maybe-instruction-pool s instruction))
         (inputs
           (remove-if-not #'cleavir-ir:variable-p
                          (cleavir-ir:inputs instruction)))
         (outputs
           (remove-if-not #'cleavir-ir:variable-p
                          (cleavir-ir:outputs instruction)))
         ;; we could use nunion, but this is probably not
         ;; going to be that fast anyway.
         (new (union inputs (set-difference pool outputs))))
    (do-all-predecessors (pred instruction)
      (let ((old (cleavir-kildall:maybe-instruction-pool s pred)))
        (unless (subsetp new old)
          (setf (cleavir-kildall:dictionary-pool pred)
                (union new old))
          (cleavir-kildall:add-work pred))))))

(defun liveness (initial-instruction)
  (let* ((s (make-instance 'liveness-traverse))
	 (after (cleavir-kildall:kildall s initial-instruction))
	 (before (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (let* ((after-set (cleavir-kildall:instruction-pool i after))
              (before-set
                (union (set-difference
                        after-set
                        (remove-if-not #'cleavir-ir:variable-p
                                       (cleavir-ir:outputs i)))
                       (remove-if-not #'cleavir-ir:variable-p
                                      (cleavir-ir:inputs i)))))
         (setf (gethash i before) before-set)))
     initial-instruction)
    (make-instance 'liveness :before before :after after)))

(defun live-before (liveness node)
  (gethash node (before liveness)))

(defun live-after (liveness node)
  (cleavir-kildall:instruction-pool node (after liveness)))
