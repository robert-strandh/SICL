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

(defvar *variables-hash*)

(defun compute-variables-hash (initial-instruction)
  (let ((id 0)
        (result (make-hash-table :test #'eq)))
    (flet ((try (datum)
             (when (and (cleavir-ir:variable-p datum)
                        (not (nth-value 1 (gethash datum result))))
               (setf (gethash datum result) id)
               (incf id))))
      (cleavir-ir:map-instructions-arbitrary-order
       (lambda (i)
         (mapc #'try (cleavir-ir:inputs i))
         (mapc #'try (cleavir-ir:outputs i)))
       initial-instruction))
    result))

(defmethod cleavir-kildall:make-pool
    ((s liveness-traverse) instruction)
  (declare (ignore instruction))
  (make-array (hash-table-count *variables-hash*)
              :element-type 'bit :initial-element 0))

(defmethod find-in-pool ((s liveness-traverse) variable pool)
  (sbit pool (gethash variable *variables-hash*)))

(defvar *input-table*)
(defvar *output-table*)

(defun make-pool-from-vars (vars)
  (let ((pool (make-array (hash-table-count *variables-hash*)
                          :element-type 'bit :initial-element 0)))
    (loop for var in vars
          do (setf (sbit pool (gethash var *variables-hash*))
                   1))
    pool))

(defun initialize-tables (initial-instruction)
  (cleavir-ir:map-instructions-arbitrary-order
   (lambda (i)
     (setf (gethash i *input-table*)
           (make-pool-from-vars
            (remove-if-not #'cleavir-ir:variable-p
                           (cleavir-ir:inputs i)))
           (gethash i *output-table*)
           (make-pool-from-vars
            (remove-if-not #'cleavir-ir:variable-p
                           (cleavir-ir:outputs i)))))
   initial-instruction))

(defun bits->list (pool)
  (loop for var being the hash-keys of *variables-hash*
          using (hash-value index)
        unless (zerop (sbit pool index))
          collect var))

(defvar *scratch-pool*) ; note each thread would need one of each.
(defvar *subbit-pool*)

(defmethod cleavir-kildall:kildall :around
    ((s liveness-traverse) initial-instruction &key)
  (let* ((*variables-hash*
           (compute-variables-hash initial-instruction))
         (*input-table* (make-hash-table :test #'eq))
         (*output-table* (make-hash-table :test #'eq))
         (*scratch-pool*
           (make-array (hash-table-count *variables-hash*)
                       :element-type 'bit :initial-element 0))
         (*subbit-pool*
           (make-array (hash-table-count *variables-hash*)
                       :element-type 'bit :initial-element 0)))
    (initialize-tables initial-instruction)
    (let ((result (call-next-method)))
      ;; convert to list-set.
      (maphash (lambda (var pool)
                 (setf (gethash var result)
                       (bits->list pool)))
               result)
      result)))

(defmacro do-all-predecessors ((name instruction) &body body)
  `(loop for ,name in (cleavir-ir:predecessors ,instruction)
         do (tagbody ,@body)))

(defun subbitp (bv1 bv2)
  ;; this seems like a dumb way to find zero-ness, but I think
  ;; it's the only game in town for bit vectors?
  (every #'zerop (bit-andc2 bv1 bv2 *subbit-pool*)))

(defmethod cleavir-kildall:transfer
    ((s liveness-traverse) instruction)
  (declare (optimize speed space))
  (let ((pool
          (cleavir-kildall:maybe-instruction-pool s instruction))
        (input (gethash instruction *input-table*))
        (output (gethash instruction *output-table*)))
    (declare (type simple-bit-vector pool input output
                   *scratch-pool*))
    (bit-andc2 pool output *scratch-pool*)
    (bit-ior *scratch-pool* input t) ; t = write into scratch
    (do-all-predecessors (pred instruction)
      (let ((dest (cleavir-kildall:maybe-instruction-pool s pred)))
        (declare (type simple-bit-vector dest))
        (unless (subbitp *scratch-pool* dest)
          (replace dest *scratch-pool*)
          (cleavir-kildall:add-work pred))))))

(defun liveness (initial-instruction)
  (let* ((s (make-instance 'liveness-traverse))
	 (after (cleavir-kildall:kildall s initial-instruction))
	 (before (make-hash-table :test #'eq)))
    (cleavir-ir:map-instructions-arbitrary-order
     (lambda (i)
       (let* ((after-set (gethash i after))
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
  (gethash node (after liveness)))
