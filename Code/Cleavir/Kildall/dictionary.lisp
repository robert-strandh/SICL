(in-package #:cleavir-kildall)

;;;; Dictionaries are the ultimate result of the algorithm and form
;;;; maps from instructions to pools. They are hash tables.

(declaim (inline make-dictionary
                 instruction-pool
                 (setf instruction-pool)))

(defun make-dictionary ()
  (make-hash-table :test 'eq))

;;; Note that the PRESENT-P value is actually needed.
(defun instruction-pool (instruction dictionary)
  (gethash instruction dictionary))
(defsetf instruction-pool (instruction dictionary) (pool)
  `(setf (gethash ,instruction ,dictionary) ,pool))

;;; This variable holds the dictionary used throughout the entire
;;; execution of the Kildall algorithm.
(defvar *dictionary*)

(defun dictionary-pool (instruction)
  (gethash instruction *dictionary*))
(defsetf dictionary-pool (instruction) (pool)
  `(setf (gethash ,instruction *dictionary*) ,pool))

;;; A default method, since all will have dictionaries.
(defmethod kildall :around (specialization instruction &key)
  (declare (ignore specialization instruction))
  (let ((*dictionary* (make-hash-table :test #'eq)))
    (call-next-method)
    *dictionary*))

;;; Return the pool for an instruction if it exists. If not, make one
;;; in some specialized way, and put it there.
(declaim (inline maybe-instruction-pool))
(defun maybe-instruction-pool (specialization instruction)
  (multiple-value-bind (pool present-p)
      (dictionary-pool instruction)
    (if present-p
        pool
        (setf (dictionary-pool instruction)
              (make-pool specialization instruction)))))

;;; We also sometimes need to know whether an instruction has a pool,
;;; because not having a pool is Kildall's distinguished _1_.
(declaim (inline pool-present-p))
(defun pool-present-p (specialization instruction)
  (declare (ignore specialization))
  (nth-value 1 (gethash instruction *dictionary*)))
