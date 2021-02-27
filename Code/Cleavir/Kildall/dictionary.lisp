(in-package #:cleavir-kildall)

;;;; Dictionaries are the ultimate result of the algorithm and form
;;;; maps from instructions to pools. They're hash tables.

(declaim (inline make-dictionary
		 instruction-pool (setf instruction-pool)))

(defun make-dictionary ()
  (make-hash-table :test 'eq))

;;; note that the present-p value is actually needed.
(defun instruction-pool (instruction dictionary)
  (gethash instruction dictionary))
(defsetf instruction-pool (instruction dictionary) (pool)
  `(setf (gethash ,instruction ,dictionary) ,pool))

;;;; The *dictionary* variable holds the dictionary used throughout
;;;; an entire Kildall-ing.

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

;;; Return the pool for an instruction if it exists. If not make
;;; one in some specialized way, and put it there.
(declaim (inline maybe-instruction-pool))
(defun maybe-instruction-pool (specialization instruction)
  (multiple-value-bind (pool present-p)
      (gethash instruction *dictionary*)
    (if present-p
        pool
        (setf (gethash instruction *dictionary*)
              (make-pool specialization instruction)))))

;;; We also sometimes need to know whether an instruction has a
;;; pool, because not having a pool is Kildall's distinguished _1_
(declaim (inline pool-present-p))
(defun pool-present-p (specialization instruction)
  (declare (ignore specialization))
  (nth-value 1 (gethash instruction *dictionary*)))
