(in-package #:cleavir-kildall)

;;;; Only works when the set of variables is known beforehand and
;;;; is the same for all instructions.
;;;; We skip instruction-variables for this reason.

(defclass bitset-pool-mixin () ())

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

(defmethod make-pool ((specialization bitset-pool-mixin)
                      instruction)
  (make-array (hash-table-count *variables-hash*)
              :element-type 'bit :initial-element 0))

(defmethod find-in-pool ((specialization bitset-pool-mixin)
                         variable pool)
  (sbit pool (gethash variable *variables-hash*)))

(defmethod map-into-pool ((specialization bitset-pool-mixin)
                          function pool)
  (loop for var being the hash-keys of *variables-hash*
          using (hash-value index)
        do (setf (sbit pool index)
                 (funcall function var (sbit pool index)))))

(defmethod pool-meet ((specialization bitset-pool-mixin) o1 o2)
  (declare (bit o1 o2))
  (logior o1 o2))

(defmethod pool<= ((specialization bitset-pool-mixin) o1 o2)
  (declare (bit o1 o2))
  ;; 1 < 0
  (if (zerop o1)
      (zerop o2)
      t))

;;; unused
(defmethod object1 ((specialization bitset-pool-mixin) variable)
  (declare (ignore variable))
  0)

(defun bits->list (pool)
  (loop for var being the hash-keys of *variables-hash*
          using (hash-value index)
        unless (zerop (sbit pool index))
          collect var))

(defmethod kildall :around ((specialziation bitset-pool-mixin)
                            initial-instruction &key)
  (let* ((*variables-hash*
           (compute-variables-hash initial-instruction))
         (result (call-next-method)))
    ;;; now convert out of bitset.
    (maphash (lambda (var pool) (setf (gethash var result)
                                      (bits->list pool)))
             result)
    result))
