(cl:in-package #:cleavir-partial-inlining)

(defmethod find-in-mapping ((mapping hash-table) from)
  (nth-value 0 (gethash from mapping)))

(defmethod add-to-mapping ((mapping hash-table) from to)
  (setf (gethash from mapping) to))

(defmethod copy-mapping ((mapping hash-table))
  (let ((result (make-hash-table :test #'eq)))
    (maphash (lambda (key value)
               (setf (gethash key result) value))
             mapping)
    result))
