(cl:in-package #:cleavir-simple-value-numbering)

;;; We use instances of this class to designate values.
(defclass value-designator ()
  ())

;;; This variable holds and EQ hash table mapping constants to value
;;; designators.
(defvar *constant-designators*)

(defun constant-designator (constant)
  (let ((result (gethash constant *constant-designators*)))
    (when (null result)
      (setf result (make-instance 'value-designator))
      (setf (gethash constant *constant-designators*) result))
    result))

(defun remove-location (location partition)
  (remove location partition :key #'car :test #'eq))

(defun add-equivalence (location1 location2 partition)
  (cons (cons location1 (cdr (assoc location2 partition :test #'eq)))
	partition))

(defun partition-intersection (partition1 partition2)
  (let ((result partition1))
    (loop for value2 in partition2
	  for (location . designator2) = value2
	  for value1 = (assoc location result :test #'eq)
	  for designator1 = (cdr value1)
	  do (cond ((null value1)
		    (push value2 result))
		   ((eq designator1 designator2)
		    nil)
		   (t
		    (setf result
			  (cons (cons location nil)
				(remove value1 result :test #'eq))))))
    result))

(defun simple-value-numbering (initial-instruction)
  (declare (ignore initial-instruction))
  (let ((*constant-designators* (make-hash-table :test #'eq)))))

;;  LocalWords:  designator designators
