(cl:in-package #:cleavir-simple-value-numbering)

;;; We use instances of this class to designate values.
(defclass value-designator ()
  ())

;;; A list of instances of this class is used to represent a
;;; partition.
(defclass entry ()
  ((%location :initarg :location :reader location)
   (%value :initarg :value :reader value)
   (%new-p :initarg :new-p :reader new-p)))

(defun new-value (location)
  (make-instance 'entry
    :location location
    :value (make-instance 'value-designator)
    :new-p t))

(defun inherit-value (location value)
  (make-instance 'entry
    :location location
    :value value
    :new-p nil))

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
  (remove location partition :key #'location :test #'eq))

(defun add-equivalence (location1 location2 partition)
  (inherit-value location1
		 (location (find location2 partition
				 :test #'eq
				 :key #'location))))

;;; Compute the intersection of two partitions.
;;;
;;; If a location is present in a pair in only one of the partitions,
;;; then keep that pair in the intersection.  If a location is present
;;; in a pair in both partitions, and the two pairs have the same
;;; value designator, then keep a single copy in the intersection.
;;; Otherwise, i.e., if the location is present in a pair in both
;;; partitions but with different value designators, then keep a pair
;;; with the location in the result, but with NIL in place of the
;;; value designator.
(defun partition-intersection (partition1 partition2)
  (let ((result partition1))
    (loop for pair2 in partition2
	  for (location . designator2) = pair2
	  for pair1 = (assoc location result :test #'eq)
	  for designator1 = (cdr pair1)
	  do (cond ((null pair1)
		    (push pair2 result))
		   ((eq designator1 designator2)
		    nil)
		   (t
		    (setf result
			  (cons (cons location nil)
				(remove pair1 result :test #'eq))))))
    result))

;;; Keep a pair only if the location is live.
(defun filter-partition (partition live-locations)
  (remove-if-not (lambda (pair)
		   (member (car pair) live-locations :test #'eq))
		 partition))

(defun simple-value-numbering (initial-instruction)
  (declare (ignore initial-instruction))
  (let ((*constant-designators* (make-hash-table :test #'eq)))))

;;  LocalWords:  designator designators
