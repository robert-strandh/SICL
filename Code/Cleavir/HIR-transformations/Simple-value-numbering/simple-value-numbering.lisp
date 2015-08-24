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
;;; If a location is present in an entry in only one of the
;;; partitions, then keep that entry in the intersection.  If a
;;; location is present in an entry in both partitions, and the two
;;; entries have the same value designator, then keep a single copy in
;;; the intersection.  Otherwise, i.e., if the location is present in
;;; an entry in both partitions but with different value designators,
;;; then keep an entry with the location in the result, but with NIL in
;;; place of the value designator.
(defun partition-intersection (partition1 partition2)
  (let ((result partition1))
    (loop for entry2 in partition2
	  for location = (location entry2)
	  for designator2 = (value entry2)
	  for entry1 = (find location result :key #'location :test #'eq)
	  do (if (null entry1)
		 (push entry2 result)
		 (let ((designator1 (value entry1)))
		   (if (and (eq designator1 designator2)
			    (eq (new-p entry1) (new-p entry2)))
		       nil
		       (setf result
			     (cons (new-value location)
				   (remove entry1 result :test #'eq)))))))
    result))

;;; Keep an entry only if the location is live.
(defun filter-partition (partition live-locations)
  (remove-if-not (lambda (entry)
		   (member (location pair) live-locations :test #'eq))
		 partition))

(defun simple-value-numbering (initial-instruction)
  (declare (ignore initial-instruction))
  (let ((*constant-designators* (make-hash-table :test #'eq)))))

;;  LocalWords:  designator designators
