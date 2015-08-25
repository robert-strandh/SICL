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

(defun same-partition-p (new old)
  (and (= (length new) (length old))
       (loop for new-entry in new
	     for location = (location new-entry)
	     for old-entry = (find location old :test #'eq :key location)
	     always (and (not (null old-entry))
			 (or (and (new-p old-entry) (new-p old-entry))
			     (eq (value new-entry) (value old-entry)))))))

;;; Keep an entry only if the location is live.
(defun filter-partition (partition live-locations)
  (remove-if-not (lambda (entry)
		   (member (location entry) live-locations :test #'eq))
		 partition))

(defgeneric update-for-assignment (partition input output))

;;; This function is used in order to update a partition according to
;;; a single instruction.
(defgeneric update-for-meet (instruction partition liveness))

;;; Before returning the result, we filter it according to the
;;; locations that are live after the instruction.
(defmethod update-for-meet :around (instruction partition liveness)
  (let ((live-locations (cleavir-liveness:live-after liveness instruction)))
    (filter-partition (call-next-method) live-locations)))

(defmethod update-for-meet (instruction partition liveness)
  (declare (ignore liveness))
  (let ((temp partition))
    (loop for output in (cleavir-ir:outputs instruction)
	  do (setf temp (remove-location temp output))
	     (push (new-value output) temp))
    temp))

(defmethod update-for-meet
    ((instruction cleavir-ir:assignment-instruction) partition liveness)
  (declare (ignore liveness))
  (let ((temp partition))
    (loop for output in (cleavir-ir:outputs instruction)
	  do (setf temp (remove-location temp output)))
    (let ((input (first (cleavir-ir:inputs instruction)))
	  (output (first (cleavir-ir:outputs instruction))))
      (if (and (typep input 'cleavir-ir:lexical-location)
	       (typep output 'cleavir-ir:lexical-location))
	  (add-equivalence output input temp)
	  temp))))

(defun simple-value-numbering (initial-instruction)
  (cleavir-meter:with-meter (m *simple-value-numbering-meter*)
    (let ((liveness (cleavir-liveness:liveness initial-instruction))
	  (work-list (list initial-instruction))
	  (before (make-hash-table :test #'eq))
	  (after (make-hash-table :test #'eq))
	  (*constant-designators* (make-hash-table :test #'eq)))
      (cleavir-meter:increment-size m)
      (setf (gethash initial-instruction before) '())
      (loop until (null work-list)
	    do (let ((instruction (pop work-list)))
		 (when (typep instruction 'cleavir-ir:enclose-instruction)
		   (let ((enter (cleavir-ir:code instruction)))
		     (setf (gethash enter before) '())
		     (push enter work-list)
		     (cleavir-meter:increment-size m)))
		 (setf (gethash instruction after)
		       (update-for-meet instruction
					(gethash instruction before)
					liveness))
		 (loop for successor in (cleavir-ir:successors instruction)
		       for predecessors = (cleavir-ir:predecessors successor)
		       for join = (if (= (length predecessors) 1)
				      (gethash (first predecessors) after)
				      (reduce #'partition-intersection
					      (mapcar (lambda (p)
							(gethash p after))
						      predecessors)))
		       when (or (null (nth-value 1 (gethash successor before)))
				(not (same-partition-p
				      join (gethash successor before))))
			 do (setf (gethash successor before) join)
			    (push successor work-list)
			    (cleavir-meter:increment-size m))))
      before)))

;;  LocalWords:  designator designators
