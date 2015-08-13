(cl:in-package #:cleavir-equivalent-lexical-locations)

;;; Return true if and only if two sets contain the same objects.  Use
;;; TEST to determine whether two objects are the same.
(defun set-equality (set1 set2 test)
  (and (null (set-difference set1 set2 :test test))
       (null (set-difference set2 set1 :test test))))

;;; Return true if and only if two equivalence classes are the same.
;;; Recall that an equivalence class is a set (represented as a list)
;;; of lexical locations.
(defun class-equality (class1 class2)
  (set-equality class1 class2 #'eq))

;;; Return true if and only if two partitions are the same.  Recall
;;; that a partition is a set (represented as a list) of equivalence
;;; classes.
(defun partition-equality (partition1 partition2)
  (set-equality partition1 partition2 #'class-equality))

;;; Given a partition and a lexical location, return a new partition
;;; that is like the original one, except that the lexical location
;;; has been removed.  Recall that the lexical location is a member of
;;; at most one of the equivalence classes of PARTITION.  Since we do
;;; not include equivalence classes with a single lexical location in
;;; them, we need to remove any such equivalence class before
;;; returning the result.
(defun remove-location (partition location)
  (let ((class (find location partition :test #'member)))
    (cond ((null class)
	   ;; LOCATION is not represented in PARTITION, meaning it is
	   ;; in an equivalence class by itself.  We return the
	   ;; original partition unmodified.
	   partition)
	  ((= (length class) 2)
	   ;; LOCATION is present, and it is in an equivalence class
	   ;; with exactly one more location.  Since we are removing
	   ;; LOCATION from the partition, the other location is from
	   ;; now one by itself in its equivalence class, and since we
	   ;; do not include such equivalence classes in the
	   ;; representation, we remove the entire equivalence class
	   ;; from the result.
	   (remove class partition :test #'eq))
	  (t
	   ;; LOCATION is present, and it is in an equivalence class
	   ;; with at least two more locations.  We need to return a
	   ;; partition that is like the original one, except that we
	   ;; need to replace the class that LOCATION is in by one
	   ;; that no longer contains LOCATION.
	   (cons (remove location class :test #'eq)
		 (remove class partition :test #'eq))))))

;;; Given a partition in which the variable DEFINED is not a member of
;;; any equivalence class, return a new partition which is like the
;;; original one, except that in it, the location DEFINED is in the
;;; same equivalence class as the location USED.  The location USED
;;; may or may not be represented in some equivalence class of
;;; PARTITION.
(defun add-equivalence (partition defined used)
  (let ((class (find used partition :test #'member)))
    (if (null class)
	;; USED is not represented in PARTITION.  We return a new
	;; partition where DEFINED and USED are in an equivalence
	;; class by themselves.
	(cons (list defined used) partition)
	;; USED is a member of CLASS in PARTITION.  We return a new
	;; partition which is like the original one, except that the
	;; original equivalence class that contained USED is replaced
	;; by one that also contains DEFINED.
	(cons (cons defined class)
	      (remove class partition :test #'eq)))))

;;; Return true if and only if CLASS contains a live variable.
(defun class-contains-live-variable-p (class live-locations)
  (not (null (intersection class live-locations))))

;;; Keep a class only if at least one of its variables is live
(defun filter-partition (partition live-locations)
  (remove-if-not (lambda (class)
		   (class-contains-live-variable-p class live-locations))
		 partition))

(defun update-for-meet (instruction partition liveness)
  (let ((temp partition)
	(live-locations (cleavir-liveness:live-after liveness instruction)))
    (loop for output in (cleavir-ir:outputs instruction)
	  do (setf temp (remove-location temp output)))
    (if (typep instruction 'cleavir-ir:assignment-instruction)
	(let ((input (first (cleavir-ir:inputs instruction)))
	      (output (first (cleavir-ir:outputs instruction))))
	  (filter-partition
	   (if (and (typep input 'cleavir-ir:lexical-location)
		    (typep output 'cleavir-ir:lexical-location))
	       (add-equivalence temp output input)
	       temp)
	   live-locations))
	(filter-partition temp live-locations))))

(defun update-for-join (partition1 partition2)
  (let* ((locations1 (reduce #'append partition1 :from-end t))
	 (locations2 (reduce #'append partition2 :from-end t))
	 (common (intersection locations1 locations2 :test #'eq))
	 (p1 (loop for class in partition1
		   for stripped = (intersection class common :test #'eq)
		   when (> (length stripped) 1)
		     collect stripped))
	 (p2 (loop for class in partition2
		   for stripped = (intersection class common :test #'eq)
		   when (> (length stripped) 1)
		     collect stripped))
	 (l1 (reduce #'append p1 :from-end t))
	 (l2 (reduce #'append p2 :from-end t))
	 (c (intersection l1 l2 :test #'eq))
	 (result '()))
    (loop until (null c)
	  do (let* ((location (first c))
		    (class1 (find location p1 :test #'member))
		    (class2 (find location p2 :test #'member))
		    (intersection (intersection class1 class2 :test #'eq))
		    (diff1 (set-difference class1 intersection :test #'eq))
		    (diff2 (set-difference class2 intersection :test #'eq)))
	       (when (null intersection) (break))
	       (setf c (set-difference c intersection :test #'eq))
	       (setf p1 (remove class1 p1 :test #'eq))
	       (setf p2 (remove class2 p2 :test #'eq))
	       (when (> (length intersection) 1)
		 (push intersection result))
	       (unless (null diff1)
		 (push diff1 p1))
	       (unless (null diff2)
		 (push diff2 p2))))
    result))

(defun compute-equivalent-lexical-locations (initial-instruction)
  (cleavir-meter:with-meter (m *equivalent-lexical-locations-meter*)
    (let ((liveness (cleavir-liveness:liveness initial-instruction))
	  (work-list (list initial-instruction))
	  (before (make-hash-table :test #'eq))
	  (after (make-hash-table :test #'eq)))
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
				      (reduce #'update-for-join
					      (mapcar (lambda (p)
							(gethash p after))
						      predecessors)))
		       when (or (null (nth-value 1 (gethash successor before)))
				(not (partition-equality
				      join (gethash successor before))))
			 do (setf (gethash successor before) join)
			    (push successor work-list)
			    (cleavir-meter:increment-size m))))
      before)))
