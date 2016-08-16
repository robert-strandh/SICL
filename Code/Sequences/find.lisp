(cl:in-package :sicl-sequence)

(defun find-list (item list from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (with-test-and-test-not (test test-not)
    (with-from-end from-end
      (for-each-relevant-cons (cons list start end from-end)
	(let ((element (apply-key-function (car cons) key)))
	  (when (satisfies-two-argument-test-p item element test test-not)
	    (return-from find-list element)))))))

;;; A version of FIND, specialized to a vector.  ITEM is the item to
;;; find.  VECTOR is the vector to search.  FROM-END is a generalized
;;; Boolean indicating the direction of the search.  At least one of
;;; TEST and TEST-NOT must be NIL.  If both are NIL it is as if TEST
;;; had the value #'EQL.  If one of TEST and TEST-NOT is not NIL, it
;;; must be a designator for a function.  START and END must be
;;; non-negative fixnums.  KEY must be a designator for function.
(defun find-vector (item vector from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (with-test-and-test-not (test test-not)
    (with-from-end from-end
      (with-element-type vector
	(for-each-relevant-element (e index vector start end from-end)
	  (let ((element (apply-key-function (car e) key)))
	    (when (satisfies-two-argument-test-p item element test test-not)
	      (return-from find-vector element))))))))

(defun find-aux (item sequence from-end test test-not start end key)
  ;; (declare (optimize (debug 0) (speed 3) (safety 0))
  ;; 	   (type (and fixnum (integer 0)) start)
  ;; 	   (type (or null (and fixnum (integer 0))) end))
  (unless (and (numberp start) (>= start 0))
    (error "invalid start value"))
  (unless (or (null end) (and (numberp end) (>= end 0)))
    (error "invalid end value"))
  (when (and (not (null test)) (not (null test-not)))
    (error "both test and test-not given"))
  (macrolet
      ((test-passes-p (element)
	 `(if (null test-not)
	      (cond ((or (null test) (eq test #'eql) (eq test 'eql))
		     (cond ((or (null key) (eq key #'identity) (eq key 'identity))
			    (eql item ,element))
			   ((or (eq key #'car) (eq key 'car))
			    (eql item (car ,element)))
			   (t
			    (eql item (funcall key ,element)))))
		    ((or (eq test #'eq) (eq test 'eq))
		     (cond ((or (null key) (eq key #'identity) (eq key 'identity))
			    (eq item ,element))
			   ((or (eq key #'car) (eq key 'car))
			    (eq item (car ,element)))
			   (t
			    (eq item (funcall key ,element)))))
		    (t
		     (cond ((or (null key) (eq key #'identity) (eq key 'identity))
			    (funcall test item ,element))
			   ((or (eq key #'car) (eq key 'car))
			    (funcall test item (car ,element)))
			   (t
			    (funcall test item (funcall key ,element))))))
	      (cond ((or (eq test-not #'eql) (eq test-not 'eql))
		     (cond ((or (null key) (eq key #'identity) (eq key 'identity))
			    (not (eql item ,element)))
			   ((or (eq key #'car) (eq key 'car))
			    (not (eql item (car ,element))))
			   (t
			    (not (eql item (funcall key ,element))))))
		    ((or (eq test-not #'eq) (eq test-not 'eq))
		     (cond ((or (null key) (eq key #'identity) (eq key 'identity))
			    (not (eq item ,element)))
			   ((or (eq key #'car) (eq key 'car))
			    (not (eq item (car ,element))))
			   (t
			    (not (eq item (funcall key ,element))))))
		    (t
		     (cond ((or (null key) (eq key #'identity) (eq key 'identity))
			    (not (funcall test-not item ,element)))
			   ((or (eq key #'car) (eq key 'car))
			    (not (funcall test-not item (car ,element))))
			   (t
			    (not (funcall test-not item (funcall key ,element))))))))))
    (if (listp sequence)
	(let ((list-temp (loop for temp = sequence then (cdr temp)
			       for i of-type fixnum = start then (1- i)
			       while (consp temp)
			       until (zerop i)
			       finally (cond ((not (listp temp))
					      (error "not a proper list"))
					     ((plusp i)
					      (error "invalid start value"))
					     (t
					      (return temp))))))
	  (when (and (numberp end) (< end start))
	    (error "invalid start/end pair"))
	  (if from-end
	      (loop with result = nil
		    for pos of-type fixnum from start
		    for temp = list-temp then (cdr temp)
		    while (consp temp)
		    until (if (null end) nil (>= pos end))
		    do (when (test-passes-p (car temp))
			 (setq result (car temp)))
		    finally (cond ((not (listp temp))
				   (error "not a proper list"))
				  ((and (numberp end) (> end pos))
				   (error "invalid end value"))
				  (t
				   (return result))))
	      (loop for pos of-type fixnum from start
		    for temp = list-temp then (cdr temp)
		    while (consp temp)
		    until (if (null end) nil (>= pos end))
		    do (when (test-passes-p (car temp))
			 (return (car temp)))
		    finally (cond ((not (listp temp))
				   (error "not a proper list"))
				  ((and (numberp end) (> end pos))
				   (error "invalid end value"))
				  (t
				   (return nil))))))
	(locally (declare (type vector sequence))
	  (let ((length (length sequence)))
	    (unless (<= 0 start length)
	      (error "invalid start value"))
	    (unless (or (null end) (<= 0 end length))
	      (error "invalid end value"))
	    (when (and (numberp end) (< end start))
	      (error "invalid start/end pair"))
	    (if from-end
		(loop with real-end = (if (null end) (length sequence) end)
		      with last = (1- real-end)
		      for pos downfrom last to start
		      do (when (test-passes-p (aref sequence pos))
			   (return (aref sequence pos))))
		(loop with real-end = (if (null end) (length sequence) end)
		      for pos from start below real-end
		      do (when (test-passes-p (aref sequence pos))
			   (return (aref sequence pos))))))))))
