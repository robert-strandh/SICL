(cl:in-package :sicl-sequence)

(defun position-list (item list from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (with-test-and-test-not (test test-not)
    (with-from-end from-end
      (for-each-relevant-cons (cons index list start end from-end)
	(let ((element (apply-key-function (car cons) key)))
	  (when (satisfies-two-argument-test-p item element test test-not)
	    (return-from position-list index)))))))

;;; A version of POSITION, specialized to a vector.  ITEM is the item
;;; for wich a position is searched for. VECTOR is the vector to
;;; search.  FROM-END is a generalized Boolean indicating the
;;; direction of the search.  At least one of TEST and TEST-NOT must
;;; be NIL.  If both are NIL it is as if TEST had the value #'EQL.  If
;;; one of TEST and TEST-NOT is not NIL, it must be a designator for a
;;; function.  START and END must be non-negative fixnums.  KEY must
;;; be a designator for function.
(defun position-vector (item vector from-end test test-not start end key)
  (declare (optimize (speed 3) (debug 0) (safety 3)))
  (declare (type fixnum start end))
  (with-test-and-test-not (test test-not)
    (with-from-end from-end
      (with-element-type vector
	(for-each-relevant-element (e index vector start end from-end)
	  (let ((element (apply-key-function (car e) key)))
	    (when (satisfies-two-argument-test-p item element test test-not)
	      (return-from position-vector index))))))))

(defun position-aux (item sequence from-end test test-not start end key)
  ;; (declare (optimize (debug 0) (speed 3) (safety 0))
  ;; 	   (type (and fixnum (integer 0)) start)
  ;; 	   (type (or null (and fixnum (integer 0))) end))
  (unless (and (numberp start) (>= start 0))
    (error "invalid start value"))
  (unless (or (null end) (and (numberp end) (>= end 0)))
    (error "invalid end value"))
  (when (and (not (null test)) (not (null test-not)))
    (error "both test and test-not given"))
  (if (listp sequence)
      (position-list item sequence from-end test test-not start end key)
      (progn (when (null end)
	       (setf end (length sequence)))
	     (position-vector item sequence from-end test test-not start end key))))
