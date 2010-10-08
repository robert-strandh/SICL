;;; Generate a bitvector of the size given where the second half
;;; contains a random sequence of 0s and 1s in such a way that the
;;; change from one to the other has the probabilities given.
(defun random-bitvector (size p01 p10)
  (let ((value (random 2))
	(result (make-array size :element-type 'bit :initial-element 0)))
    (loop for i from (ash size -1) below size
	  do (setf (sbit result i) value)
	     (if (zerop value)
		 (when (< (random 1d0) p01)
		   (setf value 1))
		 (when (< (random 1d0) p10)
		   (setf value 0))))
    result))

(defun test-build-free-blocks (p01 p10)
  (let* ((size (expt 2 10))
	 (*nursery* (make-array size
				:element-type 'fixnum
				:initial-element 0))
	 (*nursery-live* (random-bitvector size p01 p10))
	 (compare (make-array size :element-type 'fixnum)))
    ;; Build the comparison vector
    (loop with free-space = 0
	  for i from 0 below (1- (length *nursery-live*))
	  do (when (zerop (sbit *nursery-live* i))
	       (incf free-space 2)
	       (setf (aref compare (* 2 i))
		     free-space)))
    ;; Compute the sizes of the free blocks.
    (compute-sizes)
    ;; Compare the two
    (loop for i from 1 to (length *nursery-live*)
	  do (when (and (zerop (sbit *nursery-live* (1- i)))
			(or (= i (length *nursery-live*))
			    (= 1 (sbit *nursery-live* i))))
	       (when (/= (aref *nursery* (* 2 (1- i)))
			 (aref compare (* 2 (1- i))))
		 (format t "differ: ~s ~s ~s~%"
			 (* 2 (1- i))
			 (aref *nursery* (* 2 (1- i)))
			 (aref compare (* 2 (1- i)))))))))

(defun time-build-free-blocks ()
  (let* ((*nursery* (make-array 1000000 :element-type 'fixnum
				:initial-element 0))
	 (*nursery-live* (make-array (1+ (/ (length *nursery*) 2))
				     :element-type 'bit)))
    ;; Initialize the bitvector to have random contents
    (loop for i from 0 below (length *nursery-live*)
	  do (setf (sbit *nursery-live* i)
		   (random 2)))
    ;; Set the sentinel
    (setf (sbit *nursery-live* (1- (length *nursery-live*))) 1)
    (time (loop repeat 100
		;; Build the linked list
		do (build-free-blocks)))))
