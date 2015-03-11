(defparameter *size* (ash 1 19))

(defparameter *heap*
  (make-array *size*))

(defparameter *bitmap*
  (make-array *size* :element-type 'bit))

(defun compact (heap bitmap)
  (declare (type (simple-vector #.*size*) heap)
	   (type (array bit (#.*size*)) bitmap)
	   (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((d (position 0 bitmap))
	 (s (position 1 bitmap :start d)))
    (declare (type (integer 0 #.*size*) d s))
    (loop until (= s #.*size*)
	  do (setf (aref heap d) (aref heap s))
	     (incf d)
	     (incf s)
	     (loop until (or (= s #.*size*)
			     (= (sbit bitmap s) 1))
		   do (incf s)))))

(defun build-table (heap bitmap start)
  (let ((acc 0)
	(end (1+ start)))
    ;; (declare (type fixnum acc start end)
    ;; 	     (type (simple-vector #.*size*) heap)
    ;; 	     (type (simple-array bit (#.*size*)) bitmap)
    ;; 	     (optimize (speed 3) (safety 0) (debug 0)))
    (setf (svref heap start) 0)
    (loop for address of-type fixnum from 0 
	  for prev of-type bit = 0 then bit
	  for bit of-type bit across bitmap
	  do (if  (= bit 0)
		  (progn 
		    (when (= prev 1)
		      (setf (svref heap end) address)
		      (incf end))
		    (incf acc))
		  (when (= prev 0)
		    (setf (svref heap end) acc)
		    (incf end)))
	  finally (when (= prev 1)
		    (setf (svref heap end) address)
		    (incf end))
		  (return end))))

(defun binary-search (heap first last address)
  (declare (type (simple-vector #.*size*) heap)
           (type (integer 0 (#.*size*)) first last address)
           (optimize (speed 3) (debug 0) (safety 0)))
  (let ((f first)
        (l last))
    (declare (type (integer 0 (#.*size*)) f l))
    (loop until (= (- l f) 2)
          do (let* ((middle (1+ (ash (ash (+ l f) -2) 1)))
                    (elt (svref heap middle)))
               (declare (type fixnum elt))
               (if (< address elt)
                   (setf l middle)
                   (setf f middle))))
    (svref heap (1+ f))))

(defun worst-frontier (logN)
  (let* ((N (expt 2 logN))
         (m (loop with max = 0
                  with maxm = (- logN 2)
                  for m downfrom (- logN 2) to 2
                  do (let ((v (* m (- N (expt 2 (+ m 1))))))
                       (when (> v max)
                         (setf max v)
                         (setf maxm m)))
                  finally (return maxm))))
    (- N (* 2 (expt 2 m)) 1)))

(defun build-heap (logN)
  (let* ((heap (make-array (expt 2 logN) :initial-element 0))
         (N (expt 2 logN))
         (m (loop with max = 0
                  with maxm = (- logN 2)
                  for m downfrom (- logN 2) to 2
                  do (let ((v (* m (- N (expt 2 (+ m 1))))))
                       (when (> v max)
                         (setf max v)
                         (setf maxm m)))
                  finally (return maxm)))
         (frontier (- N (* 2 (expt 2 m)) 1)))
    (loop for i from frontier below N by 2
          do (setf (aref heap i)
                   (floor (* N (- i frontier))
                          (- N frontier))))
    (setf (aref heap (1- N)) N)
    (loop for i from 0 below frontier
          do (setf (aref heap i) (random N)))
    heap))

(defun adjust ()
  (declare (optimize (speed 3) (debug 0) (safety 0)))
  (loop with w = (worst-frontier 19)
        with l = (1- (expt 2 19))
        with g = *g*
        for i from 0 below w
        do (incf (aref *g* i)
                 (binary-search g w l i))))
