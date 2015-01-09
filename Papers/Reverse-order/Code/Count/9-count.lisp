;; For Irene's experiments.
;; reverse in an array

(defun count-from-end-with-length-9 (x list length)
  (declare (type fixnum length) (optimize (speed 3) (compilation-speed 0)))
  (let ((count 0))
    (declare (type fixnum count))
    (flet ((count-from-array (list length)
	     (declare (type fixnum length) (optimize (speed 3) (compilation-speed 0)))
	     (let ((v (make-array length)))
	       (loop 
		  for e in list
		  for i from 0 below length
		  do (setf (aref v i) e))
	       (loop for e across v
		    when (eql x e)
		    do (incf count)))))
      (labels ((traverse (rest length)
		 (declare (type fixnum length))
		 (if (<= length 8192)
		     (count-from-array rest length)
		     (let* ((f (ash length -1)))
		       (traverse (nthcdr f rest) (- length f))
		       (traverse rest f)))))
	(traverse list length)))
    count))

(defun reverse-count-9 (x list)
  (count-from-end-with-length-9 x list (length list)))
