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
      (macrolet ((divide (rest length k)
		   (let* ((n (ash 1 k))
			  (gensyms (loop repeat n collect (gensym)))
			  (f (gensym)))
		     `(let ((,f (ash length (- ,k)))
			    (,(car gensyms) ,rest))
			(let* ,(loop
				 for gensym1 in gensyms
				 for gensym2 in (cdr gensyms)
				 collect `(,gensym2 (nthcdr ,f ,gensym1)))
			  (traverse
			   (nthcdr ,f ,(car (last gensyms)))
			   (- ,length (ash ,f ,k)))
			  ,@(loop
			      for gensym in (reverse gensyms)
			       collect `(traverse ,gensym ,f)))))))
	(labels ((traverse (rest length)
		   (declare (type fixnum length))
		   (cond ((<= length 8192) (count-from-array rest length))
			 ((<= length 16384) (divide rest length 1)) ; 2^14 divide by 2
			 ((<= length 32768) (divide rest length 2)) ; 2^15 divide by 4
			 ((<= length 65536) (divide rest length 3)) ; 2^16 divide by 8
			 (t (divide rest length 4))))) ; divide by 16
	  (traverse list length))))
    count))

(defun reverse-count-9 (x list)
  (count-from-end-with-length-9 x list (length list)))
