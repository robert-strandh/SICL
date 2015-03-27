;; For Irene's experiments.
;; reverse in an array on stack
(defun count-from-end-with-length-11 (x list length)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (declare (type fixnum length))
  (flet ((count-from-array (x list len)
	   (declare (type fixnum len) (optimize (speed 3) (compilation-speed 0)))
	   (let ((v (make-array 4096)))
	     (declare (dynamic-extent v))
	     (loop ;; reverse list to array
		for e in list
		for i from (1- len) downto 0
		do (setf (aref v i) e))
	     (loop for i from 0 below len
		when (eq x (aref v i))
		count 1))))
    (macrolet ((divide (x rest len k)
		 (let* ((n (ash 1 k))
			(gensyms (loop repeat n collect (gensym)))
			(f (gensym)))
		   `(let ((,f (ash len (- ,k)))
			  (,(car gensyms) ,rest))
		      (let* ,(loop
				for gensym1 in gensyms
				for gensym2 in (cdr gensyms)
				collect `(,gensym2 (nthcdr ,f ,gensym1)))
			(+ 
			 (traverse ,x (nthcdr ,f ,(car (last gensyms))) (- ,len (ash ,f ,k)))
			 ,@(loop
			      for gensym in (reverse gensyms)
			      collect `(traverse ,x ,gensym ,f))))))))
      (labels
	  ((traverse (x rest len)
	     (declare (type fixnum len))
	     (cond ((<= len 4096)  (count-from-array x rest len))
		   ((<= len 8192) (divide x rest len 1)) ; 2^13 divide by 2
		   ((<= len 16384) (divide x rest len 2)) ; 2^14 divide by 4
		   ((<= len 32768) (divide x rest len 3)) ; 2^15 divide by 8
		   (t                 (divide x rest len 4))))) ; divide by 16
	(traverse x list length)))))

(defun reverse-count-11 (x list)
  (count-from-end-with-length-11 x list (length list)))
