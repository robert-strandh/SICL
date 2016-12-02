;; For Irene's experiments.

(defun count-from-end-with-length-9 (x list length)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (declare (type fixnum length))
  (labels
	  ((aux1 (x list length) ;; traversal by CDR.
		 (declare (type fixnum length))
	     (if (zerop length)
			 0
			 (+ (aux1 x (cdr list) (1- length))
				(if (eq x (car list))
					1
					0)))))
	(macrolet
		((divide (x rest length k)
		   (let* ((n (ash 1 k))
				  (gensyms (loop repeat n collect (gensym)))
				  (f (gensym)))
			 `(let ((,f (ash length (- ,k)))
					(,(car gensyms) ,rest))
				(let* ,(loop
						 for gensym1 in gensyms
						 for gensym2 in (cdr gensyms)
						 collect `(,gensym2 (nthcdr ,f ,gensym1)))
				  (+ 
				   (traverse ,x (nthcdr ,f ,(car (last gensyms))) (- ,length (ash ,f ,k)))
				   ,@(loop
					   for gensym in (reverse gensyms)
					   collect `(traverse ,x ,gensym ,f))))))))
      (labels
		  ((traverse (x rest length)
			 (declare (type fixnum length))
			 (cond ((<= length 4096)  (aux1 x rest length))
				   ((<= length 8192) (divide x rest length 1)) ; 2^13 divide by 2
				   ((<= length 16384) (divide x rest length 2)) ; 2^14 divide by 4
				   ((<= length 32768) (divide x rest length 3)) ; 2^15 divide by 8
				   (t                 (divide x rest length 4))))) ; divide by 16
		(traverse x list length)))))

(defun reverse-count-9 (x list)
  (count-from-end-with-length-9 x list (length list)))


