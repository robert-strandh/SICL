;; divide by something that depends on the length of the list does not work better than dividing by 16
(defun count-from-end-with-length-6-macro (x list length)
  (declare (optimize (speed 3) (debug 0) (safety 0) (compilation-speed 0)))
  (declare (type fixnum length))
  (let ((count 0))
    (declare (type fixnum count))
    (flet ((process (elem)
	     (when (eq elem x)
	       (incf count))))
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
	(labels ((recursive-traverse (rest length)
		   (declare (type fixnum length))
		   (when (> length 0)
		     (recursive-traverse (cdr rest) (1- length))
		     (process (car rest))))
		 (traverse (rest length)
		   (declare (type fixnum length))
		   ;;		 (print (list 'traverse (length rest) length))
		   (cond  ((<= length 8192) (recursive-traverse rest length)) ; 2^13
			  ((<= length 16384) (divide rest length 1)) ; 2^14
			  ((<= length 32768) (divide rest length 2)) ; 2^15
			  ((<= length 65536) (divide rest length 3)) ; 2^16
			  (t (divide rest length 4)))))
	  (traverse list length))))
    count))

(defun reverse-count-6-macro (x list)
  (count-from-end-with-length-6-macro x list (length list)))
