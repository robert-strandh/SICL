(defun time-in-sec (tstart tend)
  (/ (- tend tstart) internal-time-units-per-second))

(defmacro get-time (call)
 `(let ((tstart (get-internal-run-time)))
    (values
     ,call
     (time-in-sec tstart (get-internal-run-time)))))

(defmacro get-just-time (call)
 `(let ((tstart (get-internal-run-time)))
    ,call
    (time-in-sec tstart (get-internal-run-time))))

(defmacro evaluate-time (call &optional (times 1))
  (let ((fois (gensym)))
    `(let ((,fois ,times))
       (coerce
	(/
	 (loop
	   repeat ,fois
	   sum (get-just-time ,call))
	 ,fois)
	'float))))

(defun zeros (n)
  (make-list n :initial-element 0))

(defun more-zeros (n l)
  (nconc (zeros n) l))

(defun mouline-truc-versus-machin-one-stream
    (stream reverse-count-fun1 reverse-count-fun2 start end &key (step 1) (times 1))
  (loop
    for k from start to end by step
    for list = (zeros start) then (more-zeros step list)
    do (let ((truc-time (evaluate-time (funcall reverse-count-fun1 0 list) times))
	     (machin-time (evaluate-time (funcall reverse-count-fun2 0 list) times)))
;;	 (format t "~3D ~10,5F ~10,5F~%" k truc-time machin-time)
	 (finish-output t)
	 (format stream "~3D ~10,5F ~10,5F~%" k truc-time machin-time))))

(defun mouline-truc-versus-machin-one-file
    (file reverse-count-fun1 reverse-count-fun2 start end &key (step 1) (times 1))
  (with-open-file (stream file :direction :output :if-does-not-exist :create :if-exists :supersede)
      (mouline-truc-versus-machin-one-stream
       stream
       reverse-count-fun1 reverse-count-fun2 
       start end :step step :times times)))

(defun count-from-end-with-length-7 (x list length)
  (declare (type fixnum length))
  (declare (optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))
  (labels (;; AUX1 is the recursive traversal by CDR.
	   (aux1 (x list length)
	     (declare (type fixnum length))
	     (if (zerop length)
		 0
		 (+ (aux1 x (cdr list) (1- length))
		    (if (eq x (car list))
			1
			0))))
	   ;; AUX2 is the recursive traversal by (NTHCDR 10000 ...).
	   ;; It is used when the length of the list is less than
	   ;; 100000000.
	   (aux2 (x list length)
	     (declare (type fixnum length))
	     (if (<= length 10000)
		 (aux1 x list length)
		 (+ (aux2 x (nthcdr 10000 list) (- length 10000))
		    (aux1 x list 10000))))
	   ;; AUX3 is the recursive traversal by half the size of the
	   ;; list.  It is used for lists that have more than
	   ;; 100000000 elements.
	   (aux3 (x list length)
	     (declare (type fixnum length))
	     (if (< length 100000000)
		 (aux2 x list length)
		 (let* ((n (ash length -1))
			(middle (nthcdr n list)))
		   (+ (aux3 x middle (- length n))
		      (aux3 x list n))))))
    (aux3 x list length)))

(defun reverse-count-7 (x list)
  (count-from-end-with-length-7 x list (length list)))

(defun count-from-end-with-length-9 (x list length)
  (declare (type fixnum length) (optimize (speed 3) (compilation-speed 0)))
  (flet ((count-from-array (x list length)
	   (declare (type fixnum length) (optimize (speed 3) (compilation-speed 0)))
	   (let ((v (make-array length)))
	     (loop ;; reverse list to array
	       for e in list
	       for i from (1- length) downto 0
	       do (setf (aref v i) e))
	     (loop for e across v
		   when (eq x e)
		     count e))))
    (macrolet ((divide (x rest length k)
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
	     (cond ((<= length 4096)  (count-from-array x rest length))
		   ((<= length 8192) (divide x rest length 1)) ; 2^13 divide by 2
		   ((<= length 16384) (divide x rest length 2)) ; 2^14 divide by 4
		   ((<= length 32768) (divide x rest length 3)) ; 2^15 divide by 8
		   (t                 (divide x rest length 4))))) ; divide by 16
	(traverse x list length)))))

(defun reverse-count-9 (x list)
  (count-from-end-with-length-9 x list (length list)))

(defun test ()
   (mouline-truc-versus-machin-one-file
    "v7-vs-v9" 
    #'reverse-count-7
    #'reverse-count-9
    100000
    100000000
    :step 100000))
